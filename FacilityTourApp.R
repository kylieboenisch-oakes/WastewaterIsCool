library(shiny)
library(leaflet)
library(geosphere)
library(dplyr)
library(DT)
library(jsonlite)

# =====================================================================
# 1. ZERO-DEPENDENCY OSM GEOCODER (For User City/State Input)
# =====================================================================
geocode_osm <- function(address_string) {
  safe_query <- URLencode(address_string)
  osm_url <- paste0("https://nominatim.openstreetmap.org/search?q=", safe_query, "&format=json&limit=1")
  
  tryCatch({
    req <- url(osm_url, headers = c("User-Agent" = "R-National-Water-Locator-App"))
    res <- jsonlite::fromJSON(req)
    if (length(res) == 0 || nrow(res) == 0) return(c(lat = NA_real_, lng = NA_real_))
    return(c(lat = as.numeric(res$lat[1]), lng = as.numeric(res$lon[1])))
  }, error = function(e) {
    message("Geocoding failed: ", e$message)
    return(c(lat = NA_real_, lng = NA_real_))
  })
}

# =====================================================================
# 2. RESILIENT API WRAPPER (Catches 429 Rate Limits & Auto-Retries!)
# =====================================================================
safe_fetch_json <- function(url, max_retries = 2, pause_seconds = 2) {
  for (attempt in 1:max_retries) {
    res <- tryCatch({
      jsonlite::fromJSON(url)
    }, error = function(e) {
      if (grepl("429|500|502|503", e$message) && attempt < max_retries) {
        message("[API Throttled] Server busy. Waiting ", pause_seconds, "s before retry...")
        Sys.sleep(pause_seconds)
        return(NULL)
      }
      stop(e)
    })
    if (!is.null(res)) return(res)
  }
  return(list())
}

# =====================================================================
# 3A. HELPER FUNCTION: CLEAN WATER ACT (CWA) - WASTEWATER PLANTS
# =====================================================================
fetch_cwa_wastewater <- function(state_code) {
  message("--------------------------------------------------")
  message("[CWA Engine] Fetching Major Wastewater Facilities for: ", toupper(state_code))
  
  url_a <- paste0("https://echodata.epa.gov/echo/cwa_rest_services.get_facility_info?output=JSON&p_act=Y&p_ptype=NPD&p_maj=Y&p_st=", toupper(state_code))
  
  tryCatch({
    res_a <- safe_fetch_json(url_a)
    df_a  <- res_a$Results$Facilities
    if (is.null(df_a) || length(df_a) == 0 || !is.data.frame(df_a)) return(data.frame())
    
    id_col_a   <- names(df_a)[grepl("SourceID|RegistryID|CWP_PERMIT_NUMBER|permit|id$", names(df_a), ignore.case = TRUE)][1]
    name_col_a <- names(df_a)[grepl("name|facility", names(df_a), ignore.case = TRUE)][1]
    lat_col_a  <- names(df_a)[grepl("FacLat|^lat$|latitude", names(df_a), ignore.case = TRUE)][1]
    
    table_a_lat <- data.frame(
      join_id   = trimws(toupper(as.character(df_a[[id_col_a]]))),
      join_name = trimws(toupper(as.character(df_a[[name_col_a]]))),
      lat       = as.numeric(df_a[[lat_col_a]]),
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(lat) & lat != 0) %>% arrange(join_name)
    
    Sys.sleep(0.5)
    
    url_token <- paste0("https://echodata.epa.gov/echo/cwa_rest_services.get_facilities?output=JSON&p_act=Y&p_ptype=NPD&p_maj=Y&p_st=", toupper(state_code))
    res_token <- safe_fetch_json(url_token)
    qid       <- res_token$Results$QueryID
    if (is.null(qid) || is.na(qid) || qid == "") return(data.frame())
    
    Sys.sleep(0.5)
    
    url_b <- paste0("https://echodata.epa.gov/echo/cwa_rest_services.get_download?qid=", qid)
    df_b <- tryCatch({ read.csv(url_b, stringsAsFactors = FALSE, check.names = FALSE) }, error = function(e) data.frame())
    if (is.null(df_b) || nrow(df_b) == 0) return(data.frame())
    
    valid_cols_b <- names(df_b)[!grepl("flg|flag|code|type|desc|status|date|waiv|pop|dens|acs|pct", names(df_b), ignore.case = TRUE)]
    id_col_b     <- names(df_b)[grepl("SourceID|RegistryID|CWP_PERMIT_NUMBER|permit|id$", names(df_b), ignore.case = TRUE)][1]
    name_col_b   <- names(df_b)[grepl("name|facility", names(df_b), ignore.case = TRUE)][1]
    lng_col_b    <- valid_cols_b[grepl("FacLong|^lon$|^lng$|faclog|faclg|longitude", valid_cols_b, ignore.case = TRUE)][1]
    street_col   <- names(df_b)[grepl("street|addr", names(df_b), ignore.case = TRUE)][1]
    city_col     <- names(df_b)[grepl("city", names(df_b), ignore.case = TRUE)][1]
    state_col    <- names(df_b)[grepl("state|^st$", names(df_b), ignore.case = TRUE)][1]
    zip_col      <- names(df_b)[grepl("zip", names(df_b), ignore.case = TRUE)][1]
    
    table_b_lng <- data.frame(
      join_id   = trimws(toupper(as.character(df_b[[id_col_b]]))),
      join_name = trimws(toupper(as.character(df_b[[name_col_b]]))),
      permit_id = as.character(df_b[[id_col_b]]),
      name      = if (!is.na(name_col_b)) as.character(df_b[[name_col_b]]) else rep("Unknown Facility", nrow(df_b)),
      street    = if (!is.na(street_col)) as.character(df_b[[street_col]]) else rep("", nrow(df_b)),
      city      = if (!is.na(city_col)) as.character(df_b[[city_col]]) else rep("", nrow(df_b)),
      state     = if (!is.na(state_col)) as.character(df_b[[state_col]]) else rep(state_code, nrow(df_b)),
      zip       = if (!is.na(zip_col)) as.character(df_b[[zip_col]]) else rep("", nrow(df_b)),
      lng       = as.numeric(df_b[[lng_col_b]]),
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(lng) & lng != 0) %>% mutate(lng = -1 * abs(lng)) %>% arrange(join_name)
    
    merged_df <- inner_join(table_b_lng, table_a_lat, by = "join_id", suffix = c("", "_a"), relationship = "many-to-many")
    if (nrow(merged_df) == 0) merged_df <- inner_join(table_b_lng, table_a_lat, by = "join_name", suffix = c("", "_a"), relationship = "many-to-many")
    if (nrow(merged_df) == 0 && nrow(table_b_lng) == nrow(table_a_lat)) merged_df <- bind_cols(table_b_lng, table_a_lat %>% select(lat))
    
    merged_df <- merged_df %>% 
      select(-any_of(c("join_id", "join_name", "join_name_a", "join_id_a"))) %>%
      mutate(facility_type = "Wastewater Treatment (CWA)")
    
    message("[CWA Engine] Successfully loaded ", nrow(merged_df), " wastewater facilities.")
    return(merged_df)
  }, error = function(e) { message("CWA Query Failed: ", e$message); return(data.frame()) })
}

# =====================================================================
# 3B. HELPER FUNCTION: SAFE DRINKING WATER ACT (SDWA) VIA MASTER REGISTRY
# =====================================================================
fetch_sdwa_drinking_water <- function(state_code) {
  message("--------------------------------------------------")
  message("[SDWA Engine] Fetching Municipal Drinking Water Facilities for: ", toupper(state_code))
  
  Sys.sleep(1) # Polite pause
  
  url_a <- paste0("https://echodata.epa.gov/echo/echo_rest_services.get_facility_info?output=JSON&p_act=Y&p_med=S&p_st=", toupper(state_code))
  
  tryCatch({
    res_a <- safe_fetch_json(url_a)
    df_a  <- res_a$Results$Facilities
    if (is.null(df_a) || length(df_a) == 0 || !is.data.frame(df_a)) return(data.frame())
    
    id_col_a   <- names(df_a)[grepl("RegistryID|SourceID|permit|id$", names(df_a), ignore.case = TRUE)][1]
    name_col_a <- names(df_a)[grepl("FacName|name|facility", names(df_a), ignore.case = TRUE)][1]
    lat_col_a  <- names(df_a)[grepl("FacLat|^lat$|latitude", names(df_a), ignore.case = TRUE)][1]
    
    table_a_lat <- data.frame(
      join_id   = trimws(toupper(as.character(df_a[[id_col_a]]))),
      join_name = trimws(toupper(as.character(df_a[[name_col_a]]))),
      lat       = as.numeric(df_a[[lat_col_a]]),
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(lat) & lat != 0) %>% arrange(join_name)
    
    Sys.sleep(0.5)
    
    url_token <- paste0("https://echodata.epa.gov/echo/echo_rest_services.get_facilities?output=JSON&p_act=Y&p_med=S&p_st=", toupper(state_code))
    res_token <- safe_fetch_json(url_token)
    qid       <- res_token$Results$QueryID
    if (is.null(qid) || is.na(qid) || qid == "") return(data.frame())
    
    Sys.sleep(0.5)
    
    url_b <- paste0("https://echodata.epa.gov/echo/echo_rest_services.get_download?qid=", qid)
    df_b <- tryCatch({ read.csv(url_b, stringsAsFactors = FALSE, check.names = FALSE) }, error = function(e) data.frame())
    if (is.null(df_b) || nrow(df_b) == 0) return(data.frame())
    
    valid_cols_b <- names(df_b)[!grepl("flg|flag|code|type|desc|status|date|waiv|pop|dens|acs|pct", names(df_b), ignore.case = TRUE)]
    id_col_b     <- names(df_b)[grepl("RegistryID|SourceID|permit|id$", names(df_b), ignore.case = TRUE)][1]
    name_col_b   <- names(df_b)[grepl("FacName|name|facility", names(df_b), ignore.case = TRUE)][1]
    lng_col_b    <- valid_cols_b[grepl("FacLong|^lon$|^lng$|faclog|faclg|longitude", valid_cols_b, ignore.case = TRUE)][1]
    street_col   <- names(df_b)[grepl("street|addr", names(df_b), ignore.case = TRUE)][1]
    city_col     <- names(df_b)[grepl("city", names(df_b), ignore.case = TRUE)][1]
    state_col    <- names(df_b)[grepl("state|^st$", names(df_b), ignore.case = TRUE)][1]
    zip_col      <- names(df_b)[grepl("zip", names(df_b), ignore.case = TRUE)][1]
    
    table_b_lng <- data.frame(
      join_id   = trimws(toupper(as.character(df_b[[id_col_b]]))),
      join_name = trimws(toupper(as.character(df_b[[name_col_b]]))),
      permit_id = as.character(df_b[[id_col_b]]),
      name      = if (!is.na(name_col_b)) as.character(df_b[[name_col_b]]) else rep("Unknown Water System", nrow(df_b)),
      street    = if (!is.na(street_col)) as.character(df_b[[street_col]]) else rep("", nrow(df_b)),
      city      = if (!is.na(city_col)) as.character(df_b[[city_col]]) else rep("", nrow(df_b)),
      state     = if (!is.na(state_col)) as.character(df_b[[state_col]]) else rep(state_code, nrow(df_b)),
      zip       = if (!is.na(zip_col)) as.character(df_b[[zip_col]]) else rep("", nrow(df_b)),
      lng       = as.numeric(df_b[[lng_col_b]]),
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(lng) & lng != 0) %>% mutate(lng = -1 * abs(lng)) %>% arrange(join_name)
    
    merged_df <- inner_join(table_b_lng, table_a_lat, by = "join_id", suffix = c("", "_a"), relationship = "many-to-many")
    if (nrow(merged_df) == 0) merged_df <- inner_join(table_b_lng, table_a_lat, by = "join_name", suffix = c("", "_a"), relationship = "many-to-many")
    if (nrow(merged_df) == 0 && nrow(table_b_lng) == nrow(table_a_lat)) merged_df <- bind_cols(table_b_lng, table_a_lat %>% select(lat))
    
    # CRITICAL WORKAROUND: THE PRECISION MUNICIPAL WHITELIST
    # Only keeps plants whose names contain true public utility or municipal keywords.
    # Automatically blocks schools, HOA pumps, private estates, and un-tourable private systems!
    merged_df <- merged_df %>% 
      select(-any_of(c("join_id", "join_name", "join_name_a", "join_id_a"))) %>%
      filter(grepl("WATER|TREATMENT|PLANT|UTILITY|UTILITIES|CITY OF|TOWN OF|BOARD|DISTRICT|AUTHORITY", name, ignore.case = TRUE)) %>%
      # Ensure wastewater plants don't sneak into this drinking water table
      filter(!grepl("WASTEWATER|WWTP|WRP|SEWAGE|RECLAMATION|EFFLUENT|OUTFALL|SEWER", name, ignore.case = TRUE)) %>%
      mutate(facility_type = "Drinking Water Treatment (SDWA)")
    
    message("[SDWA Engine] Successfully loaded ", nrow(merged_df), " verified municipal drinking water facilities.")
    return(merged_df)
  }, error = function(e) { message("SDWA Query Failed: ", e$message); return(data.frame()) })
}

# =====================================================================
# 4. USER INTERFACE (UI)
# =====================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .btn-primary-custom { background-color: #005a9c; color: white; font-weight: bold; }
      .btn-primary-custom:hover { background-color: #003d6b; color: white; }
      .tour-btn { background-color: #2e6da4; color: #ffffff !important; font-weight: 500; text-decoration: none; padding: 4px 8px; border-radius: 4px; display: inline-block; }
      .tour-btn:hover { background-color: #1b4f72; }
    "))
  ),
  
  titlePanel("National Water & Wastewater Treatment Tour Locator"),
  
  sidebarLayout(
    sidebarPanel(
      p("Search anywhere in the US. The app automatically extracts your state and queries the EPA's active environmental registries."),
      
      textInput("user_loc", "US City and State (e.g., Portland, OR):", value = "Portland, OR"),
      
      radioButtons("facility_type", "Select Facility Type:",
                   choices = c("Both (Wastewater & Drinking Water)" = "BOTH",
                               "Wastewater Treatment Only (CWA)" = "CWA",
                               "Drinking Water Treatment Only (SDWA)" = "SDWA"),
                   selected = "BOTH"),
      
      sliderInput("radius", "Search Radius (miles):", 
                  min = 5, max = 150, value = 25, step = 5),
      
      actionButton("search_btn", "Search National Database", class = "btn btn-primary-custom", style = "width: 100%;"),
      
      hr(),
      HTML("<small><b>Architecture Note:</b> Uses a positive municipal-only whitelist to ensure only real, tourable utility and treatment plant infrastructure is rendered.</small>")
    ),
    
    mainPanel(
      leafletOutput("map", height = "450px"),
      br(),
      h4("Detected Facilities & Educational Outreach"),
      DTOutput("facility_table")
    )
  )
)

# =====================================================================
# 5. SERVER LOGIC
# =====================================================================
server <- function(input, output, session) {
  
  national_data <- eventReactive(input$search_btn, {
    req(input$user_loc)
    
    showModal(modalDialog("Geocoding location and stitching EPA water & wastewater streams...", footer = NULL))
    on.exit(removeModal())
    
    coords <- geocode_osm(input$user_loc)
    
    if (is.na(coords["lat"])) {
      showNotification("Could not locate that address. Please make sure to include a State abbreviation!", type = "error")
      return(NULL)
    }
    
    u_lat <- coords["lat"]
    u_lng <- coords["lng"]
    
    state_match <- regmatches(toupper(input$user_loc), regexpr("\\b[A-Z]{2}\\b", toupper(input$user_loc)))
    state_code <- if (length(state_match) > 0) state_match[1] else "OR" 
    
    results_list <- list()
    
    if (input$facility_type %in% c("BOTH", "CWA")) {
      cwa_data <- fetch_cwa_wastewater(state_code)
      if (nrow(cwa_data) > 0) results_list$cwa <- cwa_data
    }
    
    if (input$facility_type %in% c("BOTH", "SDWA")) {
      sdwa_data <- fetch_sdwa_drinking_water(state_code)
      if (nrow(sdwa_data) > 0) results_list$sdwa <- sdwa_data
    }
    
    api_results <- bind_rows(results_list) %>%
      distinct(permit_id, .keep_all = TRUE)
    
    if (nrow(api_results) == 0) {
      showNotification("Could not retrieve facilities for state: ", state_code, type = "warning")
      return(list(user_lat = u_lat, user_lng = u_lng, facilities = data.frame()))
    }
    
    processed_df <- api_results %>%
      rowwise() %>%
      mutate(
        distance_miles = tryCatch({
          round(distHaversine(c(u_lng, u_lat), c(lng, lat)) * 0.000621371, 1)
        }, error = function(e) 9999)
      ) %>%
      ungroup() %>%
      arrange(distance_miles)
    
    message("User Geocoded Coords: Lat ", round(u_lat, 4), " | Lng ", round(u_lng, 4))
    if (nrow(processed_df) > 0) {
      message("Closest facility found in ", state_code, ": ", processed_df$name[1], 
              " (", processed_df$distance_miles[1], " miles away - ", processed_df$facility_type[1], ")")
    }
    message("--------------------------------------------------")
    
    filtered_df <- processed_df %>%
      filter(distance_miles <= input$radius) %>%
      rowwise() %>% 
      mutate(
        clean_name = gsub("CITY OF |TOWN OF |VILLAGE OF | LLC| INC| CORP| CBWTP| WWTP| WRP| STP", "", name, ignore.case = TRUE),
        tour_keyword = ifelse(grepl("SDWA", facility_type), 
                              "drinking water treatment plant public tour OR water facility visit", 
                              "wastewater treatment plant public tour OR education center visit"),
        search_query = paste(clean_name, city, state, tour_keyword),
        Tour_Link = paste0(
          "<a href='https://www.google.com/search?q=", 
          URLencode(search_query), 
          "' target='_blank' rel='noopener noreferrer' class='tour-btn'>Find Tour Page ↗</a>"
        )
      ) %>%
      ungroup()
    
    list(user_lat = u_lat, user_lng = u_lng, facilities = filtered_df)
  }, ignoreNULL = FALSE)
  
  # --- RENDER MAP ---
  output$map <- renderLeaflet({
    data <- national_data()
    req(data)
    
    m <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = data$user_lng, lat = data$user_lat,
        icon = makeAwesomeIcon(icon = "home", markerColor = "red", library = "fa"),
        popup = paste("<b>Search Center:</b>", input$user_loc)
      )
    
    if (nrow(data$facilities) > 0) {
      icons <- awesomeIcons(
        icon = "tint",
        iconColor = "#ffffff",
        library = "fa",
        markerColor = ~ifelse(grepl("SDWA", facility_type), "blue", "orange")
      )
      
      m <- m %>% addAwesomeMarkers(
        data = data$facilities,
        lng = ~lng, lat = ~lat,
        icon = icons,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          "<b>Type:</b> <span style='color:", ifelse(grepl("SDWA", facility_type), "#005a9c;", "#d35400;"), "font-weight:bold;'>", facility_type, "</span><br>",
          "<b>Address:</b> ", ifelse(street != "", paste0(street, ", ", city), city), ", ", state, " ", zip, "<br>",
          "<b>EPA Permit ID:</b> ", permit_id, "<br>",
          "<b>Distance:</b> ", distance_miles, " miles<br><br>",
          Tour_Link
        )
      )
    }
    m
  })
  
  # --- RENDER TABLE ---
  output$facility_table <- renderDT({
    data <- national_data()
    req(data)
    
    if (nrow(data$facilities) == 0) {
      return(datatable(data.frame(Notice = "No active water or wastewater treatment plants found within this radius. Try increasing your radius slider!")))
    }
    
    display_df <- data$facilities %>%
      select(
        `Facility Name` = name,
        `Facility Type` = facility_type,
        `Street Address` = street,
        `City` = city,
        `State` = state,
        `Zip` = zip,
        `Distance (mi)` = distance_miles,
        `EPA ID` = permit_id,
        `Tour Search` = Tour_Link
      )
    
    datatable(
      display_df, 
      escape = FALSE, 
      rownames = FALSE, 
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

# Run the App
shinyApp(ui = ui, server = server)
