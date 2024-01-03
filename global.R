if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, sf, leaflet)  # Load libraries efficiently

setwd("alx-project")  # Set working directory

# Function to load and preprocess shapefile
load_and_preprocess_shapefile <- function() {
  tryCatch(
    {
      kenya_counties_sf <- st_read(dsn = getwd(), layer = "County") %>%
        mutate(
          county_name = case_when(
            COUNTY == "Tharaka" ~ "Tharaka-Nithi",
            COUNTY == "Keiyo-Marakwet" ~ "Elgeyo Marakwet",
            COUNTY == "Trans Nzoia" ~ "Trans-Nzoia",
            TRUE ~ COUNTY
          )
        )
    },
    error = function(e) {
      stop("Error loading shapefile: ", e$message)
    }
  )
}

# Function to load and filter survey data
load_and_filter_survey_data <- function() {
  tryCatch(
    {
      filtered_survey_data <- read_csv("dhs-quickstats_subnational_ken.csv") %>%
        filter(IndicatorId %in% c("RH_DELP_C_DHF", "FE_FRTR_W_TFR", "CH_DIAT_C_ORT", "ED_EDUC_W_SEH", "CM_ECMR_C_U5M")) %>%
        right_join(read_csv("province.csv"), by = c("Location" = "Region")) %>%  # Read province data as CSV
        group_by(province_name = Province, indicator_id = IndicatorId, survey_year = SurveyYear) %>%
        mutate(mean_value = mean(as.numeric(Value), na.rm = TRUE)) %>%
        ungroup()
    },
    error = function(e) {
      stop("Error loading survey data: ", e$message)
    }
  )
}

# Function to merge data
merge_data <- function() {
  tryCatch(
    {
      kenya_counties_sf <- load_and_preprocess_shapefile()
      filtered_survey_data <- load_and_filter_survey_data()
      
      # Validate data before merging (consider adding checks here)
      
      kenya_merged_data <- kenya_counties_sf %>%
        left_join(filtered_survey_data, by = c("county_name" = "Location"))
      
      return(kenya_merged_data)
    },
    error = function(e) {
      message("An error occurred during data merging: ", e$message)
    }
  )
}

create_basemap <- function(data) {
  tryCatch({
    leaflet(options = leafletOptions(
      minZoom = 5,
      zoomControl = FALSE  # Remove default zoom buttons for custom control
    )) %>%
      setMaxBounds(33.911819, -4.702271, 41.906258, 5.430648) %>%
      setView(lng = 37.818, lat = 0.606, zoom = 7) %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addPolygons(
        data = data,
        label = ~county_name,
        weight = 2,
        color = "black",
        fillColor = "lightblue",
        fillOpacity = 0.25,
        group = ~province_name,  # Group counties by province_name
        highlight = highlightOptions(
          weight = 3,
          color = "black",
          bringToFront = TRUE,
          group = ~province_name  # Highlight entire province group
        )
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri", "Carto", "Terrain", "Toner"),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomright"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Zoom to Level 7",
        onClick = JS("function(btn, map){ map.setZoom(7); }"))) %>%  # Custom zoom control
      addEasyButton(easyButton(
        icon = "fa-arrows-alt", title = "Reset View",
        onClick = JS("function(btn, map){ map.fitBounds(map.getBounds()); }")))  # Reset view button
  },
  error = function(e) {
    stop("Error creating basemap: ", e$message)
  })
}

# Function to filter data for a given variable and year
filter_data_for_variable_year <- function(data, variable, year) {
  data %>%
    filter(indicator_id == variable, survey_year == year)
}

          