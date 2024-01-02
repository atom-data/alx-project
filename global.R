###--- 1. Load libraries and set working directory ---###

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, sf, leaflet)  # Load libraries efficiently

setwd("alx-project")  # Set working directory

###--- 2. Load and preprocess data ---###

# Load and preprocess shapefile with error handling
load_shapefile <- function() {
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

# Load and filter survey data with error handling
load_survey_data <- function() {
  tryCatch(
    {
      dhs_survey_data <- read_csv("dhs-quickstats_subnational_ken.csv") %>%
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

###--- 3. Join data ---###

merge_data <- function() {
  tryCatch(
    {
      kenya_counties_sf <- load_shapefile()
      dhs_survey_data <- load_survey_data()
      
      kenya_merged_data <- kenya_counties_sf %>%
        left_join(dhs_survey_data, by = c("county_name" = "Location"))
      
      return(kenya_merged_data)
    },
    error = function(e) {
      message("An error occurred during data merging: ", e$message)
    }
  )
}

##--- 4. Visualization functions (unchanged) ---###

# Create basemap function
create_basemap <- function(kenya_counties_sf) {
  tryCatch(
    {
      leaflet(options = leafletOptions(minZoom = 5)) %>%
        setMaxBounds(33.911819, -4.702271, 41.906258, 5.430648) %>%
        setView(lng = 37.818, lat = 0.606, zoom = 7) %>%
        addTiles() %>%
        addPolygons(
          data = kenya_counties_sf,
          label = ~county_name,
          weight = 3,
          color = "black",
          fillColor = "white",
          fillOpacity = 0.0
        )
    },
    error = function(e) {
      stop("Error creating basemap: ", e$message)
    }
  )
}


# Function to filter data for a given variable and year
merge_data_for_variable_year <- function(data, variable, year) {
  data %>%
    filter(indicator_id == variable, survey_year == year)
}




