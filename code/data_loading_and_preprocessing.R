# Function to load and preprocess shapefile
load_and_preprocess_shapefile <- function() {
  tryCatch(
    {
      kenya_counties_sf <- st_read(dsn = "data/shapefile", layer = "County") %>%
        select(COUNTY, geometry) %>%
        mutate(
          county_name = case_when(
            COUNTY == "Tharaka" ~ "Tharaka-Nithi",
            COUNTY == "Keiyo-Marakwet" ~ "Elgeyo Marakwet",
            COUNTY == "Trans Nzoia" ~ "Trans-Nzoia",
            TRUE ~ COUNTY
          )
        ) %>%
        st_sf()  # Ensure sf object
      
      return(kenya_counties_sf)
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
      filtered_province_data <- read_csv("data/dhs-quickstats_subnational_ken.csv") %>%
        # Select desired indicators
        filter(IndicatorId %in% c("FE_FRTR_W_TFR", "CH_VACC_C_BAS", "CM_ECMR_C_IMR", "CM_ECMR_C_U5M", "CN_BFDR_C_MDE")) %>%
        
        # Join province data
        right_join(read_csv("data/province.csv"), by = c("Location" = "Region")) %>%
        
        # Tidy the join column
        mutate(province_name = factor(Province, levels = unique(Province))) %>%
        
        # Group and calculate mean, preserving "Indicator"
        group_by(province_name, indicator_id = IndicatorId, survey_year = SurveyYear) %>%
        summarise(mean_value = round(mean(as.numeric(Value), na.rm = TRUE), 2),
                  Indicator = first(Indicator),
                  county_name = list(unique(Location))) %>%
        
        # Ungroup and create indicator labels
        ungroup() %>%
        mutate(indicator_label = Indicator) %>%  # Use the preserved "Indicator"
        
        select(county_name, province_name, indicator_id, indicator_label, survey_year, mean_value)
      
      return(filtered_province_data)
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
      filtered_province_data <- load_and_filter_survey_data()
      
      # Unnest county_name lists to match individual counties
      filtered_province_data <- filtered_province_data %>%
        unnest(county_name)
      
      # Perform the left_join (using the existing county_name column)
      kenya_merged_data <- kenya_counties_sf %>%
        left_join(filtered_province_data, by = "county_name")
      
      return(kenya_merged_data)
    },
    error = function(e) {
      message("An error occurred during data merging: ", e$message)
    }
  )
}

