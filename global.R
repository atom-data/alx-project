###--- Loading and pre-processing of data---###
###--Set the working directory
setwd("alx-project")

###---Load the required packages
pacman::p_load(readr, dplyr, sf, leaflet)

###---Load the required dataset
dhs <- read_csv("dhs-quickstats_subnational_ken.csv")
provinces <- read_csv("province.csv")
kenya_shp <- st_read(dsn = getwd(), layer = "County")

###--- Clean up the county names of kenya_shp
replace_fun <- function(original, new){
  return(kenya_shp %>% mutate(COUNTY = replace(COUNTY,
                                               COUNTY == original,
                                               new )))
}

kenya_shp <- replace_fun("Tharaka", "Tharaka-Nithi")
kenya_shp <- replace_fun("Keiyo-Marakwet", "Elgeyo Marakwet")
kenya_shp <- replace_fun("Trans Nzoia", "Trans-Nzoia")

###--- IndicatorId of interest
indicatorId <- c("RH_DELP_C_DHF", "FE_FRTR_W_TFR", "CH_DIAT_C_ORT",
                 "ED_EDUC_W_SEH", "CM_ECMR_C_U5M")

dhs_select <- dhs %>% right_join(provinces,
                         by = c("Location" = "Region")) %>%
  filter(IndicatorId %in% indicatorId) %>%
  group_by(Province, IndicatorId, SurveyYear) %>%
  mutate(Val = mean(as.numeric(Value), na.rm = T)) %>%
  ungroup()

###---Join the two data files to be one
kenya_province <- kenya_shp %>%
  left_join(provinces, by = c("COUNTY" = "Region")) %>%
  group_by(Province)

###---create a basemap for the data
basemap <- leaflet(options = leafletOptions(minZoom = 5)) %>%
  setMaxBounds(33.911819, -4.702271, 41.906258, 5.430648) %>%
  setView(lng=37.818, lat=0.606, zoom=7) %>%
  addTiles() %>%
  addPolygons(
    data = kenya_shp,
    label = ~COUNTY,
    weight = 3,
    color = "black",
    fillColor = "white",
    fillOpacity = 0.0
  )

###--- Define a function to merge the required variable with the kenyan shapefile
kenya_dhs <- function(data = kenya_province, variable, year) {
  dhs_select_filter <- dhs_select %>%
    filter(Indicator == variable) %>%
    filter(SurveyYear == year)
  data <- data %>%
    left_join(dhs_select_filter,
                             by = "Province")
  return(data)
}
