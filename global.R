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
  mutate(Val = mean(as.numeric(Value), na.rm = T))


