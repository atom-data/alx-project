###---Load the required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, sf,
               leaflet, ggplot2, tidyr,
               htmltools, plotly)  # Load libraries

# Data loading and preprocessing
source("code/data_loading_and_preprocessing.R")  # Load functions from a separate file

# Visualization functions
source("code/visualization_functions.R")  # Load functions from a separate file
