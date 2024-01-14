# Load required packages
pacman::p_load(shiny, shinydashboard, plotly, leaflet)

# Load functions from global.R file
source("app/global.R")

# Define the dashboard header
# Define the dashboard header
header <- dashboardHeader(
  title = "Kenya Watch",
  titleWidth = 300  # Adjust as needed for logo
)

# Define the dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarItem",
              menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
              menuItem("Data Exploration", tabName = "data_exploration", icon = icon("table"))
  ),
  
  # Selection panel for Overview tab
  conditionalPanel(
    condition = "input.sidebarItem == 'overview'",
    selectInput("region", "Region:", choices = c("Province", "County")),
    conditionalPanel(
      condition = "input.region == 'Province'",
      selectInput("province", "Province:", choices = c("All", sort(unique(as.character(merged_data$province_name)))))
    ),
    conditionalPanel(
      condition = "input.region == 'County'",
      selectInput("county", "County:", choices = c("All", sort(unique(merged_data$county_name))))
    ),
    selectInput("variable", "Variable:", choices = c("All", unique(merged_data$indicator_label))),
    sliderInput("year", "Year:", min = min(as.numeric(merged_data$survey_year)), max = max(as.numeric(merged_data$survey_year)), value = max(as.numeric(merged_data$survey_year)), sep = ""),
    radioButtons("plot_type", "Plot Type:", choices = c("Time Series", "Histogram", "Scatter")),
    conditionalPanel(
      condition = "input.plot_type == 'Scatter'",
      selectInput("scatter_var", "Additional Variable:", choices = c("All", sort(unique(merged_data$indicator_label))))
    )
  ),
  
  # Filtering options for Data Exploration tab
  conditionalPanel(
    condition = "input.sidebarItem == 'data_exploration'",
    selectInput("year_filter", "Year:", choices = c("All", sort(unique(merged_data$survey_year)))),
    selectInput("county_filter", "County:", choices = c("All", sort(unique(merged_data$county_name)))),
    selectInput("province_filter", "Province:", choices = c("All", sort(unique(as.character(merged_data$province_name))))),
    selectInput("indicator_filter", "Indicator:", choices = c("All", sort(unique(merged_data$indicator_label))))
  )
)

# Define the dashboard body
body <- dashboardBody(
  tags$head(
    includeCSS("app/www/custom-styles.css")
  ),
  tabItems(
    tabItem(
      tabName = "overview",
      fluidRow(
        # Leaflet map takes the full width of the row
        column(width = 12,
               box(
                 width = NULL,
                 leafletOutput("map", height = 500),
                 title = "Kenya Map",
                 status = "primary"
               )
        ),
        # Graphical plot at the bottom, full width
        column(width = 12,
               div(
                 class = "card",  # Add the card class for visual styling
                 box(
                   width = NULL,
                   plotlyOutput("plot", height = 500),
                   title = "Graphical Plot",
                   status = "primary"
                 )
               )
        )
      )
    ),
    tabItem(
      tabName = "data_exploration",
      fluidRow(
        column(width = 12, # Use the full width of the row
               box(
                 title = "Data Table",
                 width = NULL,
                 status = "primary",
                 DT::dataTableOutput("data_table")
               ),
               box(
                 title = "Download Data",
                 width = NULL,
                 status = "success",
                 downloadButton("download_data", "Download as CSV")
               )
        )
      )
    )
  )
)


ui <- dashboardPage(header = header, sidebar = sidebar, body = body)