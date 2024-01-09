# Load required packages
pacman::p_load(shiny, shinydashboard, DT)

# Load functions from global.R file
source("app/global.R")

# Define the dashboard header
header <- dashboardHeader(
  title = "Kenya Watch",
  titleWidth = 350,
  logo = tags$img(src = "logo.png", height = "50px", align = "left")
)

# Define the dashboard siderbar
sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
    menuItem("Data Exploration", tabName = "data_exploration", icon = icon("table"))
  )
)

# Define the dashboard body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overview",
      fluidRow(
        column(width = 6,
               box(
                 width = NULL,
                 leafletOutput("map", height = 500),
                 title = "Kenya Map",
                 status = "primary"
               )
        ),
        column(width = 6,
               box(
                 width = NULL,
                 plotlyOutput("plot", height = 500),
                 title = "Graphical Plot",
                 status = "primary"
               ),
               box(
                 width = NULL,
                 title = "Selection Controls",
                 status = "info",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(
                   column(width = 4,
                          selectInput("region", "Region:", choices = c("Province", "County"))
                   ),
                   conditionalPanel(
                     condition = "input.region == 'Province'",
                     selectInput("province", "Province:", choices = c("All", unique(merged_data$province_name)))
                   ),
                   conditionalPanel(
                     condition = "input.region == 'County'",
                     selectInput("county", "County:", choices = c("All", unique(merged_data$county_name)))
                   ),
                   column(width = 4,
                          selectInput("variable", "Variable:", choices = c("All", unique(merged_data$indicator_label)))
                   ),
                   column(width = 4,
                          sliderInput("year", "Year:", min = min(merged_data$survey_year), max = max(merged_data$survey_year), value = max(merged_data$survey_year), sep = "")
                   )
                 ),
                 fluidRow(
                   column(width = 6,
                          radioButtons("plot_type", "Plot Type:", choices = c("Time Series", "Histogram", "Scatter"))
                   ),
                   column(width = 6, conditionalPanel(
                     condition = "input.plot_type == 'Scatter'",
                     selectInput("scatter_var", "Additional Variable:", choices = "...")
                   ))
                 )
               )
        )
      )
    ),
    tabItem(
      tabName = "data_exploration",
      fluidRow(
        column(width = 4,
               box(
                 title = "Filtering Options",
                 width = NULL,
                 status = "info",
                 solidHeader = TRUE,
                 selectInput("year_filter", "Year:", choices = c("All", unique(merged_data$survey_year))),
                 selectInput("county_filter", "County:", choices = c("All", unique(merged_data$county_name))),
                 selectInput("province_filter", "Province:", choices = c("All", unique(merged_data$province_name))),
                 selectInput("indicator_filter", "Indicator:", choices = c("All", unique(merged_data$indicator_label))),
                 selectInput("indicator_id_filter", "Indicator ID:", choices = c("All", unique(merged_data$indicator_id)))
               )
        ),
        column(width = 8,
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