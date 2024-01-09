# Load required packages
pacman::p_load(shiny, shinydashboard, plotly, leaflet, bslib)

# Load functions from global.R file
source("app/global.R")
merged_data <- merge_data()

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
      selectInput("province", "Province:", choices = c("All", unique(merged_data$province_name)))
    ),
    conditionalPanel(
      condition = "input.region == 'County'",
      selectInput("county", "County:", choices = c("All", unique(merged_data$county_name)))
    ),
    selectInput("variable", "Variable:", choices = c("All", unique(merged_data$indicator_label))),
    sliderInput("year", "Year:", min = min(as.numeric(merged_data$survey_year)), max = max(as.numeric(merged_data$survey_year)), value = max(as.numeric(merged_data$survey_year)), sep = ""),
    radioButtons("plot_type", "Plot Type:", choices = c("Time Series", "Histogram", "Scatter")),
    conditionalPanel(
      condition = "input.plot_type == 'Scatter'",
      selectInput("scatter_var", "Additional Variable:", choices = "...")
    )
  ),
  
  # Filtering options for Data Exploration tab
  conditionalPanel(
    condition = "input.sidebarItem == 'data_exploration'",
    selectInput("year_filter", "Year:", choices = c("All", unique(merged_data$survey_year))),
    selectInput("county_filter", "County:", choices = c("All", unique(merged_data$county_name))),
    selectInput("province_filter", "Province:", choices = c("All", unique(merged_data$province_name))),
    selectInput("indicator_filter", "Indicator:", choices = c("All", unique(merged_data$indicator_label))),
    selectInput("indicator_id_filter", "Indicator ID:", choices = c("All", unique(merged_data$indicator_id)))
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
                 solidHeader = TRUE
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

# Create the Shiny app
ui <- dashboardPage(header, sidebar, body)


#sidebar <- dashboardSidebar()
#body <- dashboardBody()
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

server <- function(input, output, session) {}
shinyApp(ui = ui, server = server)