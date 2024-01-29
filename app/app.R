# Load required packages
pacman::p_load(shiny, shinydashboard, plotly, leaflet, bslib)

# Load global file
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


# Create the Shiny app
#ui <- dashboardPage(header, sidebar, body)


#sidebar <- dashboardSidebar()
#body <- dashboardBody()
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# Define the server logic
server <- function(input, output, session){
  source("code/visualization_functions.R")
  source("code/plot_functions.R")
 
  
  # Load and merge data (assuming you have functions for this)
  merged_data <- merge_data()
  
  # Create basemap (this stays static)
  output$map <- renderLeaflet({
    create_basemap(merged_data)
  })
  
  # Define graphical plot visualization
  # Inside your server function:
  output$plot <- renderPlotly({
    # Access user input values
    region <- input$region
    province <- input$province
    variable <- input$variable
    year <- input$year
    plot_type <- input$plot_type
    scatter_var <- input$scatter_var  # Only for scatter plot
    
    # Call the create_plotly_plot function
    plot <- create_plotly_plot(merged_data, region, province, variable, year, plot_type, scatter_var)
    
    # Return the plot
    if (variable == "All") {
      plotly_empty()
    } else {
      plot
    }
  })
  
  # Define reactive expression for filtered data
  filtered_data <- reactive({
    merged_data %>%
      st_drop_geometry() %>%
      filter(
        if (input$year_filter != "All") survey_year == input$year_filter else TRUE,
        if (input$county_filter != "All") county_name == input$county_filter else TRUE,
        if (input$province_filter != "All") province_name == input$province_filter else TRUE,
        if (input$indicator_filter != "All") indicator_label == input$indicator_filter else TRUE
      )
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    tryCatch({
      DT::datatable(filtered_data(), options = list(pageLength = 10))
    }, error = function(err) {
      # Handle errors gracefully
      showNotification(paste("Error:", err$message), type = "error")
      return(NULL)
    })
  })
  
  # Data download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_data_to_download <- filtered_data()  # Store filtered data in a variable
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  observe({print(filtered_data())})
}
shinyApp(ui = ui, server = server)