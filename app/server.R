# Define the server logic
server <- function(input, output, session){
  source("code/visualization_functions.R")
  source("code/plot_functions.R")
  
  # Create basemap (this stays static)
  output$map <- renderLeaflet({
    create_basemap(merged_data)
  })
  
  # Load and merge data (assuming you have functions for this)
  merged_data <- merge_data()
  
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
}
shinyApp(ui = ui, server = server)