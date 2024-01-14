# Load required packages
pacman::p_load(ggplot2, plotly, dplyr)

create_plotly_plot <- function(data, region, province, variable, year, plot_type, scatter_var) {
  
  # Filter data based on user selections
  filtered_data <- data %>%
    filter(
      if (region == "Province") province_name == province else county_name == province,
      indicator_label == variable,
      survey_year == year
    )
  
  # Create the plot based on plot_type
  if (plot_type == "Time Series") {
    plot_ly(filtered_data, x = ~survey_year, y = ~mean_value, type = "scatter", mode = "lines+markers") %>%
      layout(
        title = paste("Time Series of", variable, "in", province),
        xaxis = list(title = "Year"),
        yaxis = list(title = variable),
        marker = list(size = 10, symbol = "square"),  # Square markers
        line = list(width = 3),                       # Thicker line
        font = list(family = "sans-serif", size = 14), # Consistent font
        plot_bgcolor = "whitesmoke",                   # Background color
        paper_bgcolor = "whitesmoke"                  # Background color
      )
  } else if (plot_type == "Histogram") {
    plot_ly(filtered_data, x = ~mean_value) %>%
      add_histogram() %>%
      layout(
        title = paste("Histogram of", variable, "in all Provinces for", year),
        xaxis = list(title = variable),
        yaxis = list(title = "Count"),
        font = list(family = "sans-serif", size = 14), # Consistent font
        plot_bgcolor = "whitesmoke",                   # Background color
        paper_bgcolor = "whitesmoke"                  # Background color
      )
  } else if (plot_type == "Scatter") {
    plot_ly(filtered_data, x = ~mean_value, y = ~get(scatter_var)) %>%
      add_markers() %>%
      layout(
        title = paste("Scatter Plot of", variable, "against", scatter_var, "in", province, "for", year),
        xaxis = list(title = variable),
        yaxis = list(title = scatter_var),
        font = list(family = "sans-serif", size = 14), # Consistent font
        plot_bgcolor = "whitesmoke",                   # Background color
        paper_bgcolor = "whitesmoke"                  # Background color
      )
  } else {
    # Handle invalid plot_type
    plotly_empty()
  }
}
