# Load required packages
pacman::p_load(plotly, dplyr)

create_plotly_plot <- function(data, region, province, variable, year, plot_type, scatter_var) {
  
  # Filter data based on user selections
  # Filter data based on user selections and plot type
  filtered_data <- if (plot_type == "Time Series") {
    data %>%
      filter(
        if (region == "Province") province_name == province else county_name == province,
        indicator_label == variable
      )
  } else if (plot_type == "Histogram") {
    data %>%
      filter(indicator_label == variable, survey_year == year) %>%
      distinct(province_name, mean_value)  # Keep only unique values per province
  } else if (plot_type == "Scatter") {
    data %>%
      select(indicator_label, survey_year, mean_value) %>%
      filter(indicator_label %in% c(variable, scatter_var), survey_year == year) %>%  # Filter for both variables
      pivot_wider(names_from = "indicator_label", values_from = "mean_value")  # Reshape
  } else {
    # Handle invalid plot_type
    data.frame()  # Return empty data frame
  }
  
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
    plot_ly(filtered_data, x = ~get(variable), y = ~get(scatter_var)) %>%  # Access variables directly
      add_markers() %>%
      layout(
        title = paste("Scatter Plot of", variable, "against", scatter_var, "for", year),
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
