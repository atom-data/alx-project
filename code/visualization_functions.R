# Load the required libraries
pacman::p_load(leaflet, dplyr, htmltools)
# Create basemap function
create_basemap <- function(data) {
  leaflet(data) %>%
    setMaxBounds(32, -4, 42, 6) %>%
    setView(lng = 37.818, lat = 0.606, zoom = 7) %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')), group = "MapBox") %>%
    addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>%
    addProviderTiles(providers$Stadia.StamenToner, group = "Toner") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Esri", "MapBox", "Terrain", "Toner"),
      options = layersControlOptions(collapsed = TRUE),
      position = "bottomright"
    ) %>%
    addEasyButton(easyButton(
      icon = "fa-globe", title = "Zoom to Level 7",
      onClick = JS("function(btn, map){ map.setZoom(7); }"))) %>%
    addEasyButton(easyButton(
      icon = "fa-arrows-alt", title = "Reset View",
      onClick = JS("function(btn, map){ map.fitBounds(map.getBounds()); }"))) %>%
    hideGroup("province_boundaries")  # Hide initial boundaries
}

# Function for legend creation 
create_legend <- function(data, values, colors, title = "Legend") {
  data %>% addLegend(
    position = "topright",  # position
    pal = colors,
    values = values,
    title = title,
    labFormat = labelFormat(digits = 2),  # customized label formatting
    opacity = 1
  )
}

# Function to validate inputs
validate_choropleth_inputs <- function(data, variable, year) {
  validate(
    need(data, "Data is missing"),
    need(variable, "Variable is missing"),
    need(year, "Year is missing")
  )
}

# Function to filter and summarize data
prepare_choropleth_data <- function(data, variable, year) {
  filtered_data <- data %>%
    filter(indicator_label == variable, survey_year == year) %>%
    group_by(province_name) %>%
    summarise(
      indicator_label = first(indicator_label),
      mean_value = round(first(mean_value), 2),
      counties = paste(sort(county_name), collapse = ", ")
    )
  
  return(filtered_data)
}


# Function to create the color palette
create_choropleth_palette <- function(data) {
  colorNumeric("YlOrRd", domain = data$mean_value, na.color = "transparent")
}

# Function to update the leaflet map with choropleth data
update_choropleth_map <- function(mapId = "map", data, pal) {
  leafletProxy(mapId) %>%
   clearShapes() %>%
    addPolygons(
      data = data,
      group = "choropleth_map",
      layerId = ~province_name,
      stroke = FALSE,
      smoothFactor = 0.2,
      fillOpacity = 0.8,
      fillColor = ~pal(mean_value),
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        bringToFront = TRUE
      ),
      popup = ~paste0(
        "<strong>Province:</strong> ", province_name, "<br/>",
        "<strong>Counties:</strong> ", counties, "<br/>",
        "<strong>Indicator:</strong> ", indicator_label, "<br/>",
        "<strong>Year:</strong> ", year, "<br/>",
        "<strong>Mean Value:</strong> ", mean_value
      )
    ) %>%
    showGroup("choropleth_map") %>%
    create_legend(values = ~data$mean_value, colors = pal, title = first(data$indicator_label))
}

# Main function to create the choropleth map
create_choropleth_map <- function(mapId = "map", data, variable, year) {
  validate_choropleth_inputs(data, variable, year)
  
  filtered_data <- prepare_choropleth_data(data, variable, year)
  
  if (!is.null(filtered_data) && "mean_value" %in% colnames(filtered_data)) {
    pal <- create_choropleth_palette(filtered_data)
    update_choropleth_map(filtered_data, pal)
  } else {
    showNotification("Invalid data or missing columns for choropleth map.", type = "error")
  }
}



