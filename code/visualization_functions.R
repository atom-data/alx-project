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

# create a choropleth map function:
create_choropleth_map <- function(data, variable, year) {
  # Validate data (example)
  if (is.null(data) || is.null(variable) || is.null(year)) {
    stop("Invalid input data or parameters.")
  }
  
  filtered_data <- data %>% filter(indicator_id == variable, survey_year == year)
  pal <- colorNumeric("YlOrRd", domain = filtered_data$mean_value, na.color = "transparent")
  
  leafletProxy("map") %>%
    clearShapes() %>%
    addPolygons(
      data = filtered_data,
      group = "province_boundaries",
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
      popup = ~paste0("<strong>Province:</strong> ", province_name, "<br/>",
                      "<strong>Counties:</strong> ", paste(county_name, collapse = ", "), "<br/>",
                      "<strong>Variable:</strong> ", variable, "<br/>",
                      "<strong>Year:</strong> ", year, "<br/>",
                      "<strong>Mean Value:</strong> ", mean_value)
    ) %>%
    showGroup("province_boundaries") %>%  # Show the hidden group
    create_legend(values = ~filtered_data$mean_value, colors = pal)
}
