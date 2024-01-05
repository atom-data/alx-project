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
