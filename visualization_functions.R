library(leaflet)
library(ggplot2)

create_basemap <- function(data) {
  leaflet(data) %>%
    setMaxBounds(33.911819, -4.702271, 41.906258, 5.430648) %>%
    setView(lng = 37.818, lat = 0.606, zoom = 7) %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Esri", "Terrain", "Toner"),
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
