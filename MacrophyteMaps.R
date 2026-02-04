# ==========================================
# Interactive Macrophyte Distribution Mapper (Draw + Shade)
# ==========================================

# Install if needed:
# install.packages(c("leaflet", "leaflet.extras", "sf"))

library(leaflet)
library(leaflet.extras)
library(sf)

# --- Center on Falling Creek Reservoir ---
center_lat <- 37.3076
center_lon <- -79.8370

# --- Build the map ---
leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite (Esri)") %>%
  addProviderTiles("CartoDB.Positron", group = "Light Basemap") %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE) %>%
  addLayersControl(
    baseGroups = c("Satellite (Esri)", "Light Basemap"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = center_lon, lat = center_lat, zoom = 15) %>%
  # --- Drawing toolbar: points + polygons + rectangles ---
  addDrawToolbar(
    targetGroup = "Macrophyte Distribution",
    polygonOptions = drawPolygonOptions(
      shapeOptions = drawShapeOptions(
        fillColor = "green", fillOpacity = 0.4, color = "darkgreen", weight = 2
      )
    ),
    rectangleOptions = drawRectangleOptions(
      shapeOptions = drawShapeOptions(
        fillColor = "green", fillOpacity = 0.4, color = "darkgreen", weight = 2
      )
    ),
    markerOptions = drawMarkerOptions(),   # keep simple: no icon arg
    circleOptions = FALSE,
    circleMarkerOptions = FALSE,
    polylineOptions = FALSE,
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("darkgreen"),
    labels = c("Distribution area")
  )


# Version 2 ####
# install.packages(c("leaflet", "leaflet.extras", "sf"))

library(leaflet)
library(leaflet.extras)
library(sf)

center_lat <- 37.3076
center_lon <- -79.8370

leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite (Esri)") %>%
  addProviderTiles("CartoDB.Positron", group = "Light Basemap") %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE) %>%
  addLayersControl(
    baseGroups = c("Satellite (Esri)", "Light Basemap"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng = center_lon, lat = center_lat, zoom = 15) %>%
  addDrawToolbar(
    targetGroup = "Macrophyte Distribution",
    polygonOptions = drawPolygonOptions(
      shapeOptions = drawShapeOptions(
        fillColor = "green", fillOpacity = 0.4, color = "darkgreen", weight = 2
      )
    ),
    rectangleOptions = drawRectangleOptions(
      shapeOptions = drawShapeOptions(
        fillColor = "green", fillOpacity = 0.4, color = "darkgreen", weight = 2
      )
    ),
    markerOptions = drawMarkerOptions(),
    circleOptions = FALSE,
    circleMarkerOptions = FALSE,
    polylineOptions = FALSE,
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("darkgreen"),
    labels = c("Distribution area"),
    title = "Macrophyte Layers"
  )
