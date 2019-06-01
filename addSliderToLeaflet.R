
#ADD SLIDER/ANIMATION TO CONTROL PRESENTATION OF MARKERS ACCORDING TO FOLLOWING VARS:
# date, time, pos
# with convert_to_shapefile either TRUE or FALSE 

library(leaflet)
library(sf)
#library(htmlwidgets) ### probably necessary for slider?

nrow <- 100
df <- data.frame(lng = rep(-87.623720, nrow) + 0.0001*(1:nrow),
                 lat = rep(41.879747, nrow) + 0.0001*(1:nrow),
                 pos = 1:nrow,
                 date = as.Date('2019-01-01') + 1:nrow,
                 class = c(rep('A', nrow/2), rep('B', nrow/2)))

table(df$class)
summary(df$lng)
summary(df$lat)
summary(df$date)
df$time <- as.POSIXct(df$date) #Generate placeholder time variable
summary(df$time)

#Convert to shapefile?
convert_to_shapefile <- FALSE
if (convert_to_shapefile) {
  df <- st_as_sf(df,
                 coords = c('lng', 'lat'),
                 crs = 4326)
}



base_view <- leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles('CartoDB', layerId = "basetile") %>%
  setView(lng = -87.623720,
          lat = 41.879747,
          zoom = 12) 

base_view %>%
  addCircleMarkers(data = df[df$class == 'A',],
                   label = ~date,
                   radius = 2, opacity = 0.5, color = 'red',
                   group = 'A'
  ) %>%
  addCircleMarkers(data = df[df$class == 'B',],
                   label = ~date,
                   radius = 2, opacity = 0.5, color = 'blue',
                   group = 'B'
                   ) %>%
  addLayersControl(
    overlayGroups = c("A", "B"),
    options = layersControlOptions(collapsed = FALSE)
  )

