#ADD SLIDER/ANIMATION TO CONTROL PRESENTATION OF MARKERS ACCORDING TO FOLLOWING VARS:
# date, time, pos
# with convert_to_shapefile either TRUE or FALSE 

#Source: https://stackoverflow.com/a/43291090/6142623

library(leaflet)
library(sf)
library(htmlwidgets)
library(htmltools)
library(geojsonio)
library(lubridate)

nrow <- 100
df <- data.frame(lng = rep(-87.623720, nrow) + 0.0001*(1:nrow),
                 lat = rep(41.879747, nrow) + 0.0001*(1:nrow),
                 pos = 1:nrow,
                 start = as.Date('2019-01-01') + 1:nrow,
                 class = c(rep('A', nrow/2), rep('B', nrow/2)))
 
df$end <- df$start + 1

#Try to clean up date
commence_clean <- TRUE
if (commence_clean) {
  df$start <- as.POSIXct(df$start)
  df$end <- as.POSIXct(df$end)
  df$start_clean_time <- df$start
  df$end_clean_time <- df$end
  # df$start <- format(df$start, "%Y-%m-%d, %I:%M %p", usetz = F)
  # df$end <- format(df$end, "%Y-%m-%d, %I:%M %p", usetz = F)
  # 2012-11-02T19:30:00.000Z -- UTC / GMT
  #THIS WORKS! BUT REPLACE WITH RIGHT GMT
  df$start <- paste0(year(df$start),
                     '-',
                     ifelse(month(df$start) < 10, paste0('0', month(df$start)), month(df$start)),
                     '-',
                     ifelse(day(df$start) < 10, paste0('0', day(df$start)), day(df$start)),
                     'T',
                     ifelse(hour(with_tz(df$start, tz = 'UTC')) < 10,
                            paste0('0',hour(with_tz(df$start, tz = 'UTC'))),
                            hour(with_tz(df$start, tz = 'UTC'))),
                     ':',
                     ifelse(minute(df$start) < 10, paste0('0', minute(df$start)), minute(df$start)),
                     ':',
                     ifelse(second(df$start) < 10, paste0('0', second(df$start)), second(df$start)),
                     '.000Z')
  df$end <- paste0(year(df$end),
                     '-',
                     ifelse(month(df$end) < 10, paste0('0', month(df$end)), month(df$end)),
                     '-',
                     ifelse(day(df$end) < 10, paste0('0', day(df$end)), day(df$end)),
                     'T',
                     ifelse(hour(with_tz(df$end, tz = 'UTC')) < 10,
                            paste0('0',hour(with_tz(df$end, tz = 'UTC'))),
                            hour(with_tz(df$end, tz = 'UTC'))),
                     ':',
                     ifelse(minute(df$end) < 10, paste0('0', minute(df$end)), minute(df$end)),
                     ':',
                     ifelse(second(df$end) < 10, paste0('0', second(df$end)), second(df$end)),
                     '.000Z')
  
  
  #WITH FUNCTION
  time_character <- function(time) {
    time <- paste0(year(time),
                   '-',
                   ifelse(month(time) < 10, paste0('0', month(time)), month(time)),
                   '-',
                   ifelse(day(time) < 10, paste0('0', day(time)), day(time)),
                   'T',
                   ifelse(hour(with_tz(time, tz = 'UTC')) < 10,
                          paste0('0',hour(with_tz(time, tz = 'UTC'))),
                          hour(with_tz(time, tz = 'UTC'))),
                   ':',
                   ifelse(minute(time) < 10, paste0('0', minute(time)), minute(time)),
                   ':',
                   ifelse(second(time) < 10, paste0('0', second(time)), second(time)),
                   '.000Z')
    return(time)
    
  }
  
  
  df$end_check <- time_character(df$end_clean_time)
  df$start_check <- time_character(df$start_clean_time)
  # df$end <- paste0(year(df$end),
  #                    '-',
  #                    ifelse(month(df$end) < 10, paste0('0', month(df$end)), month(df$end)),
  #                    '-',
  #                    ifelse(day(df$end) < 10, paste0('0', day(df$end)), day(df$end)),
  #                    'T',
  #                    ifelse(hour(df$end) < 10, paste0('0', hour(df$end)), hour(df$end)),
  #                    ':',
  #                    ifelse(minute(df$end) < 10, paste0('0', minute(df$end)), minute(df$end)),
  #                    ':',
  #                    ifelse(second(df$end) < 10, paste0('0', second(df$end)), second(df$end)),
  #                    '.000Z')
  print(df[,c('start', 'start_check', 'start_clean_time')])
  print(df[,c('end', 'end_check', 'end_clean_time')])
  print(all(df$start_check == df$start))
  print(all(df$end_check == df$end))
}




table(df$class)
summary(df$lng)
summary(df$lat)
summary(df$date)

df$start
df$end 


# use geojsonio to convert our data.frame
#  to GeoJSON which timeline expects
df_geo <- geojson_json(df,lat="lat",lon="lng", pretty = T)

#Convert to shapefile?
convert_to_shapefile <- TRUE
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

base_view$dependencies[[length(base_view$dependencies)+1]] <- htmlDependency(
  name = "leaflet-timeline",
  version = "1.0.0",
  src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
  script = "javascripts/leaflet.timeline.js",
  stylesheet = "stylesheets/leaflet.timeline.css"
)

with_animation <- base_view %>%
  addCircleMarkers(data = df[df$class == 'A',],
                   label = ~start,
                   radius = 2, opacity = 0.5, color = 'red',
                   group = 'A'
  ) %>%
  addCircleMarkers(data = df[df$class == 'B',],
                   label = ~start,
                   radius = 2, opacity = 0.5, color = 'blue',
                   group = 'B'
  ) %>%
    addLayersControl(
    overlayGroups = c("A", "B"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  onRender(sprintf(
    '
        function(el,x){
        var data = %s;

        var timelineControl = L.timelineSliderControl({
          formatOutput: function(date) {
            return new Date(date).toString();
          }
        });
        var timeline = L.timeline(data, {
          pointToLayer: function(data, latlng){
            var hue_min = 60;
            var hue_max = 0;
            var hue = hue_min;
            return L.circleMarker(latlng, {
              radius: 10,
              color: "hsl("+hue+", 100%%, 50%%)",
              fillColor: "hsl("+hue+", 100%%, 50%%)"
            });
          },
          steps: 1000,
          duration: 10000,
          showTicks: true
        });
        timelineControl.addTo(HTMLWidgets.find(".leaflet").getMap());
        timelineControl.addTimelines(timeline);
        timeline.addTo(HTMLWidgets.find(".leaflet").getMap());
        }
        ',df_geo
  ))
#https://stackoverflow.com/questions/13091523/javascript-invalid-date-error-in-internet-explorer
#for date issue with IE

saveWidget(with_animation, 'with_animation.html')
