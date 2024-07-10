# Noise Complaints

## Load Libraries -----------------------------------------------

source("code/analysis/01_wrangle_flights.R")

## Read 311 Data -----------------------------------------------

# 311 Service Requests
# https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
# From 01-01-2022 to present for heli complaints
complaint_type=c("%27Noise%20-%20Helicopter%27")
url <- glue("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$where=complaint_type%20in({complaint_type})%20AND%20created_date%3E=%272019-01-01%27&$limit=999999999")
raw <- vroom::vroom(url)

# get helicopter noise complaints
heli_noise <- raw %>%
  filter(!is.na(latitude), 
         !is.na(longitude), 
         created_date < "2023-06-01" & created_date >= "2023-05-01") %>%
  mutate(
    hour = hour(created_date), 
    day = weekdays(created_date), 
    weekday = ifelse(day %in% c("Saturday", "Sunday"), "weekend", "weekday") 
  ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)

# drop geometry to make faster
heli_noise_df <- heli_noise %>%
  st_drop_geometry()

# read heliport points
heliport_point <- 
  read_csv(heliports_path) %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("+proj=longlat +datum=WGS84") 

# Twitter map
library(htmltools)
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 20%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("May 2023 Helicopter Flights and</br>311 Helicopter Noise Complaints")
)  

geo = sf::st_sfc(sf::st_point(c(-73.7, 40.525)))
source_notes_geo = sf::st_sf(source = "Source: NYCC Data Team analysis of FlightRadar24 data and OpenData NYC 311 Service Requests",
                             geometry = geo)

twitter_map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15)) %>%
  setView(-73.999,40.734103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df %>% filter(!(name %in% c("Hackensack Univ Medical Center", "Staten Island Univ Hospital"))),
                       fill = FALSE,
                       opacity = 0.8,
                       color = "#666666") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.02, color = "red", stroke = F, radius = 2, fill = T) %>%
  addPolylines(data=flights, opacity = 0.03, color = "#2F56A6") %>%
  addControl(title, position = "topleft", className="map-title") %>%
  leaflet::addLabelOnlyMarkers(data = source_notes_geo,
                               label = ~source,
                               labelOptions = labelOptions(noHide = T,
                                                           direction = 'left',
                                                           textOnly = T,
                                                           style = list('color'="#555555",
                                                                        'fontSize'="15px")))
councildown::mapshot(twitter_map, file = "visuals/311_flights_together/twitter_map.png", 
                     vwidth = 900, vheight = 870)
