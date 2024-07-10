# Map all flights by heliport

## Load Dependencies -----------------------------------------------

# only need to source if haven't run it yet in session
source("code/analysis/01_wrangle_flights.R")

# 311 Service Requests -----------------------------------------------
# https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
# From 01-01-2022 to present for heli complaints
complaint_type=c("%27Noise%20-%20Helicopter%27")
url <- glue("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$where=complaint_type%20in({complaint_type})%20AND%20created_date%3E=%272021-01-01%27&$limit=999999999")
raw <- vroom::vroom(url)

# get helicopter noise complaints
heli_noise <- raw %>%
  filter(!is.na(latitude), 
         !is.na(longitude), 
         created_date < "2023-06-01" & created_date >= "2023-05-01") %>%
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)

# Map --------------------------------------------------

# map select flights by takeoff location and type (sample or full)
map_all_flights <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.8,
              color = "#666666") %>%
  addPolylines(data=flights %>% filter(flight_start == "Downtown Manhattan Heliport", flight_type == "tour"), 
               opacity = 0.02, color = "#2F56A6", group = "Downtown Manhattan Heliport (Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "Kearny, NJ", flight_type == "tour"), 
               opacity = 0.02, color = "#2F56A6", group = "Kearny, NJ (Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "Linden Airport", flight_type == "tour"), 
               opacity = 0.02, color = "#2F56A6", group = "Linden Airport (Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "Westchester County Airport", flight_type == "tour"), 
               opacity = 0.02, color = "#2F56A6", group = "Westchester County Airport (Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "West 30th Street Heliport", flight_type == "non-tour"), 
               opacity = 0.02, color = "#2F56A6", group = "West 30th Street Heliport (Non-Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "East 34th Street Heliport", flight_type == "non-tour"), 
               opacity = 0.02, color = "#2F56A6", group = "East 34th Street Heliport (Non-Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "JFK Airport", flight_type == "non-tour"), 
               opacity = 0.02, color = "#2F56A6", group = "JFK Airport (Non-Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "Newark Airport", flight_type == "non-tour"), 
               opacity = 0.02, color = "#2F56A6", group = "Newark Airport (Non-Tour)") %>%
  addPolylines(data=flights %>% filter(flight_start == "NYPD Floyd Bennett Field", flight_type == "NYPD"), 
               opacity = 0.02, color = "#2F56A6", group = "NYPD Floyd Bennett Field (NYPD)") %>%
  addPolylines(data=flights %>% filter(is.na(flight_start_heliport_name)), 
               opacity = 0.02, color = "#2F56A6", group = "Outside of NYC Area (All Types)") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.05, color = "red", radius = 1, fill = FALSE, 
                   group = "311 Helicopter Noise Complaints") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Downtown Manhattan Heliport (Tour)", "Kearny, NJ (Tour)", "Linden Airport (Tour)", 
                      "Westchester County Airport (Tour)",
                      "West 30th Street Heliport (Non-Tour)", "East 34th Street Heliport (Non-Tour)", "JFK Airport (Non-Tour)", 
                      "Newark Airport (Non-Tour)", "NYPD Floyd Bennett Field (NYPD)", "Outside of NYC Area (All Types)",
                      "311 Helicopter Noise Complaints"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(group = c("Kearny, NJ (Tour)", "Linden Airport (Tour)", 
                      "Westchester County Airport (Tour)",
                      "West 30th Street Heliport (Non-Tour)", "East 34th Street Heliport (Non-Tour)", "JFK Airport (Non-Tour)", 
                      "Newark Airport (Non-Tour)", "NYPD Floyd Bennett Field (NYPD)", "Outside of NYC Area (All Types)")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Flight Takeoff Location (Flight Type)</label>');
        }
    ")

htmlwidgets::saveWidget(map_all_flights, file="visuals/all_flights/leaflet_all_flights.html", selfcontained = T)


leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
                       fill = FALSE,
                       opacity = 0.8,
                       color = "#666666") %>%
  addPolylines(data=flights %>% filter(flight_end == "West 30th Street Heliport", flight_type == "tour"), 
               opacity = 0.05, color = "#2F56A6")
