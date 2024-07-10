# Map all flights by heliport

## Load Dependencies -----------------------------------------------

# only need to source if haven't run it yet in session
source("code/analysis/01_wrangle_flights.R")

# Map --------------------------------------------------
# map all heliports 
map_heliports <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 15))%>%
  setView(-73.999,40.698103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df %>% filter(!(name %in% c("Hackensack Univ Medical Center", "Staten Island Univ Hospital"))),
                       fill = FALSE,
                       opacity = 0.5,
                       color = "#2F56A6") %>%
  addLabelOnlyMarkers(data = read_csv(heliports_path) %>%
                        janitor::clean_names() %>% 
                        filter(!(name %in% c("Hackensack Univ Medical Center", "Staten Island Univ Hospital"))), 
                      ~longitude, ~latitude, label =  ~as.character(name), 
                      labelOptions = labelOptions(noHide = T, direction = 'right', 
                                                  textOnly = T, style = list("font-size" = "14px")))

councildown::mapshot(map_heliports, file = "visuals/all_flights/heliports.png", 
                     vwidth = 900, vheight = 870)
htmlwidgets::saveWidget(map_heliports, file="visuals/all_flights/heliports.html", selfcontained = T)


# map all flights (full)
map_all_flights <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df %>% filter(!(name %in% c("Hackensack Univ Medical Center", "Staten Island Univ Hospital"))),
                       fill = FALSE,
                       opacity = 0.8,
                       color = "#666666") %>%
  addPolylines(data=flights, opacity = 0.015, color = "#2F56A6")

councildown::mapshot(map_all_flights, file = "visuals/all_flights/all_flights.png", 
                     vwidth = 900, vheight = 870)
htmlwidgets::saveWidget(map_all_flights, file="visuals/all_flights/all_flights.html", selfcontained = T)
 

# function to map each heliport
flight_by_heliport_map<- function(
    flight_df, 
    heliport_name, 
    sample_frac = 1
){
  flight_subset <- flight_df %>%
    filter(flight_start_heliport_name == heliport_name) %>%
    sample_frac(sample_frac)
  
  map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15)) %>%
    setView(-73.999,40.704103, zoom=11) %>% 
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = heliport_df,
                fill = FALSE,
                opacity = 0.2,
                color = "black") %>%
    addPolylines(data=flight_subset, 
                 opacity = 0.05, color = "#2F56A6") 
  
  dir_name <- str_replace(heliport_name, " ", "_")
  dir <- glue("visuals/all_flights/{dir_name}.png")
  councildown::mapshot(map, file = dir, 
          vwidth = 900, vheight = 870)
}

# counts by starting heliport
flights %>%
  st_drop_geometry() %>%
  group_by(flight_start_heliport_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  adorn_totals()

# map each heliport - (flight_df, heliport_name, sample_frac)
flight_by_heliport_map(flights, "Downtown Manhattan Heliport", 0.1)
flight_by_heliport_map(flights, "Kearny, NJ", 0.1)
flight_by_heliport_map(flights, "West 30th Street Heliport", 0.1)
flight_by_heliport_map(flights, "Linden Airport", 0.1)
flight_by_heliport_map(flights, "East 34th Street Heliport", 0.1)
flight_by_heliport_map(flights, "JFK Airport", 0.1)
flight_by_heliport_map(flights, "NYPD Floyd Bennett Field", 0.1)
flight_by_heliport_map(flights, "Teterboro Airport", 0.1)
flight_by_heliport_map(flights, "Essex County Airport", 0.1)
flight_by_heliport_map(flights, "Newark Airport", 0.1)
flight_by_heliport_map(flights, "LaGuardia Airport", 0.1)

# NA flights
leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15)) %>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addPolylines(data=flights %>% filter(is.na(flight_start_heliport_name)), 
               opacity = 0.05, color = "#2F56A6")

