# Map other flights by heliport

## Load Dependencies -----------------------------------------------

# only need to source if haven't run it yet in session
source("code/analysis/01_wrangle_flights.R")

# Map --------------------------------------------------
# function to map each heliport
other_by_heliport_map<- function(
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
  dir <- glue("visuals/other_flights/{dir_name}.png")
  councildown::mapshot(map, file = dir, 
                       vwidth = 900, vheight = 870)
}

# other counts by starting heliport
other_flights %>%
  st_drop_geometry() %>%
  group_by(flight_start_heliport_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# map tours at each heliport - (flight_df, heliport_name, sample_frac)
other_by_heliport_map(other_flights, "West 30th Street Heliport", 1)
other_by_heliport_map(other_flights, "East 34th Street Heliport", 1)
other_by_heliport_map(other_flights, "JFK Airport", 1)
other_by_heliport_map(other_flights, "Newark Airport", 1)
other_by_heliport_map(other_flights, "NYPD Floyd Bennett Field", 1)
# other_by_heliport_map(other_flights, "Linden Airport", 1)
# other_by_heliport_map(other_flights, "Downtown Manhattan Heliport", 0.1)
# other_by_heliport_map(other_flights, "Kearny, NJ", 0.1)
# other_by_heliport_map(other_flights, "Teterboro Airport", 1)
# other_by_heliport_map(other_flights, "Essex County Airport", 1)


