# Noise Complaints

## Load Dependencies -----------------------------------------------

# only need to source if haven't run it yet in session
source("code/analysis/01_wrangle_flights.R")

## Read Data -----------------------------------------------

# 311 Service Requests
# https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
# From 01-01-2022 to present for heli complaints
complaint_type=c("%27Noise%20-%20Helicopter%27")
url <- glue("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$where=complaint_type%20in({complaint_type})%20AND%20created_date%3E=%272023-05-01%27&$limit=999999999")
raw <- vroom::vroom(url)

# get helicopter noise complaints
heli_noise <- raw %>%
  filter(!is.na(latitude), 
         !is.na(longitude), 
         created_date >= as.Date("2023-05-01") & created_date < as.Date("2023-06-01")) %>%
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326)

heli_noise_df <- raw %>%
  filter(!is.na(latitude), 
         !is.na(longitude), 
         created_date >= as.Date("2023-05-01") & created_date < as.Date("2023-06-01"))

# create buffer around each 311 complaints 
noise_buffer <- heli_noise %>%
  st_transform(3488) %>%  # Convert to a projection that has a real unit (not degrees)
  st_buffer(dist = units::as_units(.5, "mile")) %>%  # Buffer 1 mile (sf handles units)
  st_transform("+proj=longlat +datum=WGS84")

## Noise Complaints and Flight Points Data Wrangling -----------------------------------------------

# connect noise complaints with flights that go over buffer
noise_flights_join <- st_join(noise_buffer, flight_points_df) 
#noise_flights_join <- readRDS("data/input/noise_flights_join.rds")

noise_flights <- noise_flights_join %>%
  filter(
    timestamp >= created_date - as.difftime(20, units="mins") & timestamp < created_date + as.difftime(1, units="mins"))

noise_flights_keys <- noise_flights %>%
  st_drop_geometry() %>%
  select(unique_key, obs_id)

dist = st_distance(
  # noise complaints point sf
  noise_flights_keys %>%
    left_join(heli_noise_df %>% select(unique_key, longitude, latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
  # flight points point sf
  noise_flights_keys %>%
    left_join(flight_points_df %>% select(obs_id, geometry)) %>%
    st_as_sf(sf_column_name = "geometry", crs = 4326), 
  by_element = TRUE)

noise_flights_dist <- noise_flights_keys %>%
  mutate(distance = dist)

noise_flights_time <- noise_flights %>%
  left_join(noise_flights_dist, by = c("unique_key", "obs_id")) %>%
  st_drop_geometry() %>%
  filter(
    timestamp >= created_date - as.difftime(20, units="mins") & timestamp < created_date
  ) %>%
  mutate(
    timediff = round(as.numeric(difftime(created_date, timestamp), units="mins"), 0)
  ) %>%
  select(unique_key, created_date, flight_id, flight_obs_id, timestamp, timediff, distance) %>%
  left_join(
    flights %>% select(flight_id, flight_start_timestamp, flight_start, flight_end, flight_type, flight_path, geometry)
  ) %>%
  left_join(heli_noise_df %>% select(unique_key, longitude, latitude), by = "unique_key")

# drop flights that have equidistant distances in the time window (can adjust later to choose more recent one instead of dropping)
closest_flight <- noise_flights_time %>%
  group_by(unique_key) %>%
  filter(distance == max(distance), 
         !(is.na(flight_type))) %>%
  filter(!duplicated(unique_key)) 

## Noise Complaints and Flight Points Analysis -----------------------------------------------

# complaints by flight type
closest_flight %>%
  group_by(flight_type) %>%
  summarise(n = n())

## Summaries by flight start -----------------------------------------------

# complaints by flight type and start heliport
complaints_type_start <- closest_flight %>%
  group_by(flight_type, flight_start) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# tour complaints by start heliport
closest_flight %>%
  filter(flight_type == "tour") %>%
  group_by(flight_type, flight_start) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# number of 311 complaints w/ identified flight


# percent of flights that result in a noise complaint by flight type and start heliport
flight_complaints <- flights %>%
  st_drop_geometry() %>% 
  group_by(flight_type, flight_start) %>%
  summarise(num_flights = n()) %>%
  left_join(complaints_type_start, by = c("flight_type", "flight_start")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    complaints_per_flight = n / num_flights
  ) %>%
  arrange(desc(complaints_per_flight))

## Summaries by flight path -----------------------------------------------

# complaints by flight type and start heliport
complaints_type_path <- closest_flight %>%
  group_by(flight_type, flight_path) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# complaints by flight path
complaints_path <- closest_flight %>%
  group_by(flight_path) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) 

flight_complaints_path <- flights %>%
  st_drop_geometry() %>% 
  group_by(flight_type, flight_path) %>%
  summarise(num_flights = n()) %>%
  left_join(complaints_type_path, by = c("flight_type", "flight_path")) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    complaints_per_flight = n / num_flights
  ) %>%
  arrange(desc(complaints_per_flight))

## Tables -----------------------------------------------

t1 <- flight_complaints %>%
  ungroup() %>%
  select(flight_start, flight_type, num_flights, n, complaints_per_flight) %>% 
  filter(num_flights > 50) %>%
  arrange(desc(complaints_per_flight)) %>%
  mutate(complaints_per_flight = round(complaints_per_flight, 2)) %>%
  rename(
    `Start Location` = flight_start,
    `Flight Type` = flight_type,
    `Number of Flights` = num_flights, 
    `Number of Complaints` = n, 
    `Complaints Per Flight` = complaints_per_flight
  ) %>%
  gt() %>%
  tab_header(title = "Flights and 311 Complaints") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t1 %>% gtsave("visuals/311_flights_together/311_flights_together.png")
t1 %>% gtsave("visuals/311_flights_together/311_flights_together.html")

t2 <- flight_complaints_path %>%
  ungroup() %>%
  select(flight_path, flight_type, num_flights, n, complaints_per_flight) %>% 
  filter(num_flights > 50, n > 1) %>%
  arrange(desc(complaints_per_flight)) %>%
  mutate(complaints_per_flight = round(complaints_per_flight, 2)) %>%
  rename(
    `Flight Path` = flight_path,
    `Flight Type` = flight_type,
    `Number of Flights` = num_flights, 
    `Number of Complaints` = n, 
    `Complaints Per Flight` = complaints_per_flight
  ) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(title = "Flight Paths and 311 Complaints") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t2 %>% gtsave("visuals/311_flights_together/311_flights_expanded.png")
t2 %>% gtsave("visuals/311_flights_together/311_flights_expanded.html")
  

## Map -----------------------------------------------

# size of buffers
leaflet()%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addPolygons(data = noise_buffer)

# all complaints
all_complaints_map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.05, color = "red", radius = 1, fill = FALSE)
councildown::mapshot(all_complaints_map, file = "visuals/311_flights_together/all_complaints_map.png", 
                     vwidth = 900, vheight = 870)

# all complaints and overlap with complaints w/ identified flights
leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.05, color = "red", radius = 1, fill = FALSE) %>%
  addCircleMarkers(data=closest_flight %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                   opacity = 0.05, color = "#2F56A6", radius = 1, fill = FALSE)

# all complaints w/ identified flights
complaints_with_flights <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=closest_flight %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                   opacity = 0.1, color = "#2F56A6", radius = 1, fill = FALSE)
councildown::mapshot(complaints_with_flights, file = "visuals/311_flights_together/complaints_with_flights_map.png", 
                     vwidth = 900, vheight = 870)

# tour complaints w/ identified flights
leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=closest_flight %>% filter(flight_type == "tour") %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                   opacity = 0.1, color = "#2F56A6", radius = 1, fill = FALSE)

# non-tour complaints w/ identified flights
leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=closest_flight %>% filter(flight_type == "non-tour") %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                   opacity = 0.1, color = "#2F56A6", radius = 1, fill = FALSE)

# NYPD complaints w/ identified flights
leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15))%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addCircleMarkers(data=closest_flight %>% filter(flight_type == "NYPD") %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326), 
                   opacity = 0.1, color = "#2F56A6", radius = 1, fill = FALSE)

# Explore specific flight path
flight_path_selected <- closest_flight %>% filter(flight_path == "West 30th Street Heliport - West 30th Street Heliport") %>% st_drop_geometry() %>% pull(flight_id) %>% unique() 
p <- flights %>% filter(flight_id %in% flight_path_selected)


leaflet()%>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
              fill = FALSE,
              opacity = 0.2,
              color = "black") %>%
  addPolylines(data=p, 
                   opacity = 0.1, color = "#2F56A6")

