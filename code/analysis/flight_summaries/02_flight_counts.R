
source("code/analysis/01_wrangle_flights.R")

SECONDS_PER_HOUR <- 3600

nyc_boro_wshore <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary_Water_Included/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")
nyc_boro_wshore_buff <- 
  nyc_boro_wshore %>% 
  select(boro_name = BoroName) %>% 
  st_set_crs(CRS) %>%
  st_buffer(dist = units::as_units(.5, "mile"))

points_df <- 
  flight_points_df %>%
  st_join(nyc_boro_wshore_buff)

cross_into_nyc_df <- 
  points_df %>%
  group_by(flight_id) %>%
  arrange(timestamp) %>%
  filter(!is.na(boro_name) & is.na(lag(boro_name))) %>%
  arrange(timestamp) %>%
  mutate(cross_id = row_number()) %>%
  st_drop_geometry()

cross_out_nyc_df <- 
  points_df %>%
  group_by(flight_id) %>%
  arrange(timestamp) %>%
  filter(!is.na(boro_name) & is.na(lead(boro_name))) %>%
  arrange(timestamp) %>%
  mutate(cross_id = row_number()) %>%
  st_drop_geometry()
   
cross_intervals_df <- 
  left_join(
    cross_into_nyc_df,
    cross_out_nyc_df,
    by = c("flight_id", "cross_id")
  ) %>%
  select(
    flight_id,
    enter_timestamp = timestamp.x,
    exit_timestamp = timestamp.y
  ) %>%
  mutate(
    interval_duration = time_length(exit_timestamp - enter_timestamp, unit = "seconds")
  ) %>%
  group_by(flight_id) %>%
  summarise(total_nyc_sec = sum(interval_duration)) %>%
  ungroup()

flights %>%
  st_drop_geometry() %>%
  left_join(cross_intervals_df, by = "flight_id") %>%
  mutate(
    frac_time_in_nyc = total_nyc_sec/flight_duration_secs
  ) %>%
  ggplot() + 
  geom_histogram(aes(x = frac_time_in_nyc), binwidth = 0.01)

t1 <- 
  flights %>%
  st_drop_geometry() %>%
  left_join(cross_intervals_df, by = "flight_id") %>%
  group_by(flight_type) %>%
  summarise(
    total_count = n(),
    total_time_hours = sum(total_nyc_sec)/SECONDS_PER_HOUR
  ) %>%
  mutate(
    pct_count = total_count/sum(total_count),
    pct_time = total_time_hours/sum(total_time_hours)
  ) %>%
  arrange(desc(pct_time)) %>%
  transmute(
    `Flight Type` = if_else(flight_type == "NYPD", "NYPD", str_to_title(flight_type)),
    `% of Total Flights` = paste0(round(pct_count * 100), "%"), 
    `% of Total Flight Time` = paste0(round(pct_time * 100), "%"
    )
  ) %>%
  gt() %>%
  tab_header(
    title = "Flight Type Distribution",
    subtitle = glue("Total Flights = {formatC(nrow(flights), format='d', big.mark=',')}")
    ) %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

gtsave(t1, "visuals/all_flights/flight_type_dist.png")
gtsave(t1, "visuals/all_flights/flight_type_dist.html")


## Flight Paths 

t2 <- flights %>%
  st_drop_geometry() %>%
  group_by(flight_path, flight_type) %>%
  summarise(num_flights = n()) %>%
  ungroup() %>%
  filter(num_flights > 5) %>%
  arrange(desc(num_flights)) %>%
  rename(
    `Flight Path` = flight_path,
    `Flight Type` = flight_type,
    `Number of Flights` = num_flights
  ) %>%
  gt() %>%
  tab_header(title = "Most Frequent Flight Paths") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t2 %>% gtsave("visuals/all_flights/flight_paths.png")
t2 %>% gtsave("visuals/all_flights/flight_paths.html")
