# Wrangle flights for analysis

## Load Libraries -----------------------------------------------
source("code/00_load_dependencies.R")

# Read and clean data --------------------------------------------------
# flight points

flight_points_df <- read_all_parquet("data/output/flight_points/") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# heliport zones
heliports_path <- "data/input/heliports.csv"
heliport_df <- 
  read_csv(heliports_path) %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS) %>%
  mutate(
    geometry = st_buffer(geometry, dist = radius) 
  )

# convert points to lines
flight_lines <- flight_points_df %>% 
  group_by(flight_id) %>% 
  # keep the timestamp order
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>%
  # smooth the edges of the lines
  smoothr::smooth(method = "spline")

# merge flight summary with flight lines to add geometry data
flight_summary <- read_csv("data/output/flight_summary.csv") %>% 
  merge(flight_lines, by = "flight_id") %>%
  st_as_sf()

# create df of all flights used for mapping
flights <- flight_summary %>%
  filter(starts_at != "unknown" | !is.na(flight_start_heliport_name), 
         ends_at != "unknown" | !is.na(flight_end_heliport_name)) %>%
  mutate(
    flight_start = case_when(
      !is.na(flight_start_heliport_name) ~ flight_start_heliport_name, 
      flight_start_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_start_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_start_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_start_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
      ), 
    flight_end = case_when(
      !is.na(flight_end_heliport_name) ~ flight_end_heliport_name, 
      flight_end_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_end_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_end_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_end_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
      )
    ) 

flights %>%
# write_csv(glue("data/output/wrangle_flights_flight_data.csv"))
write_parquet(flights, "data/output/wrangle_flights_flight_data.parquet")

# classify flights as tours, commuter/other, or nypd
flights <- flights %>%
  mutate(
    flight_type = case_when(
      (flight_start == flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "tour",
      (flight_start != flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "non-tour",
      str_detect(hex_id, "PD") ~ "NYPD",
      TRUE ~ "NA"
    ), 
    flight_path = map2_chr(flight_start, flight_end, function(x,y) paste(sort(c(x, y)), collapse = " - "))
    ) %>%
  mutate(
    flight_type = if_else(flight_path == "JFK Airport - JFK Airport", "non-tour", flight_type), 
    flight_type = if_else(flight_path == "West 30th Street Heliport - West 30th Street Heliport", "non-tour", flight_type))

flight_unknown <- flight_summary %>%
  filter(starts_at == "unknown" | ends_at == "unknown") %>%
  mutate(
    flight_start = case_when(
      !is.na(flight_start_heliport_name) ~ flight_start_heliport_name, 
      flight_start_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_start_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_start_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_start_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
    ), 
    flight_end = case_when(
      !is.na(flight_end_heliport_name) ~ flight_end_heliport_name, 
      flight_end_bbox_edge == "S" ~ "South of NYC (Unknown Location)",
      flight_end_bbox_edge == "N" ~ "North of NYC (Unknown Location)",
      flight_end_bbox_edge == "W" ~ "West of NYC (Unknown Location)",
      flight_end_bbox_edge == "E" ~ "East of NYC (Unknown Location)",
      TRUE ~ NA
    )
  ) %>%
  mutate(
    flight_type = case_when(
      (flight_start == flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "tour",
      (flight_start != flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "non-tour",
      str_detect(hex_id, "PD") ~ "NYPD",
      TRUE ~ "NA"
    ), 
    flight_path = map2_chr(flight_start, flight_end, function(x,y) paste(sort(c(x, y)), collapse = " - "))
  ) %>%
  mutate(
    flight_type = case_when(
      (flight_start == flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "tour",
      (flight_start != flight_end & !is.na(flight_start)) & !str_detect(hex_id, "PD") ~ "non-tour",
      str_detect(hex_id, "PD") ~ "NYPD",
      TRUE ~ "NA"
    ), 
    flight_path = map2_chr(flight_start, flight_end, function(x,y) paste(sort(c(x, y)), collapse = " - "))
  )

# create df of tour flights used for mapping
tour_flights <- flights %>%
  filter(flight_type == "tour")

# create df of commuter/other flights, i.e. non-tour, non-military/non-NYPD flights used for mapping
other_flights <- flights %>%
  filter(flight_type == "non-tour")

# create df of NYPD/military flights used for mapping
nypd_flights <- flights %>%
  filter(flight_type == "NYPD")

NA_fights <- flights %>%
  filter(is.na(flight_type))

