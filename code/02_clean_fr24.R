
source("code/00_load_dependencies.R")
source("code/01_clean_helicopters.R")

output_path <- glue("data/output")

# May, 2023
# csvs are not available for public use. Contact FlightRadar24 to purchase historical flight data. 
input_dir <- glue("data/input/")
flight_points_dir <- glue("{input_dir}/fr_may23")
flight_points_df <- read_all_csv(flight_points_dir) %>%
  # some rows are repeated (maybe listed on multiple dats )
  unique()

positions_dirs <- list.dirs(path = "data/input/fr_may23", full.names = TRUE, recursive = F) 
all_positions_list <- lapply(positions_dirs, get_positions_csv)
all_positions_df <- bind_rows(all_positions_list)

merged_df <- all_positions_df %>%
  left_join(flight_points_df, by = "flight_id") %>%
  mutate(datetime = as.POSIXct(as.numeric(snapshot_id), tz = "UTC",
                               origin = "1970-01-01"))


# scraped positions
heli_position_df <- 
  merged_df %>%
  janitor::clean_names() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = CRS) %>%
  mutate(obs_id = row_number()) %>%
  select(
    callsign, #callsign
    reg, #starts_with("aircraft")
    speed, #ground_speed
    heading, #heading
    altitude, #altitude
    #vertical_rate, 
    squawk, #squawk_code
    flight_id, #unique_id
    datetime, # snapshot_id -> datetime
    obs_id # obs_id (from mutate above)
  ) %>%
  rename(
    hex_id = reg, # aircraft_registration
    alt_ft = altitude,
    timestamp = datetime, 
    unique_id = flight_id # doesn't exist in previous
  )

clean_helis(
  heli_position_df = heli_position_df,
  heliports_path = "data/input/heliports.csv",
  output_path = "data/output",
  WRITE_TO_RULE_TESTING_DIR = F
)


