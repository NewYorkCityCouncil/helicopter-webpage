# clean_helicopters

## CLEANING STEPS -----------------------------------------------

# - read in and clean data types

# - separate hexID points into transmission sessions 
#   based on TRANSMISSION_GAP_SEC threshold

# - remove hexID data for those that never cross into NYC

# - remove nuisance transmission sessions with less than
#   TRANSMISSION_SESSION_TOO_FEW_POINTS

# - remove transmission sessions that do not cross into NYC

# - clip transmission sessions to their entry and exit points from the bounding
#   box, if applicable

# - look at all intervals during which a helicopter was inside a heliport radius to 
#   determine entry and exit of helicopters into heliports. A couple criteria were used
#   in order to try and exclude brief excursions outside the heliport radius from counting
#   as entries and exits. Criteria include:

#   condition 1: If two adjacent intervals are in different heliports, then an
#   entry and an exit has occurred.

#   condition 2: If the two adjacent intervals are of the same heliport and
#   are separated by more than HELIPORT_ENTRY_EXIT_THRESHOLD_SEC amount of time, then
#   an entry and exit has occurred.

# - for each heliport interval measure the time spent in the heliport
#   as well as maximum decrease in altitude that occurred after first point 
#   inside heliport

# - filter out heliport intervals that last less than HELIPORT_DURATION_THRESHOLD_SEC
#   along with those that contained less than LANDING_ALT_REQ_MINMAX_DIFF altitude decrease. 
#   These sub-threshold intervals are considered as "not landing" flyovers.

# Assign transmission points to the nearest event that preceded them. 

# Split transmissions into flights based on the following logic:
#   A new flight begins when a transmission starts
#   OR
#   A heliport is landed at

# Flight points inside a heliport landing interval are assigned to one of the two
# adjacent flights based on the position of the point at lowest altitude or the mid point.

# Filter out flights that never cross into NYC.

## CONSTANTS -----------------------------------------------

SECONDS_PER_MINUTE <- 60

## CLEANING PARAMS -----------------------------------------------

# TRANSMISSION_GAP_SEC <-                15   * SECONDS_PER_MINUTE
# HELIPORT_ENTRY_EXIT_THRESHOLD_SEC <-   0.5  * SECONDS_PER_MINUTE
# HELIPORT_DURATION_THRESHOLD_SEC <-     1.5  * SECONDS_PER_MINUTE
# FLIGHT_DURATION_THRESHOLD_SEC <-       2.5  * SECONDS_PER_MINUTE
# 
# LANDING_ALT_REQ_MINMAX_DIFF <-         100
# 
# TRANSMISSION_SESSION_TOO_FEW_POINTS <- 10

##  NOTES -----------------------------------------------

# What about categorizing flights outside the bounding box as commercial 
# or tourism?

# We can roughly infer this if the flight, which is intersecting the NYC shapefile,
# enters and exits from the same side of the bounding box


# expand the bounding box of a set of shapes
expanded_bbox <- function(shapes){
  
  bbox_new <- st_bbox(shapes) # current bounding box
  
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  expand_fct <- 0.3 # set by trial and error
  bbox_new[1] <- bbox_new[1] - (expand_fct * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (expand_fct * xrange) # xmax - right
  bbox_new[2] <- bbox_new[2] - (expand_fct * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (expand_fct * yrange) # ymax - top
  
  return(bbox_new)
  
}

# assign each observation one of the session start or end times provided 
sessionize <- 
  function(
    obs_df, 
    session_df, 
    group_var,
    session_prefix, 
    using_session_end = F
  ){
  
    session_df <-
      session_df %>%
      select(
        !!group_var, 
        session_timestamp = timestamp,
        everything()
      ) %>%
      mutate(session_id = row_number()) %>%
      st_drop_geometry()
    
    obs_dt <- data.table(obs_df)
    session_start_dt <- data.table(session_df)
    
    setkeyv(obs_dt, cols = c(group_var, "timestamp"))
    setkeyv(session_start_dt, c(group_var, "session_timestamp"))
    
    # assign each observation to a session time
    # IF using_session_end == F, then session time indicates the start time and
    # the session start closest to, but not past, the observation time is assigned to the 
    # observation
    
    # IF using_session_end == T, then session time indicates the end time and 
    # the session end closest to, but not earlier than, the observation time is assigned to
    # the observation
    
    session_obs_match <- 
      session_start_dt[obs_dt, roll = ifelse(using_session_end, -Inf, Inf)] %>%
      select(session_id, obs_id)
    
    obs_wsession_df <- 
      obs_df %>%
      left_join(session_obs_match, by = "obs_id") %>%
      left_join(session_df %>% select(-c(!!group_var)), by = "session_id") %>% 
      rename(
        !!glue("{session_prefix}_id") := session_id,
        !!glue("{session_prefix}_timestamp") := session_timestamp
      ) %>%
      ungroup()
    
    return(obs_wsession_df)
    
}

## READ DATA -----------------------------------------------


# read in csv and clean here

#source("clean_airnav.R")
#source("clean_flight_radar.R")

clean_helis <- function(
  heli_position_df,
  heliports_path,
  output_path,
  TRANSMISSION_GAP_SEC              =   15   * SECONDS_PER_MINUTE,
  HELIPORT_ENTRY_EXIT_THRESHOLD_SEC =   0.5  * SECONDS_PER_MINUTE,
  HELIPORT_DURATION_THRESHOLD_SEC   =   1.5  * SECONDS_PER_MINUTE,
  FLIGHT_DURATION_THRESHOLD_SEC     =   2.5  * SECONDS_PER_MINUTE,
  LANDING_ALT_REQ_MINMAX_DIFF       =   100,
  TRANSMISSION_SESSION_TOO_FEW_POINTS = 10,
  WRITE_TO_RULE_TESTING_DIR = F,
  CREATE_EDA_PLOTS = T
){
  
  if (!all(c("hex_id", "alt_ft", "timestamp", "geometry") %in% colnames(heli_position_df))){
    error("heli_position_df must contain the following columns: hex_id, alt_ft, timestamp, geometry (sf)")
  }
  
  rule_str <- paste0(
    "TG-",
    TRANSMISSION_GAP_SEC,
    "_HEET-",
    HELIPORT_ENTRY_EXIT_THRESHOLD_SEC,
    "_HDT-",
    HELIPORT_DURATION_THRESHOLD_SEC,
    "_FDT-",
    FLIGHT_DURATION_THRESHOLD_SEC,
    "_LARMD-",
    LANDING_ALT_REQ_MINMAX_DIFF,
    "_NO-POS",
    "_TSTFP-",
    TRANSMISSION_SESSION_TOO_FEW_POINTS
  )
  
  # SETUP RULE TESTING DIR
  if (WRITE_TO_RULE_TESTING_DIR){
    output_path <- glue("data/output/rule-testing/{rule_str}")
  }
  dir.create(output_path, showWarnings = F, recursive = T)
  
  output_cache <- glue("{output_path}/cache")
  dir.create(output_cache, showWarnings = F, recursive = T)
  
  file.copy("code/01_clean_helicopters.R", glue("{output_cache}/01_clean_helicopters.R"))
  
  tibble(
    parameter = c(
      "TRANSMISSION_GAP_SEC",
      "HELIPORT_ENTRY_EXIT_THRESHOLD_SEC",
      "HELIPORT_DURATION_THRESHOLD_SEC",
      "FLIGHT_DURATION_THRESHOLD_SEC",
      "LANDING_ALT_REQ_MINMAX_DIFF",
      "TRANSMISSION_SESSION_TOO_FEW_POINTS"
    ),
    value = c(
      TRANSMISSION_GAP_SEC,
      HELIPORT_ENTRY_EXIT_THRESHOLD_SEC,
      HELIPORT_DURATION_THRESHOLD_SEC,
      FLIGHT_DURATION_THRESHOLD_SEC,
      LANDING_ALT_REQ_MINMAX_DIFF,
      TRANSMISSION_SESSION_TOO_FEW_POINTS    
    )
  ) %>%
    write_csv(glue("{output_cache}/param_settings.csv"))
  
  # LOAD SHAPES
  
  print("loading extra shapefiles ...")
  
  nyc_boro <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")
  nyc_boro <- nyc_boro %>% select(boro_name = BoroName)
  
  nyc_expanded_bbox_lims <- expanded_bbox(nyc_boro) 
  nyc_expanded_bbox_df <- 
    nyc_expanded_bbox_lims %>% 
    st_as_sfc() %>% 
    as.data.frame() %>%
    st_sf()
  
  nyc_boro_wshore <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary_Water_Included/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")
  nyc_boro_wshore <- nyc_boro_wshore %>% select(boro_name = BoroName)
  
  us_states <-
    unzip_sf("https://www2.census.gov/geo/tiger/TIGER2022/STATE/tl_2022_us_state.zip") %>%
    read_sf() %>%
    st_transform(CRS) %>%
    select(state = NAME, geometry)
  
  # heliport zones
  heliport_df <- 
    read_csv(heliports_path, show_col_types = F) %>%
    janitor::clean_names() %>%
    rename_with(.fn = function(x) glue("heliport_{x}"), .cols = -c("longitude", "latitude", "radius")) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = CRS) %>%
    mutate(
      geometry = st_buffer(geometry, dist = radius) 
    ) %>%
    select(-radius)
  
  # INITIAL FILTER AT HEX ID LEVEL
  
  # filter helis that have a hex_ID that at least cross into NYC boros (w shore)
  hexID_in_nyc <- 
    heli_position_df %>%
    st_filter(nyc_boro_wshore) %>%
    pull(hex_id) %>%
    unique()
  
  heli_position_df <- 
    heli_position_df %>% 
    filter(hex_id %in% hexID_in_nyc) %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2],
      bbox_edge = case_when(
        nyc_expanded_bbox_lims['ymin'] >= lat ~ "S",
        nyc_expanded_bbox_lims['ymax'] <= lat ~ "N",
        nyc_expanded_bbox_lims['xmin'] >= lon ~ "W",
        nyc_expanded_bbox_lims['xmax'] <= lon ~ "E",
        TRUE ~ NA
      )
    ) %>%
    select(-c(lon, lat)) %>%
    mutate(
      obs_id = row_number(),
      alt_ft = str_remove(alt_ft, "▼"),
      alt_ft = str_remove(alt_ft, "▲"),
      alt_ft = if_else(alt_ft == "ground", "0", alt_ft),
      alt_ft = if_else(alt_ft == "?", NA, alt_ft),
      alt_ft = as.numeric((alt_ft))
    ) %>%
    group_by(hex_id) %>%
    arrange(timestamp, .by_group = T) %>%
    mutate(
      time_diff_sec = time_length(timestamp - lag(timestamp), unit = "second"),
    ) %>%
    ungroup() %>%
    st_join(heliport_df) %>%
    st_join(us_states) 
  
  # GOAL is to separate the stream of points from a single hexID into
  # discrete flights
  
  ## TRANSMISSION SESSION -----------------------------------------------
  
  # FIRST give each position point a transmission session label
  # separate transmission sessions are almost certainly different flights
  # consider a session to start after a 5 minute gap in transmission
  
  print("breaking registration data points into transmission sessions ...")
  
  transmission_start_df <- 
    heli_position_df %>%
    group_by(hex_id) %>%
    arrange(timestamp, .by_group = T) %>%
    filter(is.na(time_diff_sec) | time_diff_sec >= TRANSMISSION_GAP_SEC) %>%
    select(hex_id, timestamp) %>%
    ungroup()
  
  heli_position_wtransmission_df <- 
    sessionize(
      obs_df = heli_position_df,
      session_df = transmission_start_df,
      group_var = "hex_id",
      session_prefix = "transmission"
    ) %>%
    rename(
      transmission_start_timestamp = transmission_timestamp
    )
  
  transmission_endpoints_df <-
    heli_position_wtransmission_df %>%
    st_drop_geometry() %>%
    group_by(transmission_id) %>%
    filter(row_number() == n()) %>%
    select(
      transmission_id, 
      transmission_end_timestamp = timestamp
    )
  
  heli_position_wtransmission_df <-
    heli_position_wtransmission_df %>%
    left_join(
      transmission_endpoints_df,
      by = "transmission_id"
    )
  
  # remove transmission sessions that do not cross into NYC area
  # and sessions that do not leave a heliport
  transmitID_in_nyc <- 
    heli_position_wtransmission_df %>%
    st_filter(nyc_boro_wshore) %>%
    st_drop_geometry() %>%
    group_by(transmission_id) %>%
    count() %>%
    filter(n > TRANSMISSION_SESSION_TOO_FEW_POINTS) %>%
    pull(transmission_id) %>%
    unique()
  
  remain_in_heliport_ID <- 
    heli_position_wtransmission_df %>%
    group_by(transmission_id) %>%
    summarise(remained = all(heliport_name == first(heliport_name, na_rm = T))) %>%
    filter(remained) %>%
    pull(transmission_id) %>%
    unique()
  
  nyc_heli_position_wtransmission_df <- 
    heli_position_wtransmission_df %>%
    filter(
      transmission_id %in% transmitID_in_nyc,
      !(transmission_id %in% remain_in_heliport_ID)
    ) %>%
    group_by(transmission_id) %>%
    arrange(timestamp, .by_group = T) %>%
    mutate(
      transmission_obs_id = row_number(),
      final_transmission_obs = row_number() == n(),
      transmission_time_diff_sec = time_length(timestamp - lag(timestamp), unit = "second")
    ) %>%
    ungroup()
  
  ## BBOX TRIM -----------------------------------------------
  
  # NEXT clip transmissions when they encounter the border box
  # to clip from the beginning, find the first point that enters NYC
  # then find the bbox point most directly preceding it
  # do the same for clipping the end
  
  print("trimming points outside the bounding box ...")
  
  first_nyc_timestamp_per_transmission_df <-
    nyc_heli_position_wtransmission_df %>%
    st_filter(nyc_boro_wshore) %>%
    st_drop_geometry() %>%
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    slice_head(n = 1) %>%
    select(
      transmission_id,
      first_nyc_timestamp = timestamp
    ) %>%
    ungroup()
  
  last_bbox_point_before_nyc_df <-
    nyc_heli_position_wtransmission_df %>%
    st_drop_geometry() %>%
    left_join(
      first_nyc_timestamp_per_transmission_df,
      by = "transmission_id"
    ) %>%
    filter(timestamp < first_nyc_timestamp, !is.na(bbox_edge)) %>%
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    slice_tail(n = 1) %>%
    select(
      transmission_id,
      last_bbox_timestamp_before_nyc = timestamp
    )
  
  last_nyc_timestamp_per_transmission_df <-
    nyc_heli_position_wtransmission_df %>%
    st_filter(nyc_boro_wshore) %>%
    st_drop_geometry() %>%
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    slice_tail(n = 1) %>%
    select(
      transmission_id,
      last_nyc_timestamp = timestamp
    )
  
  first_bbox_point_after_nyc_df <-
    nyc_heli_position_wtransmission_df %>%
    st_drop_geometry() %>%
    left_join(
      last_nyc_timestamp_per_transmission_df,
      by = "transmission_id"
    ) %>%
    filter(timestamp > last_nyc_timestamp, !is.na(bbox_edge)) %>% 
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    slice_head(n = 1) %>%
    select(
      transmission_id,
      first_bbox_timestamp_after_nyc = timestamp
    )
  
  nyc_heli_position_wtransmission_trim_df <-
    nyc_heli_position_wtransmission_df %>%
    left_join(
      last_bbox_point_before_nyc_df,
      by = "transmission_id"
    ) %>%
    left_join(
      first_bbox_point_after_nyc_df,
      by = "transmission_id"
    ) %>%
    filter(
      is.na(last_bbox_timestamp_before_nyc) | timestamp >= last_bbox_timestamp_before_nyc,
      is.na(first_bbox_timestamp_after_nyc) | timestamp <= first_bbox_timestamp_after_nyc
    ) %>%
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    mutate(
      transmission_start_timestamp = timestamp[1],
      transmission_end_timestamp = timestamp[n()]
    )
  
  # store transmission endpoint events
  
  trimmed_transmission_endpoint_events_df <- 
    nyc_heli_position_wtransmission_trim_df %>%
    st_drop_geometry() %>%
    group_by(transmission_id) %>%
    mutate(
      event = if_else(row_number() == 1, "transmission_start", "transmission_end")
    ) %>%
    filter(
      row_number() == 1 | row_number() == n()
    ) %>%
    select(
      transmission_id,
      transmission_obs_id,
      timestamp,
      event,
      heliport_name,
      bbox_edge,
      state
    )
  
  ## HELIPORT INTERVALS -----------------------------------------------
  
  # NEXT separate flights based on their crossings into heliport radii 
  
  # find sections of transmissions that intersect with heliports
  
  print("determing heliport intervals and landings ...")
  
  heliport_obs_df <- 
    nyc_heli_position_wtransmission_trim_df %>%
    st_drop_geometry() %>%
    filter(!is.na(heliport_name)) %>%
    group_by(transmission_id) %>%
    arrange(timestamp, .by_group = T) %>%
    mutate(
      lag_diff_sec = time_length(timestamp - lag(timestamp), unit = "second"),
      lead_diff_sec = time_length(lead(timestamp) - timestamp, unit = "second")
    ) %>%
    ungroup()
  
  # filter to keep points that indicate the entry of a helicopter into a heliport 
  # based on the following conditions:
  #    If a point inside a heliport exists directly prior to the current point and that point is
  #    inside a different heliport, then this is a new entry OR
  #    
  #    The last point inside the same heliport is NA or occurred less than 30 seconds ago AND
  #    it was NOT the point directly preceding the current one (this second condition is 
  #    needed because it prevents gaps in transmission from within a heliport as counting as 
  #    separate entry into the same heliport)
  
  enter_heliport_df <- 
    heliport_obs_df %>%
    group_by(transmission_id) %>%
    mutate(
      cond1 = !is.na(lag(heliport_name)) & lag(heliport_name) != heliport_name,
      cond2 = is.na(lag_diff_sec) | lag_diff_sec > HELIPORT_ENTRY_EXIT_THRESHOLD_SEC,
      cond3 = is.na(lag(transmission_obs_id)) | transmission_obs_id - lag(transmission_obs_id) != 1
    ) %>%
    filter(cond1 | (cond2 & cond3)) %>%
    select(
      transmission_id, 
      timestamp, 
      enter_heliport = heliport_name, 
      enter_transmission_obs_id = transmission_obs_id
    ) %>%
    ungroup()
  
  # apply the same conditions to determine heliport exits, except using lead data points
  
  exit_heliport_df <- 
    heliport_obs_df %>%
    group_by(transmission_id) %>%
    mutate(
      cond1 = !is.na(lead(heliport_name)) & lead(heliport_name) != heliport_name,
      cond2 = is.na(lead_diff_sec) | lead_diff_sec > HELIPORT_ENTRY_EXIT_THRESHOLD_SEC,
      cond3 = is.na(lead(transmission_obs_id)) | lead(transmission_obs_id) - transmission_obs_id != 1
    ) %>%
    filter(cond1 | (cond2 & cond3)) %>%
    select(
      transmission_id, 
      timestamp, 
      exit_heliport = heliport_name,
      exit_transmission_obs_id = transmission_obs_id, 
    ) %>%
    ungroup()
  
  # assign heliport points to a heliport session based on entry and exit times
  
  heliport_sessions_df <- 
    sessionize(
      obs_df = heliport_obs_df, 
      session_df = enter_heliport_df,
      group_var = "transmission_id",
      session_prefix = "enter_heliport"
    ) %>%
    sessionize(
      obs_df = .,
      session_df = exit_heliport_df,
      group_var = "transmission_id",
      session_prefix = "exit_heliport",
      using_session_end = T
    )
  
  # for each heliport session, measure the duration of the time spent in the heliport
  # and the maximum altitude decrease that occurs relative to the first point in the radius.
  
  heliport_session_stats_df <-
    heliport_sessions_df %>%
    st_drop_geometry() %>%
    mutate(
      secs_in_heliport = time_length(
        exit_heliport_timestamp - enter_heliport_timestamp, 
        unit = "second"
      )
    ) %>%
    group_by(enter_heliport_id) %>%
    mutate(
      min_alt_point = alt_ft == min(alt_ft, na.rm = T),
      mid_point = row_number() == ceiling(n()/2),
      sum_min_alt_point = if_else(all(is.na(alt_ft)), 0, sum(min_alt_point, na.rm = F)),
      first_min_alt_point = min_alt_point & !duplicated(min_alt_point),
      filter_point = case_when(
        sum_min_alt_point >= 1 & first_min_alt_point ~ TRUE,
        sum_min_alt_point == 0 & mid_point ~ TRUE,
        TRUE ~ FALSE
      ),
      max_alt_change = first(alt_ft, na_rm = T) - min(alt_ft, na.rm = T)
    ) %>%
    filter(filter_point) %>%
    ungroup()
  
  # use duration and altitude criteria to classify heliport landing
  
  endpoint_obs_ids <- 
    trimmed_transmission_endpoint_events_df %>%
    mutate(
      joint_id = glue("{transmission_id}_{transmission_obs_id}")
    ) %>%
    pull(joint_id)
  
  flight_start_events_df <- 
    heliport_session_stats_df %>%
    filter(
      (
        secs_in_heliport > HELIPORT_DURATION_THRESHOLD_SEC & 
          # NOTE: CHANGED FROM > to >= (likely needed b/c alt_ft is trimmed at 0)
          # max_alt_change >= LANDING_ALT_REQ_MINMAX_DIFF
          alt_ft <= 100
      ) &
        transmission_start_timestamp != enter_heliport_timestamp &
        transmission_end_timestamp != exit_heliport_timestamp
    ) %>%
    mutate(
      start_timestamp = case_when(
        transmission_start_timestamp == enter_heliport_timestamp ~ enter_heliport_timestamp,
        transmission_start_timestamp != enter_heliport_timestamp ~ timestamp
      ),
      start_transmission_obs_id = case_when(
        transmission_start_timestamp == enter_heliport_timestamp ~ enter_transmission_obs_id,
        transmission_start_timestamp != enter_heliport_timestamp ~ transmission_obs_id
      )
    ) %>%
    transmute(
      transmission_id,
      heliport_name,
      state,
      event = "flight_start",
      timestamp = start_timestamp,
      transmission_obs_id = start_transmission_obs_id,
    ) %>%
    mutate(
      joint_id = glue("{transmission_id}_{transmission_obs_id}")
    ) %>%
    filter(!(joint_id %in% endpoint_obs_ids)) %>%
    select(-joint_id)
  
  # combine events
  
  trimmed_transmission_events_df <- 
    bind_rows(
      trimmed_transmission_endpoint_events_df,
      flight_start_events_df
    ) %>%
    group_by(transmission_id) %>%
    arrange(transmission_id, timestamp, .by_group = T) %>%
    rename(
      event_transmission_obs_id = transmission_obs_id,
      event_heliport_name = heliport_name,
      event_bbox_edge = bbox_edge,
      event_state = state
    ) %>%
    ungroup() %>%
    mutate(
      event_num = row_number()
    )
  
  ## FLIGHTS -----------------------------------------------
  
  print("labeling flights ...")
  
  flight_events_df <-
    trimmed_transmission_events_df %>%
    group_by(transmission_id) %>%
    mutate(
      next_event_timestamp = lead(timestamp),
      next_event_bbox_edge = lead(event_bbox_edge),
      next_event_heliport_name = lead(event_heliport_name)
    ) %>%
    ungroup() %>%
    filter(event != "transmission_end") %>%
    transmute(
      transmission_id,
      event_num,
      flight_id = row_number(),
      flight_start_timestamp = timestamp,
      flight_start_event = event,
      flight_start_heliport_name = event_heliport_name,
      flight_start_bbox_edge = event_bbox_edge,
      flight_start_state = event_state,
      flight_end_timestamp = next_event_timestamp,
      flight_end_bbox_edge = next_event_bbox_edge,
      flight_end_heliport_name = next_event_heliport_name
    ) %>%
    mutate(flight_duration_secs = time_length(flight_end_timestamp - flight_start_timestamp))
  
  # match each observation with the event preceding it
  
  nyc_heli_flights_df <- 
    sessionize(
      obs_df = nyc_heli_position_wtransmission_trim_df, 
      session_df = trimmed_transmission_events_df %>% filter(event != "transmission_end"),
      group_var = "transmission_id",
      session_prefix = "event"
    ) %>%
    arrange(transmission_id, timestamp) %>%
    left_join(
      flight_events_df %>% select(event_num, flight_id, flight_duration_secs), 
      by = "event_num"
    ) %>%
    select(-event_num) %>%
    group_by(flight_id) %>%
    mutate(
      flight_obs_id = if_else(is.na(flight_id), NA, row_number())
    ) %>%
    ungroup()
  
  # remove flights that never cross into NYC
  
  nyc_heli_flight_ID <- 
    nyc_heli_flights_df %>%
    st_filter(nyc_boro_wshore) %>%
    pull(flight_id) %>%
    unique()
  
  nyc_heli_flights_df <- 
    nyc_heli_flights_df %>%
    filter(
      flight_id %in% nyc_heli_flight_ID
    )
  
  
  ## OUTPUTS -----------------------------------------------
  
  print("writing output ...")
  
  nyc_heli_flight_points_df <- 
    nyc_heli_flights_df %>%
    filter(
      flight_duration_secs > FLIGHT_DURATION_THRESHOLD_SEC
    ) %>%
    select(
      hex_id,
      callsign,
      starts_with("aircraft"),
      starts_with("squawk"),
      obs_id, 
      transmission_id,
      transmission_obs_id,
      flight_id,
      flight_obs_id,
      event_id,
      event,
      alt_ft,
      starts_with("speed"),
      heading,
      starts_with("vertical"),
      timestamp,
      bbox_edge,
      heliport_name,
      state,
      geometry
    )
  
  point_output_dir <- glue("{output_path}/flight_points")
  dir.create(point_output_dir, showWarnings = F, recursive = T)
  
  unique_dates <- ymd(unique(floor_date(nyc_heli_flight_points_df$timestamp, unit = "day")))
  for (i in 1:length(unique_dates)){
    nyc_heli_flight_points_df %>%
      filter(timestamp >= unique_dates[i], timestamp < unique_dates[i] + days(1)) %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry() %>%
      write_parquet(glue("{point_output_dir}/{unique_dates[i]}.parquet"))
  }
  
  
  # hex_id_summary_df <-
  #   nyc_heli_flights_df %>%
  #   st_drop_geometry() %>%
  #   group_by(hex_id) %>%
  #   summarize(
  #     hex_id = paste(unique(na.omit(hex_id)), collapse = ", "),
  #     callsign = paste(unique(na.omit(callsign)), collapse = ", "),
  #     n_unique_callsign = length(unique(na.omit(callsign))),
  #     registration = paste(unique(na.omit(registration)), collapse = ", "),
  #     n_unique_registration = length(unique(na.omit(registration))),
  #     type = paste(unique(na.omit(type)), collapse = ", "),
  #     n_type = length(unique(na.omit(type))),
  #     mil = paste(unique(na.omit(mil)), collapse = ", ")
  #   )
  # 
  # hex_id_summary_df %>% write_csv(glue("{output_path}/hexID_summary.csv"))
  # 
  # 
  transmission_summary_df <- 
    nyc_heli_flights_df %>%
    st_drop_geometry() %>%
    group_by(hex_id, transmission_id) %>%
    slice_head(n = 1) %>%
    transmute(
      hex_id,
      transmission_id,
      transmission_start_timestamp,
      transmission_end_timestamp,
      transmission_duration_secs = time_length(transmission_end_timestamp - transmission_start_timestamp)
    )
  
  transmission_summary_df %>% write_csv(glue("{output_path}/transmission_summary.csv"))
  
  
  transmission_events_df <- 
    trimmed_transmission_events_df %>%
    rename(
      event_id = event_num
    ) %>%
    left_join(
      transmission_summary_df %>% select(hex_id, transmission_id),
      by = "transmission_id"
    ) %>%
    select(
      hex_id, 
      transmission_id,
      event_id,
      event_transmission_obs_id,
      event,
      everything()
    )
  
  transmission_events_df %>% write_csv(glue("{output_path}/transmission_events.csv"))
  
  
  flight_summary_df <- 
    flight_events_df %>%
    left_join(
      transmission_summary_df %>% select(hex_id, transmission_id), 
      by = "transmission_id"
    ) %>%
    select(
      hex_id,
      everything()
    ) %>%
    filter(flight_duration_secs > FLIGHT_DURATION_THRESHOLD_SEC) %>%
    mutate(
      starts_at = case_when(
        !is.na(flight_start_heliport_name) ~ "heliport",
        !is.na(flight_start_bbox_edge) ~ "bbox",
        is.na(flight_start_bbox_edge) & is.na(flight_start_heliport_name) ~ "unknown"
      ),
      ends_at = case_when(
        !is.na(flight_end_heliport_name) ~ "heliport",
        !is.na(flight_end_bbox_edge) ~ "bbox",
        is.na(flight_end_bbox_edge) & is.na(flight_end_heliport_name) ~ "unknown"
      )
    ) %>%
    select(-event_num)
  
  flight_summary_df %>% write_csv(glue("{output_path}/flight_summary.csv"))
  
  
  flight_summary_df %>%
    count(starts_at, ends_at) %>%
    write_csv(glue("{output_path}/flight_endpoint_counts.csv"))
  
  
  ## QC ------------------------------------------------------
  
  if (WRITE_TO_RULE_TESTING_DIR){
    
    QC_dir <- glue("{output_path}/QC")
    dir.create(QC_dir, showWarnings = F)
    
    # copy analysis file 
    file.copy("code/other/QC.R", glue("{QC_dir}/QC.R"))
    file.copy("docs/QC_instructions.pdf", glue("{QC_dir}/QC_instructions.pdf"))
    
    # copy transmission points
    transmission_QC_dir <-glue("{QC_dir}/transmission_points")
    dir.create(transmission_QC_dir, showWarnings = F)
    
    transmission_unique_dates <- ymd(unique(floor_date(nyc_heli_position_wtransmission_df$timestamp, unit = "day")))
    for (i in 1:length(unique_dates)){
      nyc_heli_position_wtransmission_df %>%
        filter(timestamp >= transmission_unique_dates[i], timestamp < transmission_unique_dates[i] + days(1)) %>%
        mutate(
          lon = st_coordinates(.)[,1],
          lat = st_coordinates(.)[,2]
        ) %>%
        st_drop_geometry() %>%
        write_parquet(glue("{transmission_QC_dir}/{transmission_unique_dates[i]}.parquet"))
    }
    
    # sample flights
    sample_names <- c("NM", "CZ") 
    n_samples <- length(sample_names)
    flights_per_category <- 20
    
    n_longest <-        flights_per_category * n_samples
    n_shortest <-       flights_per_category * n_samples
    n_start_unknown <-  flights_per_category * n_samples
    n_end_unknown <-    flights_per_category * n_samples
    n_random <-         flights_per_category * n_samples
    
    longest_flights_sample_df <- 
      flight_summary_df %>%
      arrange(desc(flight_duration_secs)) %>%
      slice_head(n = n_longest) %>%
      mutate(
        sampled_as = "longest"
      )
    
    shortest_flights_sample_df <- 
      flight_summary_df %>%
      arrange(flight_duration_secs) %>%
      slice_head(n = n_shortest) %>%
      mutate(
        sampled_as = "shortest"
      )
    
    seen_flightIDs <- 
      c(
        longest_flights_sample_df$flight_id,
        shortest_flights_sample_df$flight_id
      )
    
    start_unknown_sample_df <- 
      flight_summary_df %>%
      filter(starts_at == "unknown", !(flight_id %in% seen_flightIDs)) %>%
      slice_sample(n = n_start_unknown) %>%
      mutate(
        sampled_as = "start_unknown"
      )
    
    seen_flightIDs <- 
      c(
        seen_flightIDs,
        start_unknown_sample_df$flight_id
      )
    
    end_unknown_sample_df <- 
      flight_summary_df %>%
      filter(ends_at == "unknown", !(flight_id %in% seen_flightIDs)) %>%
      slice_sample(n = n_end_unknown) %>%
      mutate(
        sampled_as = "end_unknown"
      )
    
    seen_flightIDs <- 
      c(
        seen_flightIDs,
        end_unknown_sample_df$flight_id
      )
    
    random_sample_df <-
      flight_summary_df %>%
      filter(!(flight_id %in% seen_flightIDs)) %>%
      slice_sample(n = n_random) %>% 
      mutate(
        sampled_as = "random"
      ) 
    
    for (i in 1:n_samples){
      
      sample_name <- sample_names[i]
      sample_rows_start <- 1 + ((i-1)/n_samples) * flights_per_category * n_samples
      sample_rows_end <- (i/n_samples) * flights_per_category * n_samples
      sample_row_idx <- sample_rows_start:sample_rows_end
      
      sample_df <- 
        bind_rows(
          longest_flights_sample_df %>% slice(sample_row_idx),
          shortest_flights_sample_df %>% slice(sample_row_idx),
          start_unknown_sample_df %>% slice(sample_row_idx),
          end_unknown_sample_df %>% slice(sample_row_idx),
          random_sample_df %>% slice(sample_row_idx)
        ) %>%
        mutate(notes = "")
      
      sample_df %>% write_csv(glue("{QC_dir}/{sample_name}.csv"))
      
    }
    
  }
  

  ## EDA plots -----------------------------------------------
  
  if (CREATE_EDA_PLOTS){
  
    EDA_dir <- glue("{output_path}/EDA")
    dir.create(EDA_dir, showWarnings = F)
    
    # hexID lifespan by first appearance
    
    unique_hexID <- length(unique(nyc_heli_position_wtransmission_df$hex_id))
    
    hexID_first_timestamp_df <-
      nyc_heli_position_wtransmission_df %>%
      st_drop_geometry() %>%
      group_by(hex_id) %>%
      arrange(timestamp, .by_group = T) %>%
      slice(1) %>%
      select(hex_id, first_timestamp = timestamp)
    
    hexID_ts_df <-
      nyc_heli_position_wtransmission_df %>%
      st_drop_geometry() %>%
      left_join(hexID_first_timestamp_df, by = "hex_id")
    
    hexID_ts_df$hex_id <- factor(hexID_ts_df$hex_id)
    
    hexID_lifespan_by_appearance_plot <-
      hexID_ts_df %>%
      mutate(hex_id = fct_reorder(hex_id, first_timestamp)) %>%
      ggplot() +
      geom_tile(aes(x = timestamp, y = hex_id)) +
      theme_light() +
      theme(
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
      labs(
        title = "Marks are data points recevied for Hex IDs",
        subtitle = glue("Hex IDs sorted in order of first recorded timestamp [{unique_hexID} unique hex IDs]")
      )
    
    ggsave(glue("{EDA_dir}/hexID_lifespan_by_appearance.png"), hexID_lifespan_by_appearance_plot)
    
    
    # hexID lifespan by total data points
    
    unique_hexID <- length(unique(nyc_heli_position_wtransmission_df$hex_id))
    
    hexID_count_df <-
      nyc_heli_position_wtransmission_df %>%
      st_drop_geometry() %>%
      group_by(hex_id) %>%
      summarize(n_points = n())
    
    hexID_ts_df <-
      nyc_heli_position_wtransmission_df %>%
      st_drop_geometry() %>%
      left_join(hexID_count_df, by = "hex_id")
    
    hexID_ts_df$hex_id <- factor(hexID_ts_df$hex_id)
    
    hexID_lifespan_by_total_points_plot <-
      hexID_ts_df %>%
      mutate(hex_id = fct_reorder(hex_id, -n_points)) %>%
      ggplot() +
      geom_tile(aes(x = timestamp, y = hex_id)) +
      theme_light() +
      theme(
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
      labs(
        title = "Marks are data points recevied for Hex IDs",
        subtitle = glue("Hex IDs sorted in order of total data points [{unique_hexID} unique hex IDs]")
      )
    
    ggsave(glue("{EDA_dir}/hexID_lifespan_by_total_points.png"), hexID_lifespan_by_total_points_plot)
    
  }
  
}
