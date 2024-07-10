# Summary stats of flights by type and heliport

## Load Dependencies -----------------------------------------------

# only need to source if haven't run it yet in session
source("code/analysis/01_wrangle_flights.R")

# number of flights to and from each heliport / box
x <- flights %>%
  st_drop_geometry() %>%
  group_by(flight_start, flight_type) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(num_flights), .by_group=TRUE)

start_end_counts <- x %>%
  left_join(
    x %>% 
      group_by(flight_start) %>%
      summarise(total = sum(num_flights))
  ) %>%
  # proportion of flight start/end pair that make up total flights started from location
  mutate(prop = num_flights / total)

start_end_counts %>%
  group_by(flight_type) %>%
  summarise(n = sum(num_flights))

# overall summary stats --------------------------------------------------

# total flights
flights %>% nrow()

# total flights by type
flights %>%
  st_drop_geometry() %>%
  group_by(flight_type) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(flight_type)) %>%
  mutate(
    prop = num_flights / nrow(flights)
  )

# tours --------------------------------------------------

# number of tour flights from each location 
tour_counts <- start_end_counts %>%
  filter(flight_start == "Downtown Manhattan Heliport" |
           flight_start == "Kearny, NJ" | 
           flight_start == "Linden Airport", 
         flight_type == "tour")

`Car` <- c("20 min", "25 min", "40 min")
`Public` <- c("25 min", "50 min*", "60 min")

# tours by heliport
t1 <- tour_counts %>%
  ungroup() %>%
  select(flight_start, num_flights) %>% 
  arrange(desc(num_flights)) %>%
  rename(
    Heliport = flight_start,
    `Number of Tours` = num_flights
    ) %>%
#  cbind(., `Car`, `Public`) %>%
  gt() %>%
  tab_header(title = "Tour Flights and Travel Times by Heliport") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t1 %>% gtsave("visuals/summary_stats/heliport_tours_breakdown.png")
t1 %>% gtsave("visuals/summary_stats/heliport_tours_breakdown.html")

# nyc vs nj tour counts
t2 <- tour_counts %>%
  mutate(
    Geo = case_when(
      flight_start == "Downtown Manhattan Heliport" ~ "NYC",
      TRUE ~ "NJ"
    )
  ) %>%
  group_by(Geo) %>%
  summarise(n = sum(num_flights)) %>% 
  arrange(desc(n)) %>% 
  rename(
    `Number of Tours` = n
    ) %>%
  gt() %>%
  tab_header(title = "NYC vs NJ Tour Flights") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t2 %>% gtsave("visuals/summary_stats/nyc_vs_nj_tours.png")
t2 %>% gtsave("visuals/summary_stats/nyc_vs_nj_tours.html")

# flight counts and proportions for select heliports --------------------------------------------------
# total flights and proportions by start and type
t3 <- flights %>%
  st_drop_geometry() %>%
  group_by(flight_start, flight_type) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(flight_type)) %>%
  mutate(
    prop = round(num_flights / nrow(flights), 2)
  ) %>%
  arrange(desc(num_flights)) %>% 
  head(7) %>%
  rename(
    `Start Location` = flight_start,
    `Flight Type` = flight_type,
    `Number of Flights` = num_flights,
    `Proportion of Total Flights` = prop
  ) %>%
  ungroup() %>%
  gt() %>%
  tab_header(title = "Flight Numbers by Start Location and Flight Type") %>%
  tab_style(style = cell_text(color = "#777777"),
            locations = cells_column_labels()) %>%
  tab_options(column_labels.font.weight = "bolder", 
              table.margin.left = 0, 
              table.margin.right = 0) %>%
  gt_theme_nytimes() 

t3 %>% gtsave("visuals/summary_stats/select_flight_counts_prop.png")
t3 %>% gtsave("visuals/summary_stats/select_flight_counts_prop.html")


# flight type for select heliports and total --------------------------------------------------

start_end_counts %>%
  filter(!is.na(flight_start), 
         !is.na(flight_type))

start_end_counts %>%
  filter(flight_start == "Downtown Manhattan Heliport" |
           flight_start == "East 34th Street Heliport" |
           flight_start == "West 30th Street Heliport")

# median daily tour flights --------------------------------------------------

y <- flights %>%
  st_drop_geometry() %>%
  mutate(
    date = as.Date(flight_start_timestamp)
  ) %>%
  group_by(flight_start, date, flight_type) %>%
  summarise(num_flights = n()) %>%
  arrange(desc(num_flights), .by_group=TRUE)

# downtown manhattan heliport
avg_flights_dmh <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "Downtown Manhattan Heliport", 
    flight_type == "tour", 
    day != "Sunday")
quantile(avg_flights_dmh$num_flights)
  
# kearny, nj
avg_flights_knj <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "Kearny, NJ", 
    flight_type == "tour")
quantile(avg_flights_knj$num_flights)

# linden, nj
avg_flights_lnj <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "Linden Airport", 
    flight_type == "tour")
quantile(avg_flights_lnj$num_flights)

# median daily non-tour flights --------------------------------------------------

# East 34th Street Heliport
avg_flights_meh <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "East 34th Street Heliport", 
    flight_type != "tour")
quantile(avg_flights_meh$num_flights)

# West 30th Street Heliport
avg_flights_mwh <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "West 30th Street Heliport", 
    flight_type != "tour")
quantile(avg_flights_mwh$num_flights)

 # NYPD Bennett Field
avg_flights_nypd <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "NYPD Floyd Bennett Field")
quantile(avg_flights_nypd$num_flights)

# JFK airport
avg_flights_jfk <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "JFK Airport", 
    flight_type != "tour")
quantile(avg_flights_jfk$num_flights)

# Newark airport
avg_flights_new <- y %>%
  mutate(day = weekdays(date)) %>%
  filter(
    flight_start == "Newark Airport", 
    flight_type != "tour")
quantile(avg_flights_new$num_flights)

# median daily flights (all) --------------------------------------------------

avg_flights <- flights %>%
  st_drop_geometry() %>%
  mutate(
    date = as.Date(flight_start_timestamp)
  ) %>%
  group_by(date) %>%
  summarise(num_flights = n())
quantile(avg_flights$num_flights)
