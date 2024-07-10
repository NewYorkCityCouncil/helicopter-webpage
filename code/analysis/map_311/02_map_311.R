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

## All Complaints Map -----------------------------------------------
all_complaints_map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15)) %>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df,
                       fill = FALSE,
                       opacity = 0.8,
                       color = "#666666") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.05, color = "red", radius = 1, fill = FALSE)
councildown::mapshot(all_complaints_map, file = "visuals/311_flights_together/all_complaints_map.png", 
                     vwidth = 900, vheight = 870)
htmlwidgets::saveWidget(all_complaints_map, file="visuals/311_flights_together/all_complaints_map.html", selfcontained = T)


## All Complaints w/ All Flights Map -----------------------------------------------
all_complaints_flights_map <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15)) %>%
  setView(-73.999,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  leaflet::addPolygons(data = heliport_df %>% filter(!(name %in% c("Hackensack Univ Medical Center", "Staten Island Univ Hospital"))),
                       fill = FALSE,
                       opacity = 0.8,
                       color = "#666666") %>%
  addCircleMarkers(data=heli_noise, 
                   opacity = 0.05, color = "red", stroke = F, radius = 2, fill = T) %>%
  addPolylines(data=flights, opacity = 0.03, color = "#2F56A6") %>%
  addLegend("bottomright", 
            colors = c("#d7e1f4", "#9cb3e3", "#6086d2", "#3868c7", "#2F56A6"),
            labels = c("1", "35", "75", "150", "225"),
            title = "Number of Flights</br>May 15, 2023",
            opacity = 1)
councildown::mapshot(all_complaints_flights_map, file = "visuals/311_flights_together/all_complaints_flights_map.png", 
                     vwidth = 900, vheight = 870)
htmlwidgets::saveWidget(all_complaints_flights_map, file="visuals/311_flights_together/all_complaints_flights_map.html", selfcontained = T)

## Census Tract Map -----------------------------------------------

# download census tract shapefile from Department of Planning
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2020_22a.zip"
ct_shp <- sf::read_sf(unzip_sf(url)) %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 

# mark each noise complaint with the census tract it is in
ct_noise_points <- st_join(heli_noise, ct_shp)

# total noise complaints in each census tract
ct_totals <- ct_noise_points %>%
  st_drop_geometry() %>%
  group_by(BoroCT2020) %>%
  summarise(total = n()) %>%
  right_join(ct_shp %>% select(BoroCT2020, NTAName), by = "BoroCT2020") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 

# map noise complaints by census tract (Jan 2022 - Present [June 2023])

quantile(ct_totals$total, probs = seq(0, 1, 0.01), na.rm = TRUE)
nat_intvl_ct = classInt::classIntervals(ct_totals$total, n = 5, style = 'fisher')

pal_ct = colorBin(
  palette ="nycc_blue",
  bins = c(0, 9, 22, 50, 295, 18500),
  domain = ct_totals$total, 
  na.color = "White"
)

map <-leaflet(ct_totals) %>% 
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_ct(total),
              fillOpacity = 0.9) %>%
  addCircleMarkers(data = heliport_point, color = "red", radius = 1) %>%
  addLegend(position ="bottomright", 
            pal = pal_ct, 
            opacity = 0.9,
            values = ct_totals$total,
            title =  "Helicopter Noise<br>Complaints (311)")

dir <- glue("visuals/311_complaints/all_311_complaints.png")
councildown::mapshot(map, file = dir, 
                     vwidth = 900, vheight = 870)


## group by NTA to get less granular picture  -----------------------------------------------

nta_noise <- ct_noise_points %>%
  st_drop_geometry() %>%
  group_by(BoroCT2020) %>%
  summarise(total = n()) %>%
  right_join(ct_shp %>% select(BoroCT2020, NTAName), by = "BoroCT2020") %>%
  group_by(NTAName) %>%
  summarise(total_nta = sum(total, na.rm = TRUE))

## compare May 2023 to years 2021-2023

# MAY 2023
may_heli <- heli_noise %>%
  filter(created_date < "2023-06-01", created_date >= "2023-05-01")
may_ct_noise_points <- st_join(may_heli, ct_shp)
# total noise complaints in each census tract
n_may = nrow(may_ct_noise_points)
may_ct_totals <- may_ct_noise_points %>%
  st_drop_geometry() %>%
  group_by(BoroCT2020) %>%
  summarise(total = n(), 
            perc = total / n_may)

# NON-MAY 2023
nonmay_heli <- heli_noise %>%
  filter(!(created_date < "2023-06-01" & created_date >= "2023-05-01"))
nonmay_ct_noise_points <- st_join(nonmay_heli, ct_shp)
# total noise complaints in each census tract
n_nonmay = nrow(nonmay_ct_noise_points)
nonmay_ct_totals <- nonmay_ct_noise_points %>%
  st_drop_geometry() %>%
  group_by(BoroCT2020) %>%
  summarise(total = n(), 
            perc = total / n_nonmay)

perc_change <- nonmay_ct_totals %>%
  full_join(may_ct_totals, by = "BoroCT2020") %>%
  mutate(
    perc.x = ifelse(is.na(perc.x), 0, perc.x), 
    perc.y = ifelse(is.na(perc.y), 0, perc.y), 
    perc_change = perc.x - perc.y
  ) %>%
  rename(
    total_nonmay = total.x, 
    total_may = total.y, 
    perc_nonmay = perc.x, 
    perc_may = perc.y, 
  )

ggplot(data = perc_change, aes(x = perc_nonmay, y = perc_may, size = total_nonmay)) + geom_point() + geom_abline(intercept = 0, slope = 1)

ggplot(data = perc_change, aes(x = perc_change)) + geom_histogram(binwidth = 0.001)

perc_change_sf <- perc_change %>%
  right_join(ct_shp %>% select(BoroCT2020, NTAName), by = "BoroCT2020") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 


quantile(perc_change_sf$perc_change, probs = seq(0, 1, 0.05), na.rm = TRUE)
nat_intvl_ct = classInt::classIntervals(perc_change_sf$perc_change, n = 5, style = 'fisher')

pal_ct = colorBin(
  palette = "diverging",
  bins = nat_intvl_ct$brks,
  domain = perc_change_sf$perc_change, 
  na.color = "White"
)

map <-leaflet(perc_change_sf) %>% 
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_ct(perc_change),
              fillOpacity = 0.9) %>%
  addCircleMarkers(data = heliport_point, color = "red", radius = 1) %>%
  addLegend_decreasing(position ="bottomright", 
            pal = pal_ct, 
            opacity = 0.9,
            values = perc_change_sf$perc_change,
            decreasing = T,
            title =  "Change in Noise<br>Complaints (311)<br>>0 = More in May 2023")


### plot over time -----------------------------------------------
# get helicopter noise complaints
heli_noise_all <- raw %>%
  mutate(
    month = month(created_date), 
    year = year(created_date)
  ) 

heli_noise_all %>%
  group_by(year, month) %>%
  summarise(n = n())

heli_noise_all %>%
  group_by(year) %>%
  summarise(n = n())
