library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(forcats)
library(lubridate)
library(gt)
library(maptiles)
library(terra)

# Open neighbourhoods get the neighbourhood for each station
n = st_read("./Neighbourhood_Boundaries/Neighbourhood_Boundaries.shp")
n = sf::st_transform(n, crs = "epsg:4326")

#assign each parking zone to a neighbourhood
stn_loc <- read.csv("victoria_ebike_zones.csv", stringsAsFactors = F)
stns = st_as_sf(stn_loc, coords = c("longitude","latitude"), crs = "epsg:4326")
stnn = st_intersection(stns, n)
stnn = stnn %>% rename(station_name = zone_name)
stnn <- stnn %>% select(station_name, Neighbourh) %>% st_drop_geometry()

#load the events log csv file from the Lime Squeezer
events <- read.csv('lime_victoria_zone_events.csv')

#make the time stamp a date time object
events$timestamp <- as.POSIXct(events$timestamp)

#trip per day by counting 'taken_out'
events$date <- lubridate::floor_date(events$timestamp, unit = "day")

#time series of the trip data with a count per day
trip_ts <- events %>% filter(event == "taken_out") %>% group_by(date) %>%
  summarize(total_trips = n())
trip_ts$date <- as.Date(trip_ts$date)

#remove start and end partial days. Mark weekends
trip_ts %>% filter(!(date %in% c('2026-09-01', "2026-09-22"))) %>% 
  mutate(weekend = if_else(date %in% c("2026-09-07", "2026-09-06", "2026-09-05",
                                       "2026-09-12", "2026-09-13", "2026-09-19",
                                       "2026-09-20"), TRUE, FALSE)) %>%
  ggplot(aes(date,total_trips, fill = weekend)) +
  geom_col() +
  labs(title = "Victoria Lime Trips by Day",
       y = "Number of Trips",
       x = "",
       caption = '@jeremy77.bsky.social\n Data Source: Lime Canada GBFS'
       ) +
  scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "grey70"), labels = c("Weekend","")) + 
  theme_bw() +
  theme(legend.position = "none")

#Busiest Stations as a nice table
busy_stns <- events %>% filter(!(date %in% c('2026-09-01', '2026-09-09'))) %>%
  filter(event == 'taken_out') %>% group_by(station_name) %>%
  summarize(trips_taken = n())

busy_stns <- busy_stns[order(busy_stns$trips_taken, decreasing = T),]
  
gt(busy_stns[seq(1,10),]) %>%
  tab_header(title = md("**Top 10 Most Used Victoria Bikeshare Stations**"),
             subtitle = "September 2 - 21, 2026") %>%
  cols_label(station_name = "Station Name", trips_taken = "Trips") %>%
  tab_footnote(footnote = 'Prepared by @jeremy77.bsky.social') %>%
  tab_footnote(footnote = 'Data Source: Lime Canada GBFS')

#all stations on graph with colours for hood
busy_stns <- merge(busy_stns, stnn, by.x = 'station_name', by.y = 'station_name')

busy_stns %>% filter(trips_taken > 60) %>%
  mutate(station_name = fct_reorder(station_name, trips_taken)) %>% 
ggplot(aes(x = station_name, trips_taken, fill = Neighbourh)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  scale_y_continuous(expand = c(0,0), name = "# trips")

#Map
bbox <- st_bbox(stns)
buffer_dist = 0.005
bbox["xmin"] <- bbox["xmin"] - buffer_dist
bbox["ymin"] <- bbox["ymin"] - buffer_dist
bbox["xmax"] <- bbox["xmax"] + buffer_dist
bbox["ymax"] <- bbox["ymax"] + buffer_dist

basemap <- get_tiles(bbox, provider = "Esri.WorldGrayCanvas", crop = TRUE, zoom=13,
                     retina = T, apikey = 'cb1_3gno_1_b7f3c4c195152df223224935')

g_map <- stn_loc %>% 
  rename(station_name = zone_name) %>% 
  full_join(busy_stns, by = "station_name") %>% 
  arrange(trips_taken) %>%  
  ggplot() +
  geom_spatraster_rgb(data = basemap) +
  #geom_sf(data = n) +
  #geom_point(data = stn_loc, aes(x = longitude, y = latitude), shape = 21) +
  geom_point(aes(size = trips_taken, colour = trips_taken, x = longitude, y = latitude)) +
  scale_size_continuous(range = c(0.5, 8)) +
  scale_colour_viridis_c(alpha = 0.8) +
  theme_bw() +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = 'Bike Share Use by Station') +
  theme(legend.position = "none")

ggsave("sep22_map.png", g_map)

#time of day use plot
events$hour <- lubridate::hour(events$timestamp)

events %>% filter(event == "taken_out") %>% group_by(hour) %>% 
  summarize(trips_per_hour = n()/5) %>%
  ggplot(aes(x = hour, y = trips_per_hour)) +
  geom_col() +
  labs(title = 'Lime Bike use by Hour of the Day',
       x = "Hour of the Day",
       y = "Average Number of Trips per Hour") +
  theme_bw()

events$date <- as.Date(events$date)

events <- events %>% mutate(weekend = if_else(date %in% c("2026-09-07", "2026-09-06", "2026-09-05",
                                               "2026-09-12", "2026-09-13", "2026-09-19",
                                               "2026-09-20"), TRUE, FALSE))
events %>% filter(event == "taken_out", weekend == F) %>% group_by(hour) %>% 
  summarize(trips_per_hour = n()/8) %>%
  ggplot(aes(x = hour, y = trips_per_hour)) +
  geom_col() +
  labs(title = 'Weekday Lime Bike use by Hour of the Day',
       x = "Hour of the Day",
       y = "Average Number of Trips per Hour") +
  theme_bw()


events %>% filter(event == "taken_out") %>% 
  ggplot(aes(x = hour, fill = weekend)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = 'identity',
                 alpha = 0.5,
                 bins = 24) +
  labs(title = 'Victoria Bike Share use by Hour of the Day',
       x = "Hour of the Day",
       y = "Density",
       fill = "",
       caption = '@jeremy77.bsky.social\n Data Source: Sept. 2 - 21, 2026, Lime Canada GBFS') +
  scale_fill_manual(values = c("#69b3a2", "#404080"), 
                    labels = c("Weekday", "Weekend")) +
  scale_x_continuous(breaks = seq(0,24,2), expand = c(0,0)) +
  annotate(
    geom = "text", 
    x = 3.5, 
    y = 0.053, 
    label = "Morning Rush"
  ) +
  annotate(
    'curve',
    x = 3.3, # Play around with the coordinates until you're satisfied
    y = 0.05,
    yend = 0.03,
    xend = 7.2,
    linewidth = 1,
    curvature = 0.5,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  annotate(
    geom = "text", 
    x = 20.5, 
    y = 0.09, 
    label = "Evening Rush"
  ) +
  annotate(
    'curve',
    x = 22, # Play around with the coordinates until you're satisfied
    y = 0.087,
    yend = 0.08,
    xend = 18.8,
    linewidth = 1,
    curvature = -0.5,
    arrow = arrow(length = unit(0.5, 'cm'))
  ) +
  theme_bw()

#fin


#number of bikes at a station
num_now = 2
rich_at_cook <- events %>% filter(station_name == "Cook St. & Richardson St.")
rich_at_cook <- dplyr::arrange(rich_at_cook, desc(timestamp))
rich_at_cook <- rich_at_cook %>% mutate(change = if_else(event == "taken_out", 1, -1))
rich_at_cook$bikes_available <- cumsum(rich_at_cook$change) + num_now

rich_at_cook %>% ggplot(aes(x = timestamp, y = bikes_available)) + geom_point() + geom_line()

events %>% group_by(date, event) %>% summarize(count_trans = n()) %>%
  pivot_wider(names_from = event, values_from = count_trans) %>% 
  mutate(net_daily = returned - taken_out)


busy_stns <- events %>% group_by(station_name) %>% summarize(num_events = n())

busy_stns = busy_stns %>% left_join(stnn, by ="station_name")
busy_stns$Neighbourh[busy_stns$Neighbourh == "Victoria West"] = "Vic West"
busy_stns %>% 
  filter(num_events > 6) %>% 
  mutate(station_name = fct_reorder(station_name, num_events)) %>% 
  ggplot(aes(x = num_events, y = station_name)) +
  # geom_col(aes(fill = as.factor(num_events))) +
  geom_col(aes(fill = Neighbourh)) +
  theme_bw() +
  theme(legend.position = "top",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(expand = c(0,0), name = "# events") +
  ylab(NULL) +
  labs(fill = "Neighbourhood")

events %>% 
  mutate(timestamp = as.POSIXct(timestamp),
         date = as.Date(timestamp), 
         timehour = hour(timestamp)) %>% 
  group_by(date, timehour) %>% 
  summarize(events = n()) %>% 
  ggplot(aes(x = timehour, y = events)) +
  geom_col()

most_bike_withdrawl <- events %>% filter(event == "taken_out") %>% group_by(station_name) %>%
  summarize(num_withdrawl = n())

most_bike_return <- events %>% filter(event == "returned") %>% group_by(station_name) %>%
  summarize(num_returned = n())

#map
stn_loc <- read.csv('victoria_ebike_zones.csv', stringsAsFactors = F)

#merge stn_loc in and drop bike loc
events <- merge(events, stn_loc, by.x = 'station_name', by.y = 'zone_name')

events_sf <- st_as_sf(events, coords = c("longitude", "latitude"), crs = 4326)

events_sf <- events_sf %>% group_by(station_name, event) %>% summarize(num = n())

#pivot wider
events_sf <- events_sf %>% pivot_wider(names_from = event, values_from = num, values_fill = 0)

events_sf$total <- events_sf$returned + events_sf$taken_out

coords <- st_coordinates(events_sf)
events_sf <- events_sf %>%
  mutate(x = coords[, 1], y = coords[, 2])

ggplot() +
  geom_sf(data = events_sf, size = 2, color = "black") +
  # returned - green, up arrow, offset above the point
  geom_text(
    data = events_sf,
    aes(x = x, y = y + 0.001, label = paste0("▲ ", returned)),
    color = "forestgreen",
    fontface = "bold",
    size = 3.5
  ) +
  # taken_out - red, down arrow, offset below the point
  geom_text(
    data = events_sf,
    aes(x = x, y = y - 0.001, label = paste0("▼ ", taken_out)),
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  theme_minimal() +
  labs(title = "Bike/Item Movement by Location")


library(mapview)

mapview(events_sf, zcol = 'net')

events_sf$net <- events_sf$returned - events_sf$taken_out
