library(data.table)
library(dplyr)
library(useful)
library(ggplot2)
library(lubridate)
library(quantreg)
library(splines)
library(RSQLite)
library(dbplyr)
library(matrixStats)
library(stringi)
library(tidytransit)
library(ggmap)
library(sf)
library(lwgeom)
library(rgeos)
library(scales)

date_tar <- as.Date("2020-02-25") #for a list of dates usable, run lines 1-28 and enter View(dat$.$dat_service_table) into the command line
route_tar <- "PAO" #Use route codes, not the full route names
dir_tar <- c(1,0) #Select one or both directions to view
time_start <- period_to_seconds(hms("10H 0M 0S")) 
time_end <- period_to_seconds(hms("20H 0M 0S"))

`%notin%` = Negate(`%in%`)

dat <- read_gtfs("C:/..../gtfs (2).zip") #this should be the path to your GTFS file
dat <- set_servicepattern(dat)

svc <- dat$.$date_service_table %>%
  filter(date == date_tar)

dat$routes <- dat$routes %>%
  filter(route_id %in% route_tar)

dat$trips <- dat$trips %>%
  filter(route_id %in% dat$routes$route_id) %>%
  filter(service_id %in% svc$service_id)%>%
  filter(direction_id %in% dir_tar)

dat$stop_times <- dat$stop_times %>%
  filter(trip_id %in% dat$trips$trip_id)

dat$stops <- dat$stops %>%
  filter(stop_id %in% dat$stop_times$stop_id)%>%
  filter(stop_id %notin% c("90007", "90008", "90006", "90005")) #include only the first Center City station for the route. Entry points are 90406 (Penn Medicine), 90004 (30 St), 90007 (Temple). Delete whichever is appropriate; add back/leave the rest.

list2env(dat,.GlobalEnv)

routes_sf <- get_route_geometry(gtfs_as_sf(dat))%>%
  st_transform(., " +proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=41.0333333333333 +lat_2=40.6666666666667 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-mi +no_defs")
stops_sf <- stops_as_sf(dat$stops)%>%
  st_transform(., " +proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=41.0333333333333 +lat_2=40.6666666666667 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-mi +no_defs")

stops_sf <- stops_sf %>%
  st_snap(., routes_sf, .001)%>%
  mutate(dist = gProject(as(routes_sf,Class = "Spatial"), as(stops_sf, Class = "Spatial")))%>%
  mutate(dist = dist-min(dist))

df <- stop_times %>%
  mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
  filter(arrival_time > time_start & arrival_time <= time_end)%>%
  inner_join(.,stops_sf %>% st_set_geometry(NULL) %>% select(stop_id, stop_name, dist))%>%
  inner_join(.,trips %>% select(trip_id))%>%
  mutate(arrival_time_t = as.POSIXct(arrival_time, origin = "1970-01-01 00:00:00", tz= "America/New_York"))

key <- stop_times %>%
  filter(trip_id %in% df$trip_id)%>%
  group_by(trip_id)%>%
  summarise(stop_sequence = max(stop_sequence))

df2 <- inner_join(stop_times, stops %>% select(stop_id, stop_name))%>%
  inner_join(.,key, by = c("trip_id", "stop_sequence"))%>%
  select(trip_id, headsign = stop_name)%>%
  mutate(headsign = ifelse(headsign %in% c("30th Street Station", 
                                           "Suburban Station", 
                                           "Jefferson", 
                                           "Temple University", 
                                           "Penn Medicine"), "Center City Philadelphia", headsign)) #This replaces Center City headsigns with a single headsign.

df <- df %>%
  inner_join(.,df2)

route_tar_name <- routes %>%
  pull(route_long_name)

ggplot(df, aes(x = arrival_time_t, y = dist, group = trip_id))+
  geom_line(aes(color = headsign), lwd = 1.5)+
  scale_color_brewer(palette = "Dark2", name = "Destination")+
  geom_point(lwd = 2)+
  scale_y_continuous(breaks = stops_sf$dist, labels = stops_sf$stop_name, minor_breaks = NULL)+
  scale_x_datetime(date_breaks = "1 hour", labels = date_format("%H:%M"))+
  xlab("Time")+
  ylab("Station")+
  labs(title = paste0("String chart of SEPTA ", route_tar_name, " on sample date ", date_tar), subtitle = paste0("Showing service from ", seconds_to_period(time_start), " to ", seconds_to_period(time_end)))
