library(data.table)
library(dplyr)
library(useful)
library(ggplot2)
library(lubridate)
library(quantreg)
library(splines)
library(stringi)
library(scales)
library(tidyr)

calendar_dates <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJTR_GTFS/calendar_dates.txt")
stop_times <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJTR_GTFS/stop_times.txt")
trips <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJTR_GTFS/trips.txt")
stops <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJTR_GTFS/stops.txt")
routes <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJTR_GTFS/routes.txt")
rts <- read.csv("C:/Users/udays/Desktop/Transit Analytics/NJT_route_comb.csv")

targetDay <- as.Date("2020-01-15")
targetStop <- "105"
#target_route <- c("Morris & Essex Line", "Gladstone Branch")
#target_route1 <- "Montclair-Boonton Line"
#target_route2 <- "!"
#direction <- 0
#Atlantic City Rail Line, Montclair-Boonton Line, Hudson-Bergen Light Rail, Main/Bergen County Line, Port Jervis Line, Morris & Essex Line, Gladstone Branch, Northeast Corridor, North Jersey Coast Line, Newark Light Rail, Pascack Valley Line, Princeton Shuttle, Raritan Valley Line, Riverline Light Rail    

rts$Route_1 <- as.character(rts$Route_1)
rts$Route_2 <- as.character(rts$Route_2)
result <- data.frame(target_route=character(0),  wkd_intervals=numeric(0), patterns=numeric(0), complexity_score=numeric(0), color=character(0))
result_2 <- data.frame(count=numeric(0), pattern_id=character(0))

calendar_dates$date <- stri_c(substr(calendar_dates$date, 1,4), "-", substr(calendar_dates$date, 5,6), "-", substr(calendar_dates$date, 7,8))
calendar_dates$date <- as.Date(calendar_dates$date)
calendar_dates <- subset(calendar_dates, calendar_dates$date == targetDay)
calendar_dates <- calendar_dates[!duplicated(calendar_dates$service_id),]
trips <- subset(trips, trips$service_id %in% calendar_dates$service_id)

mastersheet <- merge(trips, stop_times)
mastersheet <- merge(mastersheet, routes)
mastersheet <- mastersheet[,-c(1,7,12,13,16,19,20)]

key <- mastersheet%>%
  filter(stop_id == targetStop)%>%
  filter(period_to_seconds(hms(arrival_time))>period_to_seconds(time_start))%>%
  filter(period_to_seconds(hms(arrival_time))<=period_to_seconds(time_end))%>%
  filter(direction_id == 0)

mastersheet <- mastersheet%>%
  filter(trip_id %rerf
rts <- rts%>%  
  filter(Route_1 %in% key$route_long_name)

a <- function(target_route1, target_route2, color){
  target_route <- c(target_route1, target_route2)
  df <- subset(mastersheet, mastersheet$route_long_name %in% target_route)
  #df <- subset(df, df$direction_id == direction)
  #subset stops if using multiple routes
  #df <- subset(df, df$stop_id %in% subset(df, df$route_long_name == "Morris & Essex Line")$stop_id)
  #end subset code
  df <- merge(df, stops[,c(1,3)])
  df <- arrange(df, df$trip_id, df$stop_sequence)
  df2 <- df[,c(6,9,14)]
  df2$stop_sequence <- 1
  df2 <- reshape(df2, timevar = "stop_name", idvar = "block_id", direction = "wide")
  dfid <- unite(df2, "id", -1)
  df2 <- merge(df2, dfid)
  df2$block_id <- paste(target_route1, target_route2, df2$block_id)
  df3 <- df2[!duplicated(df2[,c(2:ncol(df2))]),]
  perm <- nrow(df3)
  counter <- function(id, pattern){
    ct <- nrow(df2[which(df2$id == pattern),])
    ct <- as.data.frame(ct)
    ct$pattern_id <- id
    colnames(ct) <- c("count", "pattern_id")
    ct$pattern_id <- as.character(ct$pattern_id)
    ct$count <- as.numeric(ct$count)
    result_2 <- rbind(result_2, ct)
    result_2 <<- result_2
  }
  b <- mapply(counter, df3$block_id, df3$id)
  target_route <- as.data.frame(stri_c(target_route1,", ", target_route2))
  target_route$wkd_intervals <- nrow(df[!duplicated(df$trip_id),])
  target_route$patterns <- perm
  target_route$complexity_score <- perm/target_route$wkd_intervals
  target_route$color <- as.character(color)
  colnames(target_route)<- c("target_route", "wkd_intervals", "patterns","complexity_score", "color")
  result <- rbind(result, target_route)
  result <<- result
  result_2 <<- result_2
  name <- stri_c(target_route1, target_route2)
  assign(stri_c(name, "df"), df3, envir = globalenv())
  
}

mapply(a, rts$Route_1, rts$Route_2,  rts$color)

result$color <- stri_c("#", result$color)
result <- arrange(result, result$target_route)
result_2 <- subset(result_2, result_2$count > 4)
result_2 <- result_2[!duplicated(result_2),]

ggplot(result, aes(x = target_route, y = complexity_score))+
  geom_col(fill = result$color)+
  scale_y_continuous(breaks=pretty_breaks(), limits = c(0,1))+
  labs(title="Complexity of NJT Rail Schedules", subtitle = "Higher values indicate increased complexity")+
  xlab("Route-Direction")+
  ylab("Number of stopping patterns per weekday train")+
  theme(axis.text.x=element_text(angle=90,hjust=1, vjust=.3))
