library(tidyverse)
library(kableExtra)
library(tidytransit)
library(lubridate)
library(dplyr)
library(readr)
library(riem)
library(gridExtra)
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

Mar18 <- read.csv("2018_03.csv")
Apr18 <- read.csv("2018_04.csv")
AGG <- rbind(Mar18, Apr18)

Newark_DEP <- filter(AGG, to=="Newark Penn Station"|type=="NJ Transit")

Newark_DEP <- filter(AGG, to=="Newark Penn Station")%>%
  filter(type=="NJ Transit")

summary(Newark_DEP$delay_minutes)

Newark_NY_DEP <- Newark_DEP[endsWith(Newark_DEP$train_id,("2"))|
                                endsWith(Newark_DEP$train_id,("4"))|
                                endsWith(Newark_DEP$train_id,("6"))|
                                endsWith(Newark_DEP$train_id,("8"))|
                                endsWith(Newark_DEP$train_id,("0")),]

## Maybe we can look at which hour, day, date of the week have the most delays?
Newark_NY_DEP$scheduled_time <- strptime(Newark_NY_DEP$scheduled_time, format = "%Y-%m-%d %H:%M:%OS", tz = "EST")
Newark_NY_DEP$actual_time <- strptime(Newark_NY_DEP$actual_time, format = "%Y-%m-%d %H:%M:%OS", tz = "EST")

class(Newark_NY_DEP$scheduled_time)

Newark_NY_DEP <- Newark_NY_DEP%>%
  mutate(interval60 = floor_date(scheduled_time, unit = "hour"),
         week = week(scheduled_time),
         dotw = wday(scheduled_time))

Newark_NY_DEP %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Hour = hour(scheduled_time)) %>%
  ggplot(aes(Hour,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains at Newark Penn Delay by hours of the day",
       x="Hour", y="delay") +
  plotTheme()

Newark_NY_DEP %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Day = date(scheduled_time)) %>%
  ggplot(aes(Day,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains at Newark Penn Delay by Day",
       x="Hour", y="delay") +
  plotTheme()

#Look at trains more than 10 mins late
Severedelay <- Newark_NY_DEP%>%
  filter(delay_minutes>15)%>%
  mutate(Hour = hour(scheduled_time),
         day = format(scheduled_time, format="%m-%d"))

Severedelay %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Hour = hour(scheduled_time)) %>%
  ggplot(aes(Hour,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains with >15min late at Newark Penn Delay by hours of the day",
       x="Hour", y="delay") +
  plotTheme()

#trains that often have >20min delay
summary(as.factor(Severedelay$train_id))

#by day
Severedelay %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Day = date(scheduled_time)) %>%
  ggplot(aes(Day,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains with >15min late at Newark Penn Delay by Day",
       x="Hour", y="delay") +
  plotTheme()

summary(as.factor(Severedelay$day))
summary(as.factor(Severedelay$dotw))
Severedelay_sat <- filter(Severedelay, dotw==6)
#03-01 morning (11), 03-02 afternoon (26), 03-08 morning, 03-09 morning, 03-16 morning...
#04-05 evening, 04-13 evening, 04-23 morning, 04-24 evening...
#I just realized "Explosive cyclogenesis" in NYC in the beginning of March 2018!
#and snow around March 10

#What about trains with little delays
Smalldelay <- Newark_NY_DEP%>%
  filter(delay_minutes<15)

Smalldelay %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Hour = hour(scheduled_time)) %>%
  ggplot(aes(Hour,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains with <15min late at Newark Penn Delay by hours of the day",
       x="Hour", y="delay") +
  plotTheme()

Smalldelay %>%
  group_by(interval60) %>% 
  summarize(delay_minutes,
            Day = date(scheduled_time)) %>%
  ggplot(aes(Day,delay_minutes)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  labs(title="NJT NY-bound trains with <15min late at Newark Penn Delay by Day",
       x="Hour", y="delay") +
  plotTheme()

#Weather
weather.DataNWK <- 
  riem_measures(station = "EWR", date_start = "2018-03-01", date_end = "2018-05-01")

weather.PanelNWK <-  
  weather.DataNWK %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Percipitation = sum(p01i),
            Huminity = mean(relh),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

grid.arrange(top = "Weather Data - Newark, NJ - March, April 2018",
             ggplot(weather.PanelNWK, aes(interval60,Percipitation)) + geom_line() + 
               labs(title="Percipitation", x="Hour", y="Percipitation") + plotTheme(),
             ggplot(weather.PanelNWK, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
             ggplot(weather.PanelNWK, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Hour", y="Temperature") + plotTheme(),
             ggplot(weather.PanelNWK, aes(interval60,Huminity)) + geom_line() + 
               labs(title="Huminity", x="Hour", y="Huminity") + plotTheme())

Severedelay <- Severedelay%>%
  left_join(weather.PanelNWK, by = "interval60")%>%
  mutate(trip_count = 1)

NWKSum <- Severedelay%>%
  group_by(interval60) %>%
  summarize(num = sum(trip_count, na.rm=T),
            mean_late = mean(delay_minutes))%>% 
  left_join(weather.PanelNWK, by = "interval60")%>%
  na.omit()

##Big Wind
NWKSum %>%
  group_by(interval60) %>% 
  summarize(mean_late = mean(mean_late),
            Wind = first(Wind_Speed)) %>%
  mutate(BigWind = ifelse(Wind > 18,"Big Wind", "None")) %>%
  group_by(BigWind) %>%
  summarize(Mean_Trip_Count = mean(mean_late)) %>%
  ggplot(aes(BigWind, Mean_Trip_Count)) + geom_bar(stat = "identity") +
  labs(title="Is delay related to Big Wind",
       x="Wind", y="Mean Big Delay") +
  plotTheme()

NWKSum %>%
  group_by(interval60) %>% 
  summarize(Trip_Count = mean(mean_late),
            Wind = first(Wind_Speed)) %>%
  mutate(month = month(interval60)) %>%
  ggplot(aes(Wind, Trip_Count)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  facet_wrap(~month, ncol=3) + 
  labs(title="Minute Late by Wind Speed by month",
       x="Wind Speed", y="Mean Late (Mins)") +
  plotTheme()

##Rain
NWKSum %>%
  group_by(interval60) %>% 
  summarize(Trip_Count = mean(mean_late),
            rain = first(Percipitation)) %>%
  mutate(month = month(interval60)) %>%
  ggplot(aes(rain, Trip_Count)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  facet_wrap(~month, ncol=2) + 
  labs(title="Minute Late by Percipitation by month",
       x="Percipitation", y="Mean Late (Mins)") +
  plotTheme()

##Humidity
NWKSum %>%
  group_by(interval60) %>% 
  summarize(Trip_Count = mean(mean_late),
            Huminity = first(Huminity)) %>%
  mutate(month = month(interval60)) %>%
  ggplot(aes(Huminity, Trip_Count)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  facet_wrap(~month, ncol=2) + 
  labs(title="Minute Late by Huminity month",
       x="Huminity", y="Mean Late (Mins)") +
  plotTheme()