library(tidyverse)
library(kableExtra)
library(tidytransit)
library(lubridate)
library(dplyr)
library(readr)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

Mar18 <- read.csv("delay/2018_03.csv")
Apr18 <- read.csv("delay/2018_04.csv")
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
