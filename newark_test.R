library(tidyverse)
library(kableExtra)
library(tidytransit)
library(lubridate)
library(dplyr)
library(readr)

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
