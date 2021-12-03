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

#We are looking at which segment of NEC are causing train small delay
##Speed Limits
##Too many passengers get on and off making it stop late

AGG$scheduled_time <- strptime(AGG$scheduled_time, format = "%Y-%m-%d %H:%M:%OS", tz = "EST")
AGG$actual_time <- strptime(AGG$actual_time, format = "%Y-%m-%d %H:%M:%OS", tz = "EST")

class(Newark_NY_DEP$scheduled_time)

AGG <- AGG%>%
  mutate(interval60 = floor_date(scheduled_time, unit = "hour"),
         week = week(scheduled_time),
         dotw = wday(scheduled_time))

AGG_NY <- AGG[endsWith(AGG$train_id,("2"))|
                endsWith(AGG$train_id,("4"))|
                endsWith(AGG$train_id,("6"))|
                endsWith(AGG$train_id,("8"))|
                endsWith(AGG$train_id,("0")),]

NECSbigNY <- filter(AGG_NY, to=="Trenton"|to=="Hamilton"|to=="Princeton Junction"|to=="New Brunswick"|to=="Metropark"|to=="Newark Airport"|to=="Newark Penn Station"|to=="New York Penn Station")%>%
  na.omit()

NECSum_AGG_NY <- aggregate(NECSbigNY$delay_minutes, list(NECSbigNY$to), mean)
#Other than Trenton, A lot of trains running late after NB.
#Padding between Trenton - Hamilton

AGG_TR <- AGG[endsWith(AGG$train_id,("1"))|
                endsWith(AGG$train_id,("3"))|
                endsWith(AGG$train_id,("5"))|
                endsWith(AGG$train_id,("7"))|
                endsWith(AGG$train_id,("9")),]

NECSbigTR <- filter(AGG_TR, to=="Newark Penn Station"|to=="Newark Airport"|to=="Metropark"|to=="New Brunswick"|to=="Princeton Junction"|to=="Hamilton"|to=="Trenton")%>%
  na.omit()
NECSum_AGG_TR <- aggregate(NECSbigTR$delay_minutes, list(NECSbigTR$to), mean)
#Late between Newark -Metropark, Princeton Junction
#Padding between Hamilton - Trenton