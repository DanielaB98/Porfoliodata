---
  title: "Cyclistic_Analysis"
author: "Daniela_Bonilla"
date: "2024-08-07"
output: html_document
---
  # STEP1:BUSINESS TASK
  
  #*Scope of Work (sow): 
 # **Deliverables**:
 # -Problem to solve: Convert occasional cyclists into members.

#**Identify the Business Task**:
#  -Develop marketing strategies and convert occasional cyclists into 
#annual members.

#**Consider Key Stakeholders**:
#  -Lily Moreno: Marketing Director and Manager
#-Cyclistic Executive Team

#**Timeline**: 1 week start: 6/august/2024 end:13/august/2024



## Packages used

#Packages
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)


# STEP2 :Collect data
#Data: Data sources used:
 # In this case, we will use historical data:
 # trips, routes, stations, custumer type
#Provided by Motivate International Inc.
#https://divvy-tripdata.s3.amazonaws.com/index.html 
#under this license
#data to analize: 2023 Nov to 2024 Jan ( three months)


#Load data
data <- read_csv("data.csv")
data2 <-read_csv("data2.csv")
data3 <- read_csv("data3.csv")
alldata<-bind_rows(data,data2,data3)



# STEP3: CLEAN DATA

#Data previously cleaned in excel, steps followed:
  
 # **Blank Spaces**: Rows containing blank cells were removed as this could bias our analysis.
#**Duplicate Values**: No duplicate values.
#**Date Format**: started_at, ended_at.
#**New Variable**: Ride_duration (ended_at - started_at). This format was chosen to account for full days in the ride duration.
#**Date**: Verify that the start date is not later than the end date (row deletion).
#**Extra Spaces**: Trim()
#**Data elimination**: 
#  To ensure that all groups have the same amount of data, a process supported by sample size and margin of error <1, with a confidence level of 95%, determining that the sample size is optimal for analyzing the data and minimizing the risk of bias.


#Check for NA values
na_rows <- alldata %>% filter(!complete.cases(.))
print(na_rows)
#none NA Values
#Check dates
str(alldata$started_at) #chr
str(alldata$ended_at)#chr
str(alldata$ride_duration) #chr "secs"





# Descriptive analysis
#Calculate Minimum, Maximum, Average(mean),Median of the ride duration.

{r}
min(alldata$ride_duration)
max(alldata$ride_duration)
mean(alldata$ride_duration)
median(alldata$ride_duration)



{r}
## Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year 

alldata$date <- as.Date(alldata$started_at) #The default format is yyyy-mm-dd
alldata$month <- format(as.Date(alldata$date), "%m")
alldata$day <- format(as.Date(alldata$date), "%d")
alldata$year <- format(as.Date(alldata$date), "%Y")
alldata$day_of_week <- format(as.Date(alldata$date), "%A")




# Compare members and casual users
#average ride time by each day for members vs casual users


# Compare members and casual users
aggregate(alldata$ride_duration ~ alldata$member_casual, FUN = mean)
aggregate(alldata$ride_duration ~ alldata$member_casual, FUN = median)
aggregate(alldata$ride_duration ~ alldata$member_casual, FUN = max)
aggregate(alldata$ride_duration ~ alldata$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(alldata$ride_duration ~ alldata$member_casual + alldata$day_of_week, 
          FUN = mean) #00:12:17
#See the maximun ride time by each day for members vs casual users 
aggregate(alldata$ride_duration ~ alldata$member_casual + alldata$day_of_week, 
          FUN = max)#23:59:47
#See the media ride time by each day for members vs casual users 
aggregate(alldata$ride_duration ~ alldata$member_casual + alldata$day_of_week, 
          FUN = median)#00:07:51


#**Analysis**:
 # The maximum duration for casuals is 23:45:57 on Monday, while for members, it is 23:20:23 on Monday too.

#On Saturdays, casuals have the longest average ride duration, and the maximum durations are the highest across the week for both groups.


## Days of week


# Notice that the days of the week are out of order> Ordered
alldata$day_of_week <- ordered(alldata$day_of_week,
                               levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(alldata$ride_duration ~ alldata$member_casual + alldata$day_of_week, FUN = mean)





#**Analysis**:
 # We can see that the ride duration for casual users is longer compared to members.
#This might suggest that casual users tend to take longer rides, while members might use the service more frequently but for shorter periods.







# Visualization of the number of rides by rider type

alldata %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #col of week
  group_by(member_casual, weekday) %>%  # group by customer and day of week
  summarise(
    number_of_rides = n(),  # number of rides
    average_duration = mean(ride_duration, na.rm = TRUE)  # avg duration; na.rm = TRUE, for values NA
  ) %>% 
  arrange(member_casual, weekday) %>%  # arrange results
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +  # aesthetics
  geom_col(position = "dodge")  # bar chart by custumer




#**Analysis**:
 # We can observe that Casual members have a longer ride duration on Saturdays and Sundays.In contrast, Annual members are more consistent every day, members also have more rides on Saturday, Wednesday, and Sunday.


# Bike type classic vs electric
## Comparation ride per bike


#calculate usage
bike_usage <- alldata %>%
  group_by(rideable_type, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(desc(number_of_rides))

#Visualization
ggplot(bike_usage, aes(x = reorder(rideable_type, -number_of_rides), y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    x = "Bike Type",
    y = "Number of Rides",
    fill = "User Type",
    title = "Number of Rides per Bike Type by User Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#**Analysis**:
 # Notice that the Classic bike is very popular among members, while casual users use electric bikes almost as much as classic bikes.


#Popular route and station


#Most popular start stations for Casuals and members

top_start_stations_members <- alldata %>%
  filter(member_casual == "member") %>%
  count(start_station_name) %>%
  top_n(2, n)

top_start_stations_casuals <- alldata %>%
  filter(member_casual == "casual") %>%
  count(start_station_name) %>%
  top_n(2, n)

# Most popular end stations for casuals and members

top_end_stations_members <- alldata %>%
  filter(member_casual == "member") %>%
  count(end_station_name) %>%
  top_n(2, n)

top_end_stations_casuals <- alldata %>%
  filter(member_casual == "casual") %>%
  count(end_station_name) %>%
  top_n(2, n)



# Most frequent days 


# Most frequent days for start stations

top_start_stations_days_members <- alldata %>%
  filter(member_casual == "member", start_station_name %in% top_start_stations_members$start_station_name) %>%
  count(start_station_name, day_of_week)

top_start_stations_days_casuals <- alldata %>%
  filter(member_casual == "casual", start_station_name %in% top_start_stations_casuals$start_station_name) %>%
  count(start_station_name, day_of_week)

# Most frequent days for end stations 

top_end_stations_days_members <- alldata %>%
  filter(member_casual == "member", end_station_name %in% top_end_stations_members$end_station_name) %>%
  count(end_station_name, day_of_week)

top_end_stations_days_casuals <- alldata %>%
  filter(member_casual == "casual", end_station_name %in% top_end_stations_casuals$end_station_name) %>%
  count(end_station_name, day_of_week)



# Visualizations


# Visualizations
ggplot(top_start_stations_days_members, aes(x = day_of_week, y = n, fill = start_station_name)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Start Station", title = "Top Start Stations for Members") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graficar las estaciones de inicio más populares para casuales
ggplot(top_start_stations_days_casuals, aes(x = day_of_week, y = n, fill = start_station_name)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Start Station", title = "Top Start Stations for Casuals")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graficar las estaciones de finalización más populares para miembros
ggplot(top_end_stations_days_members, aes(x = day_of_week, y = n, fill = end_station_name)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rides", fill = "End Station", title = "Top End Stations for Members")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graficar las estaciones de finalización más populares para casuales
ggplot(top_end_stations_days_casuals, aes(x = day_of_week, y = n, fill = end_station_name)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Number of Rides", fill = "End Station", title = "Top End Stations for Casuals")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




