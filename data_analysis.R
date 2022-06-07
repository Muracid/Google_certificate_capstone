#New working directory for trip duration study 
setwd("/users/happy/Desktop/Divvy/Time_data")
library(tidyverse)
library(lubridate)
#read the files and add them to a DF
#--------------------------------------------

df_1 <- read.csv('All_2017_time_member.csv')
df_2 <- read.csv('All_2018_time_member.csv')

#Reassign to the desired values (we will go with the current 2020 labels)
#------------------------------------------------------------------------
df_1 <-  df_1 %>% mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))
df_2 <-  df_2 %>% mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))


#first convert the time from char to datetime
#---------------------------------------------

df_1 <-  as.Date(date, format="%m/%d/%Y") 
test <- as.Date(date, "%m/%d/%Y")
                
df_2 <-  mutate(df_2, started_at = as_datetime(started_at) ,ended_at = as_datetime(ended_at)) 

methods(as_date)
?methods (as_date)
#if this doesnt work (when date time format is not correct), just split the existing data in required form)

#-------------------------------------------------------
#Lets Add New columns
#-------------------------------------------------------
#lets create new columns that take into account the dates 
df_2$date <- as.Date(df_2$started_at)
df_2$month <- format(as.Date(df_2$date),"%m")
df_2$year <- format(as.Date(df_2$date),"%Y")
df_2$day <- format(as.Date(df_2$date),"%d")
df_2$day_of_week <- format(as.Date(df_2$date),"%A")
df_2$hour_of_day <- format(as.POSIXct(df_2$started_at), "%H")




#used difftime to get the duration of the trip
#-------------------------------------------------------
df_1$trip_duration <- difftime(df_1$ended_at, df_1$started_at)


# Convert "trip_duration" from Factor to numeric so we can run calculations on the data
#--------------------------------------------------------
df_1$trip_duration <- as.numeric(as.character(df_1$trip_duration))
df_1$hour_of_day <- as.numeric(as.character(df_1$hour_of_day))
df_1$day <- as.numeric(as.character(df_1$day))

#check if its numeric
is.numeric(df_1$trip_duration)

#........................................................
#Double check data
#........................................................
#colnames(df_1)
#dim(df_1)
#summary(df_1)
#head(df_1)
#str(df_1) 


#Creation of a V2 without negative values of trip duration and HQ
#----------------------------------------------------------
df_2 <- df_1[!(df_1$trip_duration<0),]

#Check data is clean of negative values 
summary(df_2)
#head(df_2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

#Summary of Trip_duration 
#------------------------------------------------------------
summary(df_2$trip_duration)

# Compare members and casual users
#------------------------------------------------------------
aggregate(df_2$trip_duration ~ df_2$member_casual, FUN = mean)
aggregate(df_2$trip_duration ~ df_2$member_casual, FUN = median)
aggregate(df_2$trip_duration ~ df_2$member_casual, FUN = max)
aggregate(df_2$trip_duration ~ df_2$member_casual, FUN = min)
aggregate(df_2$trip_duration ~ df_2$member_casual, FUN = length)

# Order the day of the week by Sunday to Saturday
#-------------------------------------------------------------
df_2$day_of_week <- ordered(df_2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Average ride time by each day of week for members vs casual users
#--------------------------------------------------------------
aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day_of_week, FUN = mean)
#aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day, FUN = mean)

#-----------------------------------------------------
# Let's visualize the number of rides by rider type
#-----------------------------------------------------

#By weekday
df_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#By Month
df_2 %>% 
  mutate(month_1 = month(started_at)) %>% 
  group_by(member_casual, month_1) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month_1)  %>% 
  ggplot(aes(x = month_1, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


#-----------------------------------------------------
# Let's create a visualization for average duration
#-----------------------------------------------------
#by day of the week 
df_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#by month
df_2 %>% 
  mutate(month_1 = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_1) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month_1)  %>% 
  ggplot(aes(x = month_1, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
average <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day_of_week, FUN = mean), c("member_casual", "day_of_week", "Avg_trip"))
counts <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day_of_week, FUN = length), c("member_casual", "day_of_week", "num_trip"))

average_month <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$month, FUN = mean) , c("member_casual", "month", "Avg_trip"))
counts_months <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$month, FUN = length), c("member_casual", "month", "num_trip"))


write.csv(average, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_avg_ride.csv')
write.csv(counts, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_num_rides.csv')

write.csv(average_month, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_avg_ride_months.csv')
write.csv(counts_months, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_num_rides_months.csv')

#----------------------------
#will be needed
# Reassign to the desired values (we will go with the current 2020 labels)
#all_trips <-  all_trips %>% mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))

