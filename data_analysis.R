#The biggest issue with the data is the size of it. The dataset has Millions of rows and in several files
#Excel cant process the number of rows, and the my hardware cant process more than a few files
#In terms of time to process a function, and the result obtained, it was easier to split the data in many block and analysed separately


#The first step is to download all the raw data in the folder and set it as the work direcotry
setwd("/users/happy/Desktop/google certificate capstone")

# In the fowlloing steps the data for one year is grouped into one set of data (for example the 2017 data is read)
# Each csv file is read as a DF and stored (for ex, 2017-Q1 is read as data_1)
data_1 <- read.csv('Divvy_Trips_2017_Q1.csv')
data_2 <- read.csv('Divvy_Trips_2017_Q2.csv')
data_3 <- read.csv('Divvy_Trips_2017_Q3.csv')
data_4 <- read.csv('Divvy_Trips_2017_Q4.csv')


#Some of the columns are renamed to generate a set of consistency across all data available (note that over the years a lot of columns had changed names)
#In the example below trip_in is renamed ride_id, etc...

data_1_2 <- rename(data_1, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_2_2 <- rename(data_2, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_3_2 <- rename(data_3, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_4_2 <- rename(data_4, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)



#As the data is huge, it was decided to select onlyt the data which is analysed.
#In here only the ride_id, the time data and the category are selected
#This allows the file to be less heavy and easier to process with my hardware.

data_1_1 <- select(data_1_2, ride_id, started_at, ended_at, member_casual)
data_2_1 <- select(data_2_2, ride_id, started_at, ended_at, member_casual)
data_3_1 <- select(data_3_2, ride_id, started_at, ended_at, member_casual)
data_4_1 <- select(data_4_2, ride_id, started_at, ended_at, member_casual)

#All the different filtered and selected data is appended to create a file that comprises the full year.
#For ex, all dataframe are appended into one new DF called All_2017, where the full data for 2017 is present.

All_2017 <- bind_rows(data_1_1, data_2_1, data_3_1, data_4_1)

#Now that this df for a whole year is created, its written down as csv file.

write.csv(All_2017, file = '/Users/happy/Desktop/Divvy/Time_data/All_2017_time_member.csv')


#=========================================================================================================
#This process up to now has been carried out separately over all the years from 2013-2022.
#Every file (corresponding to a year) is analysed separatly and an output file is created.
#These final transformed data will be combined to generate visual graphs across years

#==========================================================================================================



#The second part of the work consists of anlysing the simplified and "cleaned" data set.
#First lets set the new working directory for trip duration study 

setwd("/users/happy/Desktop/Divvy/Time_data")
library(tidyverse)
library(lubridate)


#read the simplified files files and add them to a DF
#------------------------------------------------------------------------

df_1 <- read.csv('All_2017_time_member.csv')
df_2 <- read.csv('All_2018_time_member.csv')

#Reassign to the desired values (we will go with the current 2020 labels)
#------------------------------------------------------------------------
df_1 <-  df_1 %>% mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))


#Case 1
#first convert the time from char to datetime
                
df_1 <-  mutate(df_1, started_at = as_datetime(started_at) ,ended_at = as_datetime(ended_at)) 

#..............................................................................................................

#Case 2
#if this doesnt work (when date time format is not correct, e.g. some date are present as M/Y instead of M/d/Y)
#The solution is splitting the string and defining what each data is

#split date to day month and year 
df_1$copy <- df_1$date
df_1 <- df_1 %>% separate(copy, c('month', 'day', 'year'), "/")

#split time in hour month minutes and seconds 
df_1$copy2 <- df_1$time
df_1 <- df_1 %>% separate(copy2, c('hour', 'minute', 'seconds'), ":")

#Check unique values to see if the data seems correct (e.g. months shoudl go from 1 to 12)
unique(df_1$month)
unique(df_1$year)
unique(df_1$day)

#-------------------------------------------------------
#Case 1
#Lets Add New columns with the use if lubridate 
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

