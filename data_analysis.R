###############################
#Cyclistic Data study
###############################

#The data is locted on a webpage as a set of csv files availble through a link on coursera google analytics course. 
#All the files have been downloaded to the loacl hardware. 
#The biggest issue with this dataset is the size and number of files.

#=========================================================================
#The dataset has Millions of rows in each csv file
#Excel can't process the number of rows, and my hardware can't process more than a few files
#In order to optimise the work load, it was decided to split the data in small parts and analyse them separately

###################################################################################################################
#Cleaning Data
###################################################################################################################


#===========================================
#Step 1 - Group the data by year
#===========================================

#The first step is to download all the raw data in the folder and set it as the work direcotry
#..............................................................................................
setwd("/users/.../google certificate capstone")

# In the fowlloing step all the data corresponding to a sepcific year is grouped into one set of data (the example below if for 2017)
# Each csv file is read as a DF and stored (for ex, 2017-Q1 is read as data_1)
#....................................................................................................................................
data_1 <- read.csv('Divvy_Trips_2017_Q1.csv')
data_2 <- read.csv('Divvy_Trips_2017_Q2.csv')
data_3 <- read.csv('Divvy_Trips_2017_Q3.csv')
data_4 <- read.csv('Divvy_Trips_2017_Q4.csv')


#Some of the columns are renamed to create consistency across all data (note that over the years a lot of columns had changed names)
#In the example below trip_id is renamed ride_id, etc...
#....................................................................................................................................
data_1_2 <- rename(data_1, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_2_2 <- rename(data_2, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_3_2 <- rename(data_3, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)
data_4_2 <- rename(data_4, ride_id = trip_id, started_at = start_time, ended_at = end_time, member_casual = usertype)



#As the data is huge, it was decided to select only the data which is analysed.
#The intial analysis is to have the time data across all years and the ride_id to have the number of unique rides.
#So only the ride_id, the time data and the category (member or casual) are selected
#This allows the file to be less heavy and easier to process with my hardware.
#.................................................................................................................
data_1_1 <- select(data_1_2, ride_id, started_at, ended_at, member_casual)
data_2_1 <- select(data_2_2, ride_id, started_at, ended_at, member_casual)
data_3_1 <- select(data_3_2, ride_id, started_at, ended_at, member_casual)
data_4_1 <- select(data_4_2, ride_id, started_at, ended_at, member_casual)

#All the filtered and selected data is appended to create a file that comprises the full year.
#For ex, all dataframe are appended into one new DF called All_2017, where the full data for 2017 is present.
#............................................................................................................
All_2017 <- bind_rows(data_1_1, data_2_1, data_3_1, data_4_1)

#The newly created dataframe is exported as csv file.
#Note: initially several diffrent combination of variables, such as location data & number of years were attempted, but this always generated a big file.
#The output generated here was the best compromise between size and content to carry out the analysis on my hardware.
#........................................................................................................................................................
write.csv(All_2017, file = '/Users/.../Time_data/All_2017_time_member.csv')


#=========================================================================================================
#The process described above has been carried out separately over all years from 2013-2022.
#Every file (corresponding to a year) is analysed separatly and a csv file is created.
#These files will now be transformed and analysed
#==========================================================================================================


#===========================================
#Step 2 - Clean and trasform raw data
#===========================================

#The second part of the work consists of anlysing the manageable data set.
#First lets set the new working directory where the previously generated data is located. 
#lets also invoque some useful functions
#-----------------------------------------------------------------------------------------
setwd("/users/.../Time_data")
library(tidyverse)
library(lubridate)

#read the simplified files files and add them to a DF
#------------------------------------------------------------------------
df_1 <- read.csv('All_2017_time_member.csv')

#Reassign the data to the desired values (we will use the 2020 labels, where the categories are member and casual)
#-----------------------------------------------------------------------------------------------------------------
df_1 <-  df_1 %>% mutate(member_casual = recode(member_casual,"Subscriber" = "member","Customer" = "casual"))

#Case 1
#first convert the time from char to datetime
#--------------------------------------------                
df_1 <-  mutate(df_1, started_at = as_datetime(started_at) ,ended_at = as_datetime(ended_at)) 

#Case 2
#if this doesnt work (when date time format is not correct, e.g. some dates are present as M/Y instead of M/d/Y)
#The solution is to split the string and define each new column to the value it corresponds to (e.g. 'year', 'month'...)
#------------------------------------------------------------------------------------------------------------------------

#split date to day month and year 
#--------------------------------
df_1$copy <- df_1$date
df_1 <- df_1 %>% separate(copy, c('month', 'day', 'year'), "/")

#split time in hour month minutes and seconds
#-------------------------------------------- 
df_1$copy2 <- df_1$time
df_1 <- df_1 %>% separate(copy2, c('hour', 'minute', 'seconds'), ":")

#Check unique values to see if the data seems correct (e.g. months should go from 1 to 12)
#-----------------------------------------------------------------------------------------
unique(df_1$month)
unique(df_1$year)
unique(df_1$day)

#-------------------------------------------------------
#Case 1
#Lets Add New columns with the use of lubridate 
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
colnames(df_1)
dim(df_1)
summary(df_1)
head(df_1)
str(df_1) 

#Creation of a V2 without negative values of trip duration and HQ
#----------------------------------------------------------------
df_2 <- df_1[!(df_1$trip_duration<0),]

#Check data is clean of negative values 
#----------------------------------------------------------------
summary(df_2)
head(df_2)

#-------------------------------------------------------------------------------------------------------------
#Case 2
#transform the collumns into one date column recognised by lubridate (this is to retrieve the day of the week)
#remove the time variant to make the DF lighter
#-------------------------------------------------------------------------------------------------------------

df_2 <- subset(df_1, select = -c(seconds, date, time))
df_2$date2 <- paste(df_2$year,"-",df_2$month,"-",df_2$day)
df_2 <-  mutate(df_2, date2 = as_date(date2)) 
df_2$day_of_week <- format(as.Date(df_2$date2),"%A")

#Summary of Trip_duration 
#------------------------------------------------------------
summary(df_2$trip_duration)

#=====================================
# STEP 3: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================


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
# STEP 4: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
#4 files are created, 2 set of average data and 2 set of counts, each set analysed by day of the week and by month
#------------------------------------------------------------------------------------------------------------------

#Average and cout of trip duration by day of the way 
#-------------------------------------------------------
average <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day_of_week, FUN = mean), c("member_casual", "day_of_week", "Avg_trip"))
counts <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$day_of_week, FUN = length), c("member_casual", "day_of_week", "num_trip"))

#Average and cout of trip duration by month 
#-------------------------------------------------------
average_month <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$month, FUN = mean) , c("member_casual", "month", "Avg_trip"))
counts_months <- setNames(aggregate(df_2$trip_duration ~ df_2$member_casual + df_2$month, FUN = length), c("member_casual", "month", "num_trip"))

#write a new csv file with the average and counts by the average ride
#--------------------------------------------------------------------
write.csv(average, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_avg_ride.csv')
write.csv(counts, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_num_rides.csv')

#write a new csv file with the average and counts by month
#--------------------------------------------------------------------
write.csv(average_month, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_avg_ride_months.csv')
write.csv(counts_months, file = '/Users/happy/Desktop/Divvy/Time_data/analysed data/2018_num_rides_months.csv')

#==============================================================================================================================================================
#==============================================================================================================================================================
#==============================================================================================================================================================

#The cvs files written are appended by excel as the size of the data allows for it to be managed easily.
#Furthermore this allows for double checking the coherence of the data observed.
#The new files created are uploaded to the folder and analysed in a new notebook. 
