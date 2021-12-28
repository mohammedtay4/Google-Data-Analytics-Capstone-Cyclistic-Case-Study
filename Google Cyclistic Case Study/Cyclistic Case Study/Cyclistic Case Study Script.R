####Cyclistic bike-share data Analysis
#This analysis is based on the Cyclistic bike-share 
#analysis case study. The purpose of this script is 
#to consolidate downloaded bike-trip data into a single dataframe 
#and then conduct simple analysis to help answer the key question:
#"How Does a Bike-Share Navigate Speedy Success?"

# Loading the required packages

library(tidyverse)  #helps wrangle the data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize the data
getwd() #displays your working directory
setwd("") 


#=====================
# STEP 1: COLLECT DATA
#=====================

NOV20 <- read_csv("1_202011-divvy-tripdata.csv")
DEC20 <- read_csv("2_202012-divvy-tripdata.csv")
JAN21 <- read_csv("3_202101-divvy-tripdata.csv")
FEB21 <- read_csv("4_202102-divvy-tripdata.csv")
MAR21 <- read_csv("5_202103-divvy-tripdata.csv")
APR21 <- read_csv("6_202104-divvy-tripdata.csv")
MAY21 <- read_csv("7_202105-divvy-tripdata.csv")
JUN21 <- read_csv("8_202106-divvy-tripdata.csv")
JUL21 <- read_csv("9_202107-divvy-tripdata.csv")
AUG21 <- read_csv("10_202108-divvy-tripdata.csv")
SEP21 <- read_csv("11_202109-divvy-tripdata.csv")
OCT21 <- read_csv("12_202110-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

#inspecting columns
colnames(NOV20)
colnames(DEC20)
colnames(JAN21)
colnames(FEB21)
colnames(MAR21)
colnames(APR21)
colnames(MAY21)
colnames(JUN21)
colnames(JUL21)
colnames(AUG21)
colnames(SEP21)
colnames(OCT21)

#Inspecting the dataframes to check for incongruencies
str(NOV20)
str(DEC20)
str(JAN21)
str(FEB21)
str(MAR21)
str(APR21)
str(MAY21)
str(JUN21)
str(JUL21)
str(AUG21)
str(SEP21)
str(OCT21)

# Fixing incogruencies 
NOV20 <-  transform(NOV20, start_station_id = as.character(start_station_id)
                 ,end_station_id = as.character(end_station_id))

OCT21[['started_at']] <- as.POSIXct(OCT21[['started_at']], format = "%Y-%m-%d %H:%M:%S")
OCT21[['ended_at']] <- as.POSIXct(OCT21[['ended_at']], format = "%Y-%m-%d %H:%M:%S")

# Converting ride_id and rideable_type to character so that they can stack correctly

NOV20 <-  mutate(NOV20, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
DEC20 <-  mutate(DEC20, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
JAN21 <-  mutate(JAN21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
FEB21 <-  mutate(FEB21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
MAR21 <-  mutate(MAR21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
APR21 <-  mutate(APR21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
MAY21 <-  mutate(MAY21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
JUN21 <-  mutate(JUN21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
JUL21 <-  mutate(JUL21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
AUG21 <-  mutate(AUG21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
SEP21 <-  mutate(SEP21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))
OCT21 <-  mutate(OCT21, ride_id = as.character(ride_id)
                 ,rideable_type = as.character(rideable_type))

# Stacking the individual month data frames into one big data frame
all_trips <- bind_rows(NOV20,DEC20,JAN21,FEB21,MAR21,APR21,MAY21,JUN21,JUL21,AUG21,SEP21,OCT21)

# Remove lat and long fields 
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

glimpse(all_trips)
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Re-ordering the days
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# The average ride time again by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '')


