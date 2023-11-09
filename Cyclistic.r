### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

# Install and load required packages
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("lubridate")) {
  install.packages("lubridate")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("readr")) {
  install.packages("readr")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}

library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)


getwd() #displays your working directory
setwd("C:\\Users\\weniw\\Projects\\Google Data Analytics\\8 Data Analysis Capstone\\Track 1\\Case Study 1\\CsvFiles") 
#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
trip_09_2022 <- read_csv("202209-divvy-publictripdata.csv")
trip_10_2022 <- read_csv("202210-divvy-tripdata.csv")
trip_11_2022 <- read_csv("202211-divvy-tripdata.csv")
trip_12_2022 <- read_csv("202212-divvy-tripdata.csv")
trip_01_2023 <- read_csv("202301-divvy-tripdata.csv")
trip_02_2023 <- read_csv("202302-divvy-tripdata.csv")
trip_03_2023 <- read_csv("202303-divvy-tripdata.csv")
trip_04_2023 <- read_csv("202304-divvy-tripdata.csv")
trip_05_2023 <- read_csv("202305-divvy-tripdata.csv")
trip_06_2023 <- read_csv("202306-divvy-tripdata.csv")
trip_07_2023 <- read_csv("202307-divvy-tripdata.csv")
trip_08_2023 <- read_csv("202308-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(trip_09_2022)
colnames(trip_10_2022)
colnames(trip_11_2022)
colnames(trip_12_2022)
colnames(trip_01_2023)
colnames(trip_02_2023)
colnames(trip_03_2023)
colnames(trip_04_2023)
colnames(trip_05_2023)
colnames(trip_06_2023)
colnames(trip_07_2023)
colnames(trip_08_2023)

# Inspect the dataframes and look for incongruencies
str(trip_09_2022)
str(trip_10_2022)
str(trip_11_2022)
str(trip_12_2022)
str(trip_01_2023)
str(trip_02_2023)
str(trip_03_2023)
str(trip_04_2023)
str(trip_05_2023)
str(trip_06_2023)
str(trip_07_2023)
str(trip_08_2023)

# Convert ride_id and rideable_type to character so that they can stack correctly
trip_09_2022 <-  mutate(trip_09_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_10_2022 <-  mutate(trip_10_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_11_2022 <-  mutate(trip_11_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_12_2022 <-  mutate(trip_12_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_01_2023 <-  mutate(trip_01_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_02_2023 <-  mutate(trip_02_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_03_2023 <-  mutate(trip_03_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_04_2023 <-  mutate(trip_04_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_05_2023 <-  mutate(trip_05_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_06_2023 <-  mutate(trip_06_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_07_2023 <-  mutate(trip_07_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
trip_08_2023 <-  mutate(trip_08_2023, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(trip_09_2022, trip_10_2022, trip_11_2022, trip_12_2022, trip_01_2023, trip_02_2023, trip_03_2023, trip_04_2023, trip_05_2023, trip_06_2023, trip_07_2023, trip_08_2023)

# Remove lat, long
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.
tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
#Check if there are any missing values in the ride_length
all_trips_v2 <- na.omit(all_trips_v2)

#Ensure that the ride_length column is of numeric data type
all_trips_v2$ride_length <- as.numeric(all_trips_v2$ride_length)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
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

# # Create a box plot for ride length by member type
# ggplot(all_trips_v2, aes(x = member_casual, y = ride_length, fill = member_casual)) +
#   geom_boxplot() +
#   labs(title = "Ride Length Distribution by Member Type",
#        x = "Member Type",
#        y = "Ride Length (seconds)") +
#   scale_fill_manual(values = c("casual" = "lightblue", "member" = "lightgreen")) +
#   theme_minimal()

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean, data = all_trips_v2)
write.csv(counts, file = "C:/Users/weniw/Projects/Google Data Analytics/8 Data Analysis Capstone/Track 1/Case Study 1/avg_ride_length.csv")


