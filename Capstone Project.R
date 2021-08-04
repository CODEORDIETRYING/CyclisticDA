#Installing and loading the necessary packages and libraries.
install.packages('tidyverse')
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(magrittr)
library(utils)

#########################################################

# Uploading the Cyclistic data set into R
June_2020 <- read.csv("202006-divvy-tripdata (June_2020).csv")
July_2020 <- read.csv("202007-divvy-tripdata (July_2020).csv")
Aug_2020 <- read.csv("202008-divvy-tripdata (Aug_2020).csv")
Sept_2020 <- read.csv("202009-divvy-tripdata (Sept_2020).csv")
Oct_2020 <- read.csv("202010-divvy-tripdata (Oct_2020).csv")
Nov_2020 <- read.csv("202011-divvy-tripdata (Nov_2020).csv")
Dec_2020 <- read.csv("202012-divvy-tripdata (Dec_2020).csv")
Jan_2021 <- read.csv("202101-divvy-tripdata (Jan_2021).csv")
Feb_2021 <- read.csv("202102-divvy-tripdata (Feb_2021).csv")
Mar_2021 <- read.csv("202103-divvy-tripdata (Mar_2021).csv")
Apr_2021 <- read.csv("202104-divvy-tripdata (Apr_2021).csv")
May_2021 <- read.csv("202105-divvy-tripdata (May_2021).csv")

ls() # To view all data frames in our current project

#########################################################

#Comparing column names of each data frame for consistency in data types and column names.

colnames(June_2020) # Column names are consistent. This was already done in Google Sheets.
colnames(July_2020)
colnames(Aug_2020)
colnames(Sept_2020)
colnames(Oct_2020)
colnames(Nov_2020)
colnames(Dec_2020)
colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)

# Checking for consistent and right data types for each dataframe
str(June_2020)
str(July_2020)
str(Aug_2020)
str(Sept_2020)
str(Oct_2020)
str(Nov_2020)
str(Dec_2020)
str(Jan_2021)
str(Feb_2021)
str(Mar_2021)
str(Apr_2021)
str(May_2021)

#########################################################

#I am going to convert the data type of "start_station_id" and "end_station_id" to character type so it is consistent across all dataframes.

June_2020 <- mutate(June_2020, start_station_id = as.character(start_station_id)
             , end_station_id = as.character(end_station_id))

July_2020 <- mutate(July_2020, start_station_id = as.character(start_station_id)
             , end_station_id = as.character(end_station_id))

Aug_2020 <- mutate(Aug_2020, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Sept_2020 <- mutate(Sept_2020, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Oct_2020 <- mutate(Oct_2020, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Nov_2020 <- mutate(Nov_2020, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Dec_2020 <- mutate(Dec_2020, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Jan_2021 <- mutate(Jan_2021, start_station_id = as.character(start_station_id)
                    , end_station_id = as.character(end_station_id))

Feb_2021 <- mutate(Feb_2021, start_station_id = as.character(start_station_id)
                   , end_station_id = as.character(end_station_id))

Mar_2021 <- mutate(Mar_2021, start_station_id = as.character(start_station_id)
                   , end_station_id = as.character(end_station_id))

Apr_2021 <- mutate(Apr_2021, start_station_id = as.character(start_station_id)
                   , end_station_id = as.character(end_station_id))

May_2021 <- mutate(May_2021, start_station_id = as.character(start_station_id)
                   , end_station_id = as.character(end_station_id))

#########################################################

# We can now stack the data frames into one big data frame for the Cyclistic calendar year

all_months <- bind_rows(June_2020, July_2020, Aug_2020, Sept_2020, Oct_2020, Nov_2020, Dec_2020, Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021)

#########################################################

# For more granular analysis, I am going to create new columns out of the "started_At" column (day, month, year and day of the week).

all_months$Date <- as.Date(all_months$started_at, "%d/%m/%Y")
all_months$day <- format(as.Date(all_months$Date), "%d")
all_months$month <- format(as.Date(all_months$Date), "%m")
all_months$year <- format(as.Date(all_months$Date), "%Y")
all_months$day_of_week <- format(as.Date(all_months$Date), "%A")

#########################################################

all_months$ride_length_secs <- difftime(all_months$ended_at,all_months$started_at)

is.character(all_months$ride_length_secs)
all_months$ride_length_secs <- as.numeric(as.character(all_months$ride_length_secs))

is.numeric(all_months$ride_length_secs) #This gives us a true output, signifying that the ride_length in seconds is formatted to numeric.



# We see that the data contains a few hundred entries where bikes were taken out of docks and checked for quality, this gives us negative vallues for time difference (ride_length_secs). To remove this:

all_months_new <- all_months[!(all_months$start_station_name=="HQ QR"|all_months$ride_length_secs<0),]



#########################################################

# QUICK DESCRIPTIVE ANALYSIS ON THE CLEANED DATA SO FAR

summary(all_months)

# Comparing casual and member users with ride length

aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual, FUN = mean)
aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual, FUN = median)
aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual, FUN = max)
aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual, FUN = min)

# Looking at the Average ride time by each day for members and casual users.

aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual + all_months_new$day_of_week, FUN = mean)

# To display the above in an orderly manner by day of the week.

all_months_new$day_of_week <- ordered(all_months_new$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Looking at weekday data by type

all_months_new %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%  # This creates a weekday field using wday()
  group_by (member_casual, weekday) %>% # This groups the output by user type and weekday.
  summarise(number_of_Rides = n(), average_duration = mean(ride_length_hrs)) %>%  #Calculates the number of rides and avg. duration
  arrange(member_casual, weekday) # This sorts the output.
  

#########################################################
# COMPUTING VISUALIZATIONS
#########################################################

# Viz for the number of rides by rider type for each day.

all_months_new %>% 
  mutate(weekday=wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), total_duration = sum(ride_length_hrs)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclistic Bike Sharing: Daily Total Number of Rides by Usertype"
       ,subtitle = "This shows the total ride time for each day of the week by usertype."
       ,caption = "This is a case study for the Google Data Analytics Certification Course Project")

# Viz for average duration.
all_months_new %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length_hrs)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge") 

####################################################################
# EXPORTING SUMMARY FILES FOR FURTHER ANALYSIS AND VISUALIZATION
####################################################################

# 1 # Showing the average time of rides each day of the week by member type: This can help estimate out what day of the week riders love to ride the longest across usertype.

counts <- aggregate(all_months_new$ride_length_hrs ~ all_months_new$member_casual+all_months_new$day_of_week, FUN = mean)

## Setting the working directory for "counts" data frame.

working_dir <- "C:/Users/HP/Documents/R/Ready for viz/Google Data Analytics Capstone Project - Cyclistic Bike Share Customer Analysis"
setwd(working_dir)
write.csv(counts, file = "Average_Ride_Length_Hrs.csv")
getwd()

# 2a # Showing a count of members and casual riders for the calender year.

member_casual_count_year <- all_months_new %>% 
  count(member_casual, sort = TRUE)

write.csv(member_casual_count_year, file = "Number_Of_Customers_by_Usertype_year.csv")

# 2b # Showing a count of members and casual riders for each month.

member_casual_count_months <- all_months_new %>% 
  count(month, member_casual, sort = TRUE) %>% 
  group_by(month, member_casual)

write.csv(member_casual_count_year, file = "Number_Of_Customers_by_Usertype_month.csv")

# 3 # To export the whole stacked table (all_months_new) as a .CSV file for further analysis in SQL

write.csv(all_months_new, file = "Full_Cyclistic_Table_Cleaned.csv", row.names=F)

all_months_new



aggregate(all_months$ride_length_hrs ~ all_months$member_casual+all_months$day_of_week, FUN = sum)




