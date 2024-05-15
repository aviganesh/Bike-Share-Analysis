#=================
# IMPORT LIBRARIES
#=================

library(tidyverse)
library(readr)
library(ggplot2)

#=====================
# STEP 1: COLLECT DATA
#=====================

# Specify the file path to CSV file
jan2024_data_file_path <- "C:/Users/Owner/Downloads/202312-divvy-tripdata/202312-divvy-tripdata.csv"
feb2024_data_file_path <- "C:/Users/Owner/Downloads/202401-divvy-tripdata/202401-divvy-tripdata.csv"
mar2024_data_file_path <- "C:/Users/Owner/Downloads/202402-divvy-tripdata/202402-divvy-tripdata.csv"
apr2024_data_file_path <- "C:/Users/Owner/Downloads/202403-divvy-tripdata/202403-divvy-tripdata.csv"
may2024_data_file_path <- "C:/Users/Owner/Downloads/202404-divvy-tripdata/202404-divvy-tripdata.csv"

# Read CSV file into a data frame
jan2024_data <- read.csv(jan2024_data_file_path)
feb2024_data <- read.csv(feb2024_data_file_path)
mar2024_data <- read.csv(mar2024_data_file_path)
apr2024_data <- read.csv(apr2024_data_file_path)
may2024_data <- read.csv(may2024_data_file_path)

#Combine into 1 file
trip_data <- bind_rows(jan2024_data, feb2024_data, mar2024_data, apr2024_data, may2024_data)

#=========================================
# STEP 2: CLEAN DATA AND PREP FOR ANALYSIS
#=========================================

# Inspect cleaned data
colnames(trip_data) #List of column names
dim(trip_data) #Dimensions of data frame
head(trip_data) #First 6 rows of data frame
tail(trip_data) #Last 6 rows of data frame
str(trip_data) #Structure of data frame
summary(trip_data) #Statistical summary of data
View(trip_data) #View the data frame

#Check relevant columns to verify that there are the right amount of labels per column for aggregation
levels(as.factor(trip_data$rideable_type))
levels(as.factor(trip_data$member_casual))

#Replace empty strings with NA
trip_data[trip_data == ""] <- NA

#Check how many rows to delete
colSums(is.na(trip_data))

#Keep only the rows with no NA values
trip_datav2 <- trip_data[complete.cases(trip_data), ]

#Verify they are all removed by checking these equal 0
colSums(is.na(trip_datav2))
colSums(trip_datav2 == "")

# Adding additional columns of data (day, month, year) to provide additional opportunities to aggregate data
trip_datav2$date <- as.Date(trip_datav2$started_at)
trip_datav2$month <- format(as.Date(trip_datav2$date), "%m")
trip_datav2$day <- format(as.Date(trip_datav2$date), "%d")
trip_datav2$year <- format(as.Date(trip_datav2$date), "%Y")
trip_datav2$day_of_week <- format(as.Date(trip_datav2$date), "%A")

# Add a column that computes the ride length
trip_datav2$ride_length <- difftime(trip_datav2$ended_at, trip_datav2$started_at)

#Inspect structure of data frame and update ride length to be numeric
str(trip_datav2)
is.factor(trip_datav2$ride_length)
trip_datav2$ride_length <- as.numeric(as.character(trip_datav2$ride_length)) # convert to numeric
is.numeric(trip_datav2$ride_length)
str(trip_datav2)

# Need to delete negative trip duration rows
trip_datav3 <- trip_datav2[!(trip_datav2$ride_length<0), ]

#=====================================
# STEP 3: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Statistical analysis of ride length
summary(trip_datav3$ride_length)

# Compare members and casual users
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual, FUN = mean)
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual, FUN = median)
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual, FUN = max)
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual, FUN = min)

# See average ride time by each day for members vs casual users
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual + trip_datav3$day_of_week, FUN = mean)

#Fix order to make days of the week in order
trip_datav3$day_of_week <- ordered(trip_datav3$day_of_week, levels = c("Sunday", "Monday", "Tuesday", 
                                                                       "Wednesday", "Thursday", "Friday",
                                                                       "Saturday"))
#Re-run aggregate
aggregate(trip_datav3$ride_length ~ trip_datav3$member_casual + trip_datav3$day_of_week, FUN = mean)


# Analyze ridership data by type and weekday
analyze_by_type_weekday <- trip_datav3 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #Creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by user type and weekday
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% #Calculates number of rides and average duration
  arrange(member_casual, weekday) #sorts

View(analyze_by_type_weekday)

#==============================
# STEP 4: CREATE VISUALIZATIONS
#==============================

#Create pie chart to visualize proportion of rides by user type
pie(table(trip_datav3$member_casual), main="Proportion of Rides by User Type", col=c("#FF7F7F", "#008080"))

##############################################################################
# This chart identify composition of Cyclistic's user base. Casual > Members.#
##############################################################################

#Create a bar chart to visualize weekday vs number of rides by user type
analyze_by_type_weekday %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total Rides Per Weekday",
       x = "Weekday",
       y = "Total Rides") +
  theme(plot.title = element_text(hjust = 0.5))
##########################################################################################################
# This chart gives a high level overview of how many rides were taken by each usertype. Members > Casual.#
##########################################################################################################

  
#Create a bar chart to visualize weekday vs average duration by user type
analyze_by_type_weekday %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Trip Duration Per Weekday",
       x = "Weekday",
       y = "Average Trip Duration") +
  theme(plot.title = element_text(hjust = 0.5))

#################################################################################################
# This chart identify differences in average ride duration between user types. Casual > Members.#
#################################################################################################

#Create a bar chart to visualize popular start and end stations

# Create a subset dataframe with start and end station names and their counts
start_stations <- as.data.frame(table(trip_datav3$start_station_name))
colnames(start_stations) <- c("Station", "Count")
end_stations <- as.data.frame(table(trip_datav3$end_station_name))
colnames(end_stations) <- c("Station", "Count")

# Sort the stations by count in descending order
start_stations <- start_stations[order(-start_stations$Count), ]
end_stations <- end_stations[order(-end_stations$Count), ]

# Create a bar plot for popular start stations
ggplot(start_stations[1:10, ], aes(x = reorder(Station, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Popular Start Stations",
       x = "Station",
       y = "Number of Rides") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 9.5))

# Create a bar plot for popular end stations
ggplot(end_stations[1:10, ], aes(x = reorder(Station, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Popular End Stations",
       x = "Station",
       y = "Number of Rides") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 9.5))

###########################################################################################################
# These charts help identify key locations where marketing efforts could be focused to attract customers.##
# Top 8 most popular start and end stations are the same - need to focus on the most popular destinations.# 
###########################################################################################################