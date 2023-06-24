# Loading Packages --------------------------------------------------------
library(tidyverse)
library(scales)

# Importing Datasets ------------------------------------------------------
jan22 <- read_csv("csv_files/202201-divvy-tripdata.csv")
feb22 <- read_csv("csv_files/202202-divvy-tripdata.csv")
mar22 <- read_csv("csv_files/202203-divvy-tripdata.csv")
apr22 <- read_csv("csv_files/202204-divvy-tripdata.csv")
may22 <- read_csv("csv_files/202205-divvy-tripdata.csv")
jun22 <- read_csv("csv_files/202206-divvy-tripdata.csv")
jul22 <- read_csv("csv_files/202207-divvy-tripdata.csv")
aug22 <- read_csv("csv_files/202208-divvy-tripdata.csv")
sep22 <- read_csv("csv_files/202209-divvy-tripdata.csv")
oct22 <- read_csv("csv_files/202210-divvy-tripdata.csv")
nov22 <- read_csv("csv_files/202211-divvy-tripdata.csv")
dec22 <- read_csv("csv_files/202212-divvy-tripdata.csv")

# Inspecting column names and datatypes -------------------------------------
glimpse(jul22) # variable data types do not match other months
rm(jul22) # Removed from project

# Imported again with appropriate column data types
jul22 <- read_csv("csv_files/202207-divvy-tripdata.csv", 
                  col_types = cols(started_at = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                   ended_at = col_datetime(format = "%m/%d/%Y %H:%M")))
# seconds were not included as all observations result in NA values


# Data Cleaning -----------------------------------------------------------

# Combine all data frames
all_2022 <- bind_rows(jan22, feb22, mar22, apr22, may22, jun22,
                      jul22, aug22, sep22, oct22, nov22, dec22)

# Remove columns with id in name
all_2022_v2 <- all_2022 %>% 
  select(-ends_with('id'))

# Remove duplicates
all_2022_v3 <- all_2022_v2 %>% 
  distinct(started_at, ended_at, .keep_all = TRUE)

# Add a ride_length column
all_2022_v4 <- all_2022_v3 %>% 
  mutate(
    ride_length = ended_at - started_at,
    .before = start_station_name)

# Change ride_length to numeric data type
all_2022_v4$ride_length = as.numeric(as.difftime(all_2022_v4$ride_length))

# Remove negative ride lengths
all_2022_v4 %>% 
  arrange(ride_length)

all_2022_v5 <- all_2022_v4 %>% 
  filter(ride_length >= 0)

# Find day of week from starting_at date
all_2022_v6 <- all_2022_v5 %>% 
  mutate(day_of_week = (wday(all_2022_v5$started_at,label = TRUE)))

# Arrange, rename, and relocate columns for clarity
all_2022_v7 <- all_2022_v6 %>% 
  arrange(started_at) %>% 
  relocate(member_casual, rideable_type, day_of_week) %>% 
  rename(member_type = member_casual, bike_type = rideable_type)

# Adding and removing additional columns
all_2022_v8 <-  all_2022_v7 %>%
  mutate(
    weekday = (wday(all_2022_v7$started_at,label = TRUE)),
    month = (month(all_2022_v7$started_at, label = TRUE)),
    hour_of_day = (hour(all_2022_v7$started_at))
  ) %>%
  select(-day_of_week, -start_station_name, -end_station_name, 
         -start_lat, -start_lng, -end_lat, -end_lng)

# Remove excess data frames
rm(jan22, feb22, mar22, apr22, may22, june22,
   jul22, aug22, sep22, oct22, nov22, dec22,
   all_2022, all_2022v2, all_2022v3, all_2022v4,
   all_2022v5, all_2022v6)

# Data Analysis -----------------------------------------------------------

# How many rides for the entire year by member type?
all_2022_v8 %>%
  count(member_type) %>% 
  group_by(member_type) %>%
  arrange(n) %>% 
  View()

# How many rides for the entire year by bike type?
all_2022_v7 %>%
  count(member_type, bike_type) %>% 
  group_by(bike_type, member_type) %>%
  arrange(n) %>% 
  View()

# What is the avg ride length (in seconds) per member type?
all_2022_v7 %>% 
  group_by(member_type) %>% 
  summarise(avg_ride_length = mean(ride_length)) %>% 
  View()

# How many rides per month by member type?
all_2022_v8 %>%
  count(month, member_type) %>% 
  group_by(month, member_type) %>%
  #filter(member_type== "member") %>% 
  arrange(desc(n)) %>% 
  View()

# How many rides per weekday by member type?
all_2022_v8 %>%
  count(weekday, member_type) %>% 
  group_by(weekday, member_type) %>%
  #filter(member_type== "member") %>% 
  arrange(desc(n)) %>% 
  View()

# How many rides per hour of day by member type?
all_2022_v8 %>%
  count(hour_of_day, member_type) %>% 
  group_by(hour_of_day, member_type) %>%
  #filter(member_type== "member") %>% 
  arrange(desc(n)) %>% 
  View()

# What station was the most popular by member type?
# Creating first df
start_station_counts_by_member <- all_2022_v7 %>%
  group_by(member_type) %>%
  count(start_station_name) %>%
  rename(station_name = start_station_name) %>% 
  drop_na() %>% 
  arrange(desc(n)) %>%
  filter(n> 19000)

# Creating second df
end_station_counts_by_member <- all_2022_v7 %>%
  group_by(member_type) %>%
  count(end_station_name) %>%
  rename(station_name = end_station_name) %>% 
  drop_na() %>% 
  arrange(desc(n)) %>%
  filter(n> 19000)

# Joining both data frames
joined_station_counts <- merge(start_station_counts_by_member, 
                               end_station_counts_by_member, 
                               by = "station_name") %>% 
  select(station_name, member_type.x, n.x, n.y) %>% 
  mutate(total_count = n.x + n.y) %>%
  rename(member_type = member_type.x) %>% 
  arrange(desc(total_count)) %>% 
  View()

# What is the avg ride length per month?
all_2022_v7 %>%
  mutate(month = (month(all_2022_v7$started_at, label = TRUE, abbr = FALSE))) %>% 
  group_by(month) %>% 
  summarise(avg_bike_length = mean(ride_length)) %>%
  arrange(desc(avg_bike_length)) %>% 
  View()

# What is the avg ride_length per weekday?
all_2022_v7 %>%
  group_by(day_of_week) %>% 
  summarise(avg_bike_length = mean(ride_length)) %>%
  arrange(desc(avg_bike_length)) %>% 
  View()

# What is the avg ride_length per hour of day?
all_2022_v7 %>%
  mutate(hour_of_day = (hour(all_2022_v7$started_at))) %>% 
  group_by(hour_of_day) %>% 
  summarise(avg_bike_length = mean(ride_length)) %>%
  arrange(desc(avg_bike_length)) %>% 
  View()


# Data Visualization ------------------------------------------------------

# Plotting Number of Rides by member type
plot1 <- all_2022_v8 %>%
  count(member_type) %>% 
  group_by(member_type) %>%
  arrange(n) %>% 
  ggplot(aes(x = member_type, y = n, fill = member_type))+
    geom_col(color = "black", width = 0.5, alpha = 0.8)+
  labs(
    title = "Number of Bike Rides",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(labels = label_comma())+
  geom_text(
    aes(label = comma(n)),
    color = "black", size = 4, vjust = -.5
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

# Plotting Number of Bike Rides by bike type
plot2 <- all_2022_v8 %>% 
  group_by(member_type) %>% 
  count(bike_type) %>% 
  ggplot(aes(x = bike_type, y = n, fill = member_type))+
  geom_col(color = "black", position = "dodge", width = 0.5, alpha = 0.8)+
  labs(
    title = "Number of Bike Rides by Bike Type",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(labels = label_comma())+
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 10)
  )

# Plotting Avg Ride Length by member type
plot3 <- all_2022_v8 %>%
  group_by(member_type) %>%
  summarise(avg_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = member_type, y = avg_ride_length, fill = member_type))+
    geom_col(color = "black", width = 0.5, alpha = 0.8)+
  labs(
    title = "Average Bike Ride Length",
    x = NULL,
    y = "time (seconds)"
  )+
  geom_text(
    aes(label = comma(avg_ride_length)),
    color = "black", size = 4, vjust = -.5
  )+
  theme_classic()+
  theme(
    plot.title= element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

# Plotting Number of Bike Rides per Month
plot4 <- all_2022_v8 %>%
  group_by(member_type) %>% 
  count(month) %>% 
  ggplot(aes(x = month, y = n, fill = member_type))+
  geom_col(color= "black",  width=0.5, alpha= 0.8)+
  labs(
    title = "Number of Bike Rides per Month",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(labels = label_comma())+
  theme_classic()+
  theme(
    plot.title= element_text(size= 16, hjust = 0.5),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=10),
    legend.position= "bottom"
  )

# Plotting Number of Bike Rides per Weekday
plot5 <- all_2022_v8 %>%
  group_by(member_type) %>% 
  count(weekday) %>% 
  ggplot(aes(x = weekday, y = n, fill = member_type))+
  geom_col(color= "black",  width = 0.5, alpha= 0.8, position = "dodge")+
  labs(
    title = "Number of Bike Rides per Weekday",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(labels = label_comma())+
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )


# # Plotting Number of Bike Rides per Hour of Day
plot6 <- all_2022_v8 %>%
  group_by(member_type) %>% 
  count(hour_of_day) %>% 
  ggplot(aes(x = hour_of_day, y = n, fill = member_type))+
  geom_col(color= "black",  width =0.5, alpha = 0.8, position = "dodge")+
  labs(
    title = "Number of Bike Rides per Hour of Day",
    x = NULL,
    y = NULL
  )+
  scale_y_continuous(labels = label_comma())+
  scale_x_continuous(breaks = pretty(all_2022_v8$hour_of_day, n = 20))+ # creates ticks marks for all hours
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )


# Plotting Popular Stations by member type
plot7 <- joined_station_counts %>%
  rename(member_type = member_type.x) %>%
  select(station_name, member_type, total_count) %>% 
  arrange(desc(total_count)) %>% 
  ggplot(aes(x = fct_reorder(station_name, total_count), 
             y = total_count, fill = member_type))+
  geom_col(color = "black",  width = 0.5, alpha = 0.8)+
  labs(
    title = "Popular Stations",
    x = NULL,
    y = "number of visits"
  )+
  geom_text(
    aes(label = comma(total_count)),
    color = "black", size = 4.5, vjust = 0.5, hjust = 1.3
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 13),
    legend.position = "right"
  )+
  coord_flip()