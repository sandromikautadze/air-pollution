# DATA CLEANING

#libraries
library(readxl)
library(tidyverse)
library(DataExplorer)
library(gridExtra)

# importing data
pm10 <- read_excel("data/2018 data -EU values.xlsx", sheet = "PM10", skip = 6)
pm25 <- read_excel("data/2018 data -EU values.xlsx", sheet = "PM2.5", skip = 5)
o3 <- read_excel("data/2018 data -EU values.xlsx", sheet = "O3", skip = 7)
no2 <- read_excel("data/2018 data -EU values.xlsx", sheet = "NO2", skip = 5)
bap <- read_excel("data/2018 data -EU values.xlsx", sheet = "BaP", skip = 5)
so2 <- read_excel("data/2018 data -EU values.xlsx", sheet = "SO2", skip = 5)

# EXPLORATORY ANALYSIS----------
colnames(pm10)
colnames(pm10) == colnames(pm25)
colnames(pm25) == colnames(o3)
colnames(o3) == colnames(no2)
colnames(no2) == colnames(bap)
colnames(bap) == colnames(so2) 
## all true

# bar char for the frequencies of the countries
par(mfrow = c(3,3))
plot_bar(pm10$Country, title = "PM10")
plot_bar(pm25$Country, title = "PM2.5")
plot_bar(o3$Country, title = "O3")
plot_bar(no2$Country, title = "NO2")
plot_bar(bap$Country, title = "BaP")
plot_bar(so2$Country, title = "SO2")

# select only Italy
ita_pm10 <- pm10 %>% filter(Country == "Italy")
ita_pm25 <- pm25 %>% filter(Country == "Italy")
ita_o3 <- o3 %>% filter(Country == "Italy")
ita_no2 <- no2 %>% filter(Country == "Italy")
ita_bap <- bap %>% filter(Country == "Italy")
ita_so2 <- so2 %>% filter(Country == "Italy")

#remove original datasets since they are useless now
remove(bap, no2, o3, pm10, pm25, so2)

# changing class of some columns for all datasets
change_col <- function(df){
  df$AirQualityStationType <- factor(df$AirQualityStationType)
  df$AirQualityStationArea <- factor(df$AirQualityStationArea)
  df$Longitude <- as.numeric(df$Longitude)
  df$Latitude <- as.numeric(df$Latitude)
  return(df)
}

ita_bap <- change_col(ita_bap)
ita_no2 <- change_col(ita_no2)
ita_o3 <- change_col(ita_o3)
ita_pm10 <- change_col(ita_pm10)
ita_pm25 <- change_col(ita_pm25)
ita_so2 <- change_col(ita_so2)

# we will select pm10, pm2.5, o3, no2 since we have more data
remove(ita_bap, ita_so2)


# PLOTS-------

# PM10 plot
plot_pm10 <- ita_pm10 %>%
    ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel)) +
    geom_point() +
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
    labs(title = "PM10")

# PM25 plot
plot_pm25 <- ita_pm25 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "PM25")

# O3 plot
plot_o3 <- ita_o3 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "O3")

# NO2 plot
plot_no2 <- ita_no2 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "NO2")


# POLLUTION FOR EACH POLLUTANT
grid.arrange(plot_pm10, plot_pm25, plot_o3, plot_no2, nrow = 2)
