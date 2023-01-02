# DATA CLEANING

#libraries
library(readxl)
library(tidyverse)
library(DataExplorer)
library(mapview)
library(gridExtra) #grid.arrange function
library(mosaic) #derivedFactor function

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

# changing class of some columns for all datasets
change_col <- function(df){
  df$AirQualityStationType <- as.factor(df$AirQualityStationType)
  df$AirQualityStationArea <- as.factor(df$AirQualityStationArea)
  df$Longitude <- as.numeric(df$Longitude)
  df$Latitude <- as.numeric(df$Latitude)
  return(df)
}

bap <- change_col(bap)
no2 <- change_col(no2)
o3 <- change_col(o3)
pm10 <- change_col(pm10)
pm25 <- change_col(pm25)
so2 <- change_col(so2)

# visualize the location of the data
mapview(pm10, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(pm25, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(o3, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(no2, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(so2, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(bap, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)


# bar char for the frequencies of the countries
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

dim(ita_bap) # 144 12
dim(ita_no2) # 603 12
dim(ita_o3) # 340 12
dim(ita_pm10) # 525 12
dim(ita_pm25) # 269 12
dim(ita_so2) # 224 12

# we will select pm10, pm2.5, o3, no2 since we have more data
remove(ita_bap, ita_so2)
# and remove some initial datasets as they're now useless
remove(bap, so2)

# keeping only necessary variables
ita_pm10 <- ita_pm10 %>% 
  select(c(AirPollutionLevel, Longitude, Latitude))
ita_pm25 <- ita_pm25 %>% 
  select(c(AirPollutionLevel, Longitude, Latitude))
ita_o3 <- ita_o3 %>% 
  select(c(AirPollutionLevel, Longitude, Latitude))
ita_no2 <- ita_no2 %>% 
  select(c(AirPollutionLevel, Longitude, Latitude))

# Distribution of Air Pollution in Italy ----------
par(mfrow = c(2,2))
boxplot(ita_pm10$AirPollutionLevel, main = "PM10 [ug/m3]")
boxplot(ita_pm25$AirPollutionLevel, main = "PM2.5 [ug/m3]")
boxplot(ita_o3$AirPollutionLevel, main = "O3 [ug/m3]")
boxplot(ita_no2$AirPollutionLevel, main = "NO2 [ug/m3]")

# classifying observation based on North, Center, South
ita_pm10 <- ita_pm10 %>% mutate(
  Zone = derivedFactor(
    "North" = Latitude > 44,
    "Center" = Latitude <= 44 & Latitude > 41.4,
    "South" = Latitude <= 41.4,
    .method = "first",
    .default = 0
  )
)
ita_pm25 <- ita_pm25 %>% mutate(
  Zone = derivedFactor(
    "North" = Latitude > 44,
    "Center" = Latitude <= 44 & Latitude > 41.4,
    "South" = Latitude <= 41.4,
    .method = "first",
    .default = 0
  )
)
ita_no2 <- ita_no2 %>% mutate(
  Zone = derivedFactor(
    "North" = Latitude > 44,
    "Center" = Latitude <= 44 & Latitude > 41.4,
    "South" = Latitude <= 41.4,
    .method = "first",
    .default = 0
  )
)
ita_o3 <- ita_o3 %>% mutate(
  Zone = derivedFactor(
    "North" = Latitude > 44,
    "Center" = Latitude <= 44 & Latitude > 41.4,
    "South" = Latitude <= 41.4,
    .method = "first",
    .default = 0
  )
)

## PLOT POLLUTION FOR EACH POLLUTANT ---------------

# PM10 plot
plot_pm10 <- ita_pm10 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel, shape = Zone)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "PM10 [ug/m3]")

# PM25 plot
plot_pm25 <- ita_pm25 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel, shape = Zone)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "PM2.5 [ug/m3]")

# O3 plot
plot_o3 <- ita_o3 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel, shape = Zone)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "O3 [ug/m3]")

# NO2 plot
plot_no2 <- ita_no2 %>%
  ggplot(aes(x = Longitude, y = Latitude, colour = AirPollutionLevel, shape = Zone)) +
  geom_point() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  labs(title = "NO2 [ug/m3]")

grid.arrange(plot_pm10, plot_pm25, plot_o3, plot_no2, nrow = 2)
remove(plot_pm10, plot_pm25, plot_o3, plot_no2)

# saving cleaned data
write.csv(ita_pm10, "data/cleaned/Ita-PM10.csv")
write.csv(ita_o3, "data/cleaned/Ita-O3.csv")
write.csv(ita_no2, "data/cleaned/Ita-NO2.csv")
write.csv(ita_pm25, "data/cleaned/Ita-PM25.csv")

remove(ita_pm10, ita_pm25, ita_o3, ita_no2)

# Causes and Effects Data -----

# for each country we calculate the average air pollution level
avg_pollution_level <- function(df){
  df_avg <- df %>% group_by(Country) %>% 
    summarise(avg_pollution_level = mean(AirPollutionLevel, na.rm = TRUE))
  return(df_avg)
}
pm10_avg <- avg_pollution_level(pm10)
pm25_avg <- avg_pollution_level(pm25)
o3_avg <- avg_pollution_level(o3)
no2_avg <- avg_pollution_level(no2)

# merging data sets with average value 
df1 <- merge(pm10_avg, no2_avg, by = "Country") %>% 
  rename(pm10_avg = avg_pollution_level.x,
         no2_avg = avg_pollution_level.y)
df2 <- merge(df1, o3_avg, all = TRUE) %>% 
  rename(o3_avg = avg_pollution_level)
avg_final <- merge(df2, pm25_avg, all = TRUE) %>% 
  rename(pm25_avg = avg_pollution_level)
remove(df1, df2, pm10_avg, pm25_avg, no2_avg, o3_avg, pm10, pm25, no2, o3)

## AVERAGE AIR POLLUTION LEVEL DISTRIBUTION--------
# checking normality
par(mfrow = c(2,3))
hist(avg_final$pm10_avg, xlab = "PM10 / [ug/m3]", main = "")
boxplot(avg_final$pm10_avg, main = "Air Pollution PM10")
qqnorm(avg_final$pm10_avg, main = "")
qqline(avg_final$pm10_avg, col = "blue", lwd = 0.5)
hist(avg_final$pm25_avg, xlab = "PM2.5 / [ug/m3]", main = "")
boxplot(avg_final$pm25_avg, main = "Air Pollution PM2.5")
qqnorm(avg_final$pm25_avg, main = "")
qqline(avg_final$pm25_avg, col = "blue", lwd = 0.5)
par(mfrow = c(2,3))
hist(avg_final$o3_avg, xlab = "O3 / [ug/m3]", main = "")
boxplot(avg_final$o3_avg, main = "Air Pollution O3")
qqnorm(avg_final$o3_avg, main = "")
qqline(avg_final$o3_avg, col = "blue", lwd = 0.5)
hist(avg_final$no2_avg, xlab = "NO2 / [ug/m3]", main = "")
boxplot(avg_final$no2_avg, main = "Air Pollution NO2")
qqnorm(avg_final$no2_avg, main = "")
qqline(avg_final$no2_avg, col = "blue", lwd = 0.5)

# try box cox with PM10 and O3

# importing Causes ----
causes_nonclean <- read_excel("data/GS's Causes and Consequences of Air Pollution.xlsx",
                     sheet = "causes")

# changing Slovak Republic to Slovakia in causes to match to avg_final row name
causes_nonclean$Country[causes_nonclean$Country == "Slovak Republic"] <- "Slovakia"

causes <- merge(causes_nonclean, avg_final, by = "Country")
remove(causes_nonclean)

#replacing NA in GDP per capita column for Andorra row
causes$`GDP per capita, PPP (current  international $)`[is.na(causes$`GDP per capita, PPP (current  international $)`)] <- 42903.44

write.csv(causes, "data/cleaned/Europe-Causes.csv")
remove(causes, cor_causes)

# importing Effects ----
effects_nonclean <- read_excel("data/GS's Causes and Consequences of Air Pollution.xlsx",
                              sheet = "consequences")
effects <- merge(effects_nonclean, avg_final, by = "Country")
remove(effects_nonclean, avg_final)

write.csv(effects, "data/cleaned/Europe-Effects.csv")
remove(effects, cor_effects)
