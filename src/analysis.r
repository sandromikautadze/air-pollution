# STATISTICAL ANALYSIS

library(tidyverse)
library(broom)

# importing cleaned data
PM10 <- read_csv("data/cleaned/Ita-PM10.csv") %>% select(-1)
PM25 <- read_csv("data/cleaned/Ita-PM25.csv") %>% select(-1)
O3 <- read_csv("data/cleaned/Ita-O3.csv") %>% select(-1)
NO2 <- read_csv("data/cleaned/Ita-NO2.csv") %>% select(-1)


# ----
# Is there enough statistical evidence to show that Italy's pollution levels
# exceed the EU guidelines? What about the North, Center and South of Italy?

# Let mu0 be the EU reference guideline for a specific pollutant 
# H0: mu <= mu0 vs H1 : mu > mu0, T-test, alpha0 = 0.05

# Entire Italy 

test <- function(df, mu0){
  tt <- t.test(df$AirPollutionLevel,
         alternative = "greater",
         mu = mu0)
  return(tt)
}

tt_pm10 <- test(df = PM10, mu0 = 50)
tt_pm25 <- test(df = PM25, mu0 = 25)
tt_o3 <- test(df = O3, mu0 = 120)
tt_no2 <- test(df = NO2, mu0 = 40)

pollutant <- c("PM10", "PM2.5", "O3", "NO2")
result <- c("Retain H0", "Retain H0", "Retain H0", "Retain H0")
summary_italy <- cbind(pollutant,
                 rbind(tidy(tt_pm10), tidy(tt_pm25), tidy(tt_o3), tidy(tt_no2)),
                 result)

remove(tt_pm10, tt_pm25, tt_o3, tt_no2)

# Northern, Central, Southern Italy for each pollutant

# PM10
pm10_north <- PM10 %>% filter(Zone == "North")
pm10_center <- PM10 %>% filter(Zone == "Center")
pm10_south <- PM10 %>% filter(Zone == "South")

tt_pm10_north <- test(df = pm10_north, mu0 = 50)
tt_pm10_center <- test(df = pm10_center, mu0 = 50)
tt_pm10_south <- test(df = pm10_south, mu0 = 50)

zones <- c("North", "Center", "South")
result <- c("Retain H0", "Retain H0", "Retain H0")
summary_pm10 <- cbind(zones,
                      rbind(tidy(tt_pm10_north), tidy(tt_pm10_center), tidy(tt_pm10_south)),
                      result)
remove(tt_pm10_north, tt_pm10_center, tt_pm10_south, pm10_north, pm10_center, pm10_south)

# PM2.5
pm25_north <- PM25 %>% filter(Zone == "North")
pm25_center <- PM25 %>% filter(Zone == "Center")
pm25_south <- PM25 %>% filter(Zone == "South")

tt_pm25_north <- test(df = pm25_north, mu0 = 25)
tt_pm25_center <- test(df = pm25_center, mu0 = 25)
tt_pm25_south <- test(df = pm25_south, mu0 = 25)

summary_pm25 <- cbind(zones,
                      rbind(tidy(tt_pm25_north), tidy(tt_pm25_center), tidy(tt_pm25_south)),
                      result)

remove(tt_pm25_north, tt_pm25_center, tt_pm25_south, pm25_north, pm25_center, pm25_south)

# O3
o3_north <- O3 %>% filter(Zone == "North")
o3_center <- O3 %>% filter(Zone == "Center")
o3_south <- O3 %>% filter(Zone == "South")

tt_o3_north <- test(df = o3_north, mu0 = 120)
tt_o3_center <- test(df = o3_center, mu0 = 120)
tt_o3_south <- test(df = o3_south, mu0 = 120)

res_o3 <- c("Reject H0", "Retain H0", "Retain H0")
summary_o3 <- cbind(zones,
                    rbind(tidy(tt_o3_north), tidy(tt_o3_center), tidy(tt_o3_south)),
                    res_o3)

remove(tt_o3_north, tt_o3_center, tt_o3_south, o3_north, o3_center, o3_south)

# NO2
no2_north <- NO2 %>% filter(Zone == "North")
no2_center <- NO2 %>% filter(Zone == "Center")
no2_south <- NO2 %>% filter(Zone == "South")

tt_no2_north <- test(df = no2_north, mu0 = 40)
tt_no2_center <- test(df = no2_center, mu0 = 40)
tt_no2_south <- test(df = no2_south, mu0 = 40)

summary_no2 <- cbind(zones,
                    rbind(tidy(tt_no2_north), tidy(tt_no2_center), tidy(tt_no2_south)),
                    result)

remove(tt_no2_north, tt_no2_center, tt_no2_south, no2_north, no2_center, no2_south)


# Causes vs Air Pollution Levels ----
##
# Air Pollution Levels vs Effects ----
##


