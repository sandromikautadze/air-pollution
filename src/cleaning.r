# DATA CLEANING

#libraries
library(readxl)
library(tidyverse)

# importing data
pm10 <- read_excel("data/2018 data -EU values.xlsx", sheet = "PM10", skip = 6)
pm25 <- read_excel("data/2018 data -EU values.xlsx", sheet = "PM2.5", skip = 5)
o3 <- read_excel("data/2018 data -EU values.xlsx", sheet = "O3", skip = 7)
no2 <- read_excel("data/2018 data -EU values.xlsx", sheet = "NO2", skip = 5)
bap <- read_excel("data/2018 data -EU values.xlsx", sheet = "BaP", skip = 5)
so2 <- read_excel("data/2018 data -EU values.xlsx", sheet = "SO2", skip = 5)

# exploratory analysis of the data

