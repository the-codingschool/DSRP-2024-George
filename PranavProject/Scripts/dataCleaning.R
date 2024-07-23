## library(readr)
## library(data.table)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

## read in dataset
fileName <- list.files("data")
data <- read.csv(paste0("data/", fileName))
head(data)

dim(data)
nrow(data)
ncol(data)

str(clean_data)

clean_data <- clean_names(data)

summarize(clean_data)
summarize(data)

get_dupes(clean_data, "country")

sum(is.na(clean_data))

ggplot(data = clean_data, aes(x = "", y = total_ecological_footprint_consumption)) +
  geom_boxplot() +
  labs(title = "Total Ecological Footprint Consumption",
       x = "",
       y = "Consumption")

ggplot(data = clean_data, aes(x = "", y = cropland)) +
  geom_boxplot() +
  labs(title = "Cropland",
       x = "",
       y = "land")

ggplot(data = clean_data, aes(x = per_capita_gdp, y = cropland_footprint)) +
  geom_point() +
  labs(title = "GDP Per Capita vs. Cropland Footprint",
       x = "GDP per capita ($)",
       y = "Cropland footprint (hectares per person)")

clean_data$per_capita_gdp

ggplot(data = clean_data, aes(x = per_capita_gdp, y = number_of_earths_required)) +
  geom_point() +
  labs(title = "GDP Per Capita vs. Number of Earths Required",
       x = "GDP per capita ($)",
       y = "Earths Required")
