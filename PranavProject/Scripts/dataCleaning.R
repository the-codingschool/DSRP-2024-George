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


high_cprint <- filter(clean_data, carbon_footprint > 5)
high_cprint$country

ggplot(data = clean_data, aes(x = carbon_footprint, y = number_of_earths_required)) +
  geom_point() +
  labs(title = "Carbon Footprint vs Number of Earths Required",
       x = "Carbon Footprint",
       y = "Earths Required")

ggplot(data = clean_data, aes(x = total_ecological_footprint_consumption, y = number_of_earths_required)) +
  geom_point() +
  labs(title = "Total Footprint vs Number of Earths Required",
       x = "Total Footprint by Country",
       y = "Earths Required")


ggplot(data = clean_data, aes(x = income_group, y = total_ecological_footprint_consumption)) +
  geom_bar(stat = "identity") +
  labs(title = "consumption by income group",
       x = "income group",
       y = "consumption")

ggplot(data = clean_data, aes(x = population_millions, y = total_biocapacity)) +
  geom_point() +
  labs(title = "Population vs Total Biocapacity",
       x = "Population",
       y = "Biocapacity")

ggplot(data = clean_data, aes(x = per_capita_gdp, y = total_biocapacity)) +
  geom_point() +
  labs(title = "GDP Per Capita vs Total Biocapacity",
       x = "GDP Per capita",
       y = "Biocapacity")


# trying to make gdp numeric ####
str <- "$623,000"
as.numeric(paste(substr(str, 2, nchar(str) - 4), substr(str, nchar(str) - 2, nchar(str)), sep = ""))
?paste

gdps <- clean_data$per_capita_gdp

clean_data$per_capita_gdp
gdps

gdps[2]

gdp_per_cap_num = as.numeric(paste(substr(gdps, 2, nchar(gdps) - 4), substr(gdps, nchar(gdps) - 2, nchar(gdps)), sep = ""))

new_data <- mutate(clean_data, gpd_per_cap_num = as.numeric(paste(substr(per_capita_gdp, 2, nchar(per_capita_gdp) - 4), substr(per_capita_gdp, nchar(per_capita_gdp) - 2, nchar(per_capita_gdp)), sep = "")))

new_data$gpd_per_cap_num

clean_data$per_capita_gdp <- gsub(",", "", clean_data$per_capita_gdp)
clean_data$per_capita_gdp <- gsub("\\$", "", clean_data$per_capita_gdp)
clean_data = mutate(clean_data, per_capita_gdp = as.numeric(per_capita_gdp))

clean_data$per_capita_gdp

#####

