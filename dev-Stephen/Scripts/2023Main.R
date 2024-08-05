#Loading in libraries
library(dplyr)
library(ggplot2) 
library(ggpubr)
library(DescTools)
library(janitor)
library(stringr)

#Reading in data, cleaning names, and data summary
data23 <- read.csv("data/Global Ecological Footprint 2023.csv")

data23 <- as.data.frame(data23) |> 
  clean_names(case = "snake") |>
  rename(builtup_land_footprint = built_up_land,
         total_consumption_footprint = total_ecological_footprint_consumption,
         forest_footprint = forest_product_footprint,
         cropland_capacity = cropland,
         fish_land = fishing_ground,
         builtup_land_capacity = built_up_land_1,
         ) |>
  rename_with(~ str_replace(.x, "land", "capacity"), ends_with("land"), ) |>
  rename_with(~ str_replace(.x, "^number_of", "num"), starts_with("number_of")) 
data23$per_capita_gdp <- gsub(",", "", data23$per_capita_gdp)
data23$per_capita_gdp <- gsub("\\$", "", data23$per_capita_gdp)
data23 <- mutate(data23, per_capita_gdp = as.numeric(per_capita_gdp))

#Groups countries based on different groups of sustainability 
data23 <- mutate(data23, category = case_when(num_countries_required <= 1 & num_earths_required > 1 ~ "1",
                                  num_countries_required <= 1 & num_earths_required <= 1 ~ "2", 
                                  num_countries_required > 1 & num_earths_required > 1 ~ "3",
                                  num_countries_required > 1 & num_earths_required <= 1 ~ "4"))

data23

saveRDS(data23, "2023Main")

num_countries23 <- ggplot(data23, aes(x = category, y = num_countries_required)) + 
  geom_boxplot()+
  ylim(0, 20) +
  labs(title = "Number of Countries Required by Category", 
       x = "Category",
       y = "Number of Countries")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "3"), 
    y = c(1.8, 18)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("2", "3"), 
    y = c(1.8, 19)
  )

num_countries_df <- select(data23, category, num_countries_required)
anova_result_countries <- aov(num_countries_required ~ category, data = num_countries_df, na.action = na.exclude)
summary(anova_result_countries)

ScheffeTest(anova_result_countries)
#3-1, 3-2

num_earths23 <- ggplot(data23, aes(x = category, y = num_earths_required)) + 
  geom_boxplot()+
  ylim(0, 11)+
  labs(title = "Number of Earths Required by Category", 
       x = "Category",
       y = "Number of Countries")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(6, 1.5)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(6.5, 1.3)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("2", "3"), 
    y = c(2, 9.5)
  )+
  annotate(
    "text", label = "*",
    color = "purple",
    size = 10, 
    x = c("3", "4"), 
    y = c(10, 1.8)
  )

num_earth_df <- select(data23, category, num_earths_required)
anova_result_earth <- aov(num_earths_required ~ category, data = num_earth_df, na.action = na.exclude)
summary(anova_result_earth)

ScheffeTest(anova_result_earth)
#2-1, 4-1, 3-2, 4-3

fp23 <- ggplot(data23, aes(x = category, y = total_consumption_footprint)) + 
  geom_boxplot()+
  ylim(0, 16)+
  labs(title = "Total Consumption Footprint by Category", 
       x = "Category",
       y = "Total Consumption Footprint")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(9, 2.5)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(9.75, 2.5)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("2", "3"), 
    y = c(3.25, 14)
  )+
  annotate(
    "text", label = "*",
    color = "purple",
    size = 10, 
    x = c("3", "4"), 
    y = c(15, 3.25)
  )

consumption_df <- select(data23, category, total_consumption_footprint)
anova_result_consumption <- aov(total_consumption_footprint ~ category, data = consumption_df, na.action = na.exclude)
summary(anova_result_consumption)

ScheffeTest(anova_result_consumption)
#2-1, 4-1, 3-2, 4-3

cap23 <- ggplot(data23, aes(x = category, y = total_biocapacity)) + 
  geom_boxplot()+
  labs(title = "Total Biocapacity Required by Category", 
       x = "Category",
       y = "Total Biocapacity")+ 
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(25, 15)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "3"), 
    y = c(30, 15)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "4"), 
    y = c(35, 15)
  )

biocapacity_df <- select(data23, category, total_biocapacity)
anova_result_biocapacity <- aov(total_biocapacity ~ category, data = biocapacity_df, na.action = na.exclude)
summary(anova_result_biocapacity)

ScheffeTest(anova_result_biocapacity)
#2-1, 3-1, 4-1

plots23 <- annotate_figure(ggarrange(num_countries23, num_earths23, fp23, cap23,
                                     ncol = 2, nrow = 2), top = text_grob("2023"))
plots23



#Resource usage and availability within each group. 

#Cropland
ggplot(data23, aes(x = category, y = cropland_footprint))+
  geom_boxplot()+
  labs(title = "Cropland Footprint by Category", 
       x = "Category",
       y = "Cropland Footprint")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(2, 1)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(2.25, 1)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("2", "3"), 
    y = c(1.25, 2.25)
  )+
  annotate(
    "text", label = "*",
    color = "purple",
    size = 10, 
    x = c("3", "4"), 
    y = c(2.5, 1.25)
  )

crop_fp_df <- select(data23, category, cropland_footprint)
anova_result_crop_fp <- aov(cropland_footprint ~ category, data = crop_fp_df, na.action = na.exclude)
summary(anova_result_crop_fp)

ScheffeTest(anova_result_crop_fp)
#2-1, 4-1, 3-2, 4-3

ggplot(data23, aes(x = category, y = cropland_capacity))+
  geom_boxplot()+
  labs(title = "Cropland Capacity by Category", 
       x = "Category",
       y = "Cropland Capacity")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(3.5, 1)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(4, 1.3)
  )

crop_cap_df <- select(data23, category, cropland_capacity)
anova_result_crop_cap <- aov(cropland_capacity ~ category, data = crop_cap_df, na.action = na.exclude)
summary(anova_result_crop_cap)

ScheffeTest(anova_result_crop_cap)
#2-1, 4-1

#Grazing
ggplot(data23, aes(x = category, y = grazing_footprint))+
  geom_boxplot()+
  labs(title = "Grazing Footprint by Category", 
       x = "Category",
       y = "Grazing Footprint")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(1.75, 1)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(1.85, .75)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "3"), 
    y = c(1.95, .85)
  )
  
grazing_fp_df <- select(data23, category, grazing_footprint)
anova_result_grazing_fp <- aov(grazing_footprint ~ category, data = grazing_fp_df, na.action = na.exclude)
summary(anova_result_grazing_fp)
  
ScheffeTest(anova_result_grazing_fp)  
#2-1, 3-1, 4-1

ggplot(data23, aes(x = category, y = grazing_capacity))+
  geom_boxplot()+
  labs(title = "Grazing Capacity by Category", 
       x = "Category",
       y = "Cropland Capacity")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(7.5, 3)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(8, 1)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "3"), 
    y = c(8.5, 2.25)
  )

grazing_cap_df <- select(data23, category, grazing_capacity)
anova_result_grazing_cap <- aov(grazing_capacity ~ category, data = grazing_cap_df, na.action = na.exclude)
summary(anova_result_grazing_cap)

ScheffeTest(anova_result_grazing_cap)
#2-1, 3-1, 4-1

#Forest
ggplot(data23, aes(x = category, y = forest_footprint))+
  geom_boxplot()+
  labs(title = "Forest Footprint by Category", 
       x = "Category",
       y = "Forest Footprint")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(4.75, 1)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(5, .6)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "3"), 
    y = c(5.25, 1.6)
  )

forest_fp_df <- select(data23, category, forest_footprint)
anova_result_forest_fp <- aov(forest_footprint ~ category, data = forest_fp_df, na.action = na.exclude)
summary(anova_result_forest_fp)

ScheffeTest(anova_result_forest_fp)
#2-1, 3-1, 4-1

ggplot(data23, aes(x = category, y = forest_capacity))+
  geom_boxplot()+
  labs(title = "Forest Production Capacity by Category", 
       x = "Category",
       y = "Forest Production Capacity")+
  ylim(0, 20)+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(15, 6.5)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(16, 1)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "3"), 
    y = c(17, 3)
  )

forest_cap_df <- select(data23, category, forest_capacity)
anova_result_forest_cap <- aov(forest_capacity ~ category, data = forest_cap_df, na.action = na.exclude)
summary(anova_result_forest_cap)

ScheffeTest(anova_result_forest_cap)
#2-1, 3-1, 4-1

#Fish
ggplot(data23, aes(x = category, y = fish_footprint))+
  geom_boxplot()+
  labs(title = "Fish Footprint by Category", 
       x = "Category",
       y = "Fish Footprint")

fish_fp_df <- select(data23, category, fish_footprint)
anova_result_fish_fp <- aov(fish_footprint ~ category, data = fish_fp_df, na.action = na.exclude)
summary(anova_result_fish_fp)
#None

ggplot(data23, aes(x = category, y = fish_capacity))+
  geom_boxplot()+
  labs(title = "Fish Capacity by Category", 
       x = "Category",
       y = "Fish Capacity")+
  ylim(0,8)+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(7, 2)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(7.5, .75)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("1", "3"), 
    y = c(8, 2.5)
  )

fish_cap_df <- select(data23, category, fish_capacity)
anova_result_fish_cap <- aov(fish_capacity ~ category, data = fish_cap_df, na.action = na.exclude)
summary(anova_result_fish_cap)

ScheffeTest(anova_result_fish_cap)
#2-1, 3-1, 4-1

#Built Up Land
ggplot(data23, aes(x = category, y = builtup_land_footprint))+
  geom_boxplot()+
  labs(title = "Built Up Land Footprint by Category", 
       x = "Category",
       y = "Built Up Land Footprint")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("3", "4"), 
    y = c(0.55, 0.2)
  )

land_fp_df <- select(data23, category, builtup_land_footprint)
anova_result_land_fp <- aov(builtup_land_footprint ~ category, data = land_fp_df, na.action = na.exclude)
summary(anova_result_land_fp)

ScheffeTest(anova_result_land_fp)
#4-3

ggplot(data23, aes(x = category, y = builtup_land_capacity))+
  geom_boxplot()+
  labs(title = "Built Up Land Capacity by Category", 
       x = "Category",
       y = "Built Up Land Capacity")+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("2", "3"), 
    y = c(0.25, 0.6)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("3", "4"), 
    y = c(0.65, 0.15)
  )

land_cap_df <- select(data23, category, builtup_land_capacity)
anova_result_land_cap <- aov(builtup_land_capacity ~ category, data = land_cap_df, na.action = na.exclude)
summary(anova_result_land_cap)

ScheffeTest(anova_result_land_cap)
#3-2, 4-3

#Carbon
ggplot(data23, aes(x = category, y = carbon_footprint))+
  geom_boxplot()+
  labs(title = "Carbon Footprint by Category", 
       x = "Category",
       y = "Carbon Land Footprint")+
  ylim(0, 14)+
  annotate(
    "text", label = "*",
    color = "red",
    size = 10, 
    x = c("1", "2"), 
    y = c(6, 1)
  )+
  annotate(
    "text", label = "*",
    color = "blue",
    size = 10, 
    x = c("1", "4"), 
    y = c(6.5, 1.5)
  )+
  annotate(
    "text", label = "*",
    color = "green",
    size = 10, 
    x = c("2", "3"), 
    y = c(1, 13)
  )+
  annotate(
    "text", label = "*",
    color = "purple",
    size = 10, 
    x = c("3", "4"), 
    y = c(15.5, 2))
    

carbon_df <- select(data23, category, carbon_footprint)
anova_result_carbon <- aov(carbon_footprint ~ category, data = carbon_df, na.action = na.exclude)
summary(anova_result_carbon)

ScheffeTest(anova_result_carbon)
#2-1, 4-1, 3-2, 4-3