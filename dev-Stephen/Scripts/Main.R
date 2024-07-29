#Loading in libraries
library(dplyr)
library(ggplot2) 
library(ggpubr)
library(DescTools)
library(janitor)
library(stringr)

#Reading in data, cleaning names, and data summary
data <- read.csv("data/Global Ecological Footprint 2023.csv")
data = as.data.frame(data) |> 
  clean_names(case = "snake") |>
  rename(builtup_land_footprint = built_up_land,
         total_consumption_footprint = total_ecological_footprint_consumption,
         crop_land = cropland,
         fishing_land = fishing_ground,
         builtup_land_capacity = built_up_land_1,
         ) |>
  rename_with(~ str_replace(.x, "land", "capacity"), ends_with("land"), ) |>
  rename_with(~ str_replace(.x, "^number_of", "num"), starts_with("number_of"))

data

#Groups countries based on different groups of sustainability 
data <- mutate(data, category = case_when(num_countries_required < 1 & num_earths_required > 1 ~ "1",
                                  num_countries_required < 1 & num_earths_required < 1 ~ "2", 
                                  num_countries_required > 1 & num_earths_required > 1 ~ "3",
                                  num_countries_required > 1 & num_earths_required < 1 ~ "4"))

ggplot(data, aes(x = category, y = num_countries_required)) + 
  geom_boxplot()+
  ylim(0, 20) +
  labs(title = "Number of Countries Required by Category", 
       x = "Category",
       y = "Number of Countries")

num_countries_df <- select(data, category, num_countries_required)
anova_result_countries <- aov(num_countries_required ~ category, data = num_countries_df, na.action = na.exclude)
summary(anova_result_countries)

ScheffeTest(anova_result_countries)
#3-1, 3-2

ggplot(data, aes(x = category, y = num_earths_required)) + 
  geom_boxplot()+
  labs(title = "Number of Earths Required by Category", 
       x = "Category",
       y = "Number of Countries")

num_earth_df <- select(data, category, num_earths_required)
anova_result_earth <- aov(num_earths_required ~ category, data = num_earth_df, na.action = na.exclude)
summary(anova_result_earth)

ScheffeTest(anova_result_earth)
#2-1, 4-1, 3-2, 4-3

ggplot(data, aes(x = category, y = ecological_deficit_or_reserve)) + 
  geom_boxplot()+
  labs(title = "Ecological Deficit or Reserve by Category", 
       x = "Category",
       y = "Ecological Deficit Or Reserve")

ecological_df <- select(data, category, ecological_deficit_or_reserve)
anova_result_ecological <- aov(ecological_deficit_or_reserve ~ category, data = ecological_df, na.action = na.exclude)
summary(anova_result_ecological)

ScheffeTest(anova_result_ecological)
#2-1, 3-1, 4-1

ggplot(data, aes(x = category, y = total_consumption_footprint)) + 
  geom_boxplot()+
  labs(title = "Total Biocapacity Required by Category", 
       x = "Category",
       y = "Number of Countries")

consumption_df <- select(data, category, total_consumption_footprint)
anova_result_consumption <- aov(total_consumption_footprint ~ category, data = consumption_df, na.action = na.exclude)
summary(anova_result_consumption)

ScheffeTest(anova_result_consumption)
#2-1, 4-1, 3-2, 4-3

ggplot(data, aes(x = category, y = total_biocapacity)) + 
  geom_boxplot()+
  labs(title = "Total Biocapacity Required by Category", 
       x = "Category",
       y = "Number of Countries")

biocapacity_df <- select(data, category, total_biocapacity)
anova_result_biocapacity <- aov(total_biocapacity ~ category, data = biocapacity_df, na.action = na.exclude)
summary(anova_result_biocapacity)

ScheffeTest(anova_result_biocapacity)
#2-1, 3-1, 4-1

#1.5 is the cutoff between planet sustainability and unsustainability




#Resource usage within each group. 

#Cropland
ggplot(data, aes(x = category, y = cropland_footprint))+
  geom_boxplot()+
  labs(title = "Cropland Footprint by Category", 
       x = "Category",
       y = "Cropland Footprint")

cropland_df <- select(data, category, cropland_footprint)
anova_result_crop <- aov(cropland_footprint ~ category, data = cropland_df, na.action = na.exclude)
summary(anova_result_crop)

ScheffeTest(anova_result_crop)
#2-1,4-1,3-2,4-3

#Grazing
ggplot(data, aes(x = category, y = grazing_footprint))+
  geom_boxplot()+
  ylim(0, 2)
  labs(title = "Grazing Footprint by Category", 
       x = "Category",
       y = "Grazing Footprint")
  
grazing_df <- select(data, category, grazing_footprint)
anova_result_grazing <- aov(grazing_footprint ~ category, data = grazing_df, na.action = na.exclude)
summary(anova_result_grazing)
  
ScheffeTest(anova_result_land)  
#4-3

#Forest
ggplot(data, aes(x = category, y = forest_product_footprint))+
  geom_boxplot()+
  labs(title = "Forest Production Footprint by Category", 
       x = "Category",
       y = "Forest Production Footprint")

forest_df <- select(data, category, forest_product_footprint)
anova_result_forest <- aov(forest_product_footprint ~ category, data = forest_df, na.action = na.exclude)
summary(anova_result_forest)

ScheffeTest(anova_result_forest)
#2-1, 3-1, 4-1

#Fish
ggplot(data, aes(x = category, y = fish_footprint))+
  geom_boxplot()+
  labs(title = "Fish Footprint by Category", 
       x = "Category",
       y = "Fish Footprint")

fish_df <- select(data, category, fish_footprint)
anova_result_fish <- aov(fish_footprint ~ category, data = fish_df, na.action = na.exclude)
summary(anova_result_fish)
#None

#Built Up Land
ggplot(data, aes(x = category, y = builtup_land_footprint))+
  geom_boxplot()+
  labs(title = "Built Up Land Footprint by Category", 
       x = "Category",
       y = "Built Up Land Footprint")

land_df <- select(data, category, builtup_land_footprint)
anova_result_land <- aov(builtup_land_footprint ~ category, data = land_df, na.action = na.exclude)
summary(anova_result_land)

ScheffeTest(anova_result_land)
#4-3

#Carbon
ggplot(data, aes(x = category, y = carbon_footprint))+
  geom_boxplot()+
  labs(title = "Carbon Footprint by Category", 
       x = "Category",
       y = "Carbon Land Footprint")

carbon_df <- select(data, category, carbon_footprint)
anova_result_carbon <- aov(carbon_footprint ~ category, data = carbon_df, na.action = na.exclude)
summary(anova_result_carbon)

ScheffeTest(anova_result_carbon)
#2-1, 4-1, 3-2, 4-3

#Most important: Cropland, Forest, Carbon
#Important: Grazing, Built Up Land
#Not Important: Fish 



#Income in each group 
cat1 <- filter(data, category == "1") 
cat2 <- filter(data, category == "2")
cat3 <- filter(data, category == "3")
cat4 <- filter(data, category == "4")

cat1 #num_countries <1 (sustainable) and num_earth >1 (unsustainable)
cat2 #num_countries <1 (sustainable) and num_earth <1 (sustainable)
cat3 #num_countries >1 (unsustainable) and num_earth >1 (unsustainable)
cat4 #num_countries >1 (unsustainable) and num_earth <1 (sustainable)

custom_colors <- c("HI" = "red", "LI" = "blue", "LM" = "green", "UM" = "orange")

#Category 1 
cat1_count <- cat1 %>%
  count(income_group) %>%
  rename(value = n)

cat1_pie <- ggplot(cat1_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 1") +
  scale_fill_manual(values = custom_colors)

#Category 2
cat2_count <- cat2 %>%
  count(income_group) %>%
  rename(value = n)

cat2_pie <- ggplot(cat2_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 2") +
  scale_fill_manual(values = custom_colors)

#Category 3
cat3_count <- cat3 %>%
  count(income_group) %>%
  rename(value = n)

cat3_pie <- ggplot(cat3_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 3") +
  scale_fill_manual(values = custom_colors)

#Category 4 
cat4_count <- cat4 %>%
  count(income_group) %>%
  rename(value = n)

cat4_pie <- ggplot(cat4_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 4") +
  scale_fill_manual(values = custom_colors)

#Stacked bar chart
income_bar <- ggplot(data, aes(x = category, fill = income_group)) + 
  geom_bar(position = "stack")

figure <- ggarrange(cat1_pie, cat2_pie, cat3_pie, cat4_pie, income_bar,
                    ncol = 2, nrow = 3)
figure

fisher.test(contingency_table, workspace = 2e9)

contingency_table <- table(data$category, data$income_group)
contingency_table
fisher_test <- fisher.test(contingency_table, workspace = 2e8)
fisher_test


#Category 1 (num_earth > 1) - UM, HI, LM, (few UM)
#Category 3 (num_earth > 1)- HI, UM LM, (few UM)

#Category 2 (num_earth < 1) - LI, LM
#Category 3 (num_earth < 1) - LI, LM, (few UM)


#Connection with footprints

#Cropland connection with income
ggplot(data, aes(x = income_group, y = cropland_footprint))+
  geom_boxplot()

income_crop_df <- select(data, income_group, cropland_footprint)
income_crop <- aov(cropland_footprint ~ income_group, data = income_crop_df, na.action = na.exclude)
summary(income_crop)

ScheffeTest(income_crop)
#LI-HI, LM-HI, UM-HI, UM-LI, UM-LM

#Forest connection with income
ggplot(data, aes(x = income_group, y = forest_product_footprint))+
  geom_boxplot()

income_forest_df <- select(data, income_group, forest_product_footprint)
income_forest <- aov(forest_product_footprint ~ income_group, data = income_forest_df, na.action = na.exclude)
summary(income_forest)

ScheffeTest(income_forest)
#LI-HI, LM-HI, UM-HI

#Carbon connection with income
ggplot(data, aes(x = income_group, y = carbon_footprint))+
  geom_boxplot()

income_carbon_df <- select(data, income_group, carbon_footprint)
income_carbon <- aov(carbon_footprint ~ income_group, data = income_carbon_df, na.action = na.exclude)
summary(income_carbon)

ScheffeTest(income_carbon)
#LI-HI, LM-HI, UM-HI, UM-LI, UM-LM

#Grazing connection with income
ggplot(data, aes(x = income_group, y = grazing_footprint))+
  geom_boxplot()

income_grazing_df <- select(data, income_group, grazing_footprint)
income_grazing <- aov(grazing_footprint ~ income_group, data = income_grazing_df, na.action = na.exclude)
summary(income_grazing)
#NA

#Built Up Land connection with income
ggplot(data, aes(x = income_group, y = builtup_land_footprint))+
  geom_boxplot()

income_land_df <- select(data, income_group, builtup_land_footprint)
income_land <- aov(builtup_land_footprint ~ income_group, data = income_land_df, na.action = na.exclude)
summary(income_land)

ScheffeTest(income_land)
#LI-HI

#Fish connection with income
ggplot(data, aes(x = income_group, y = fish_footprint))+
  geom_boxplot()

income_fish_df <- select(data, income_group, fish_footprint)
income_fish <- aov(fish_footprint ~ income_group, data = income_fish_df, na.action = na.exclude)
summary(income_fish)
#NA




#Region in each group
custom_colors <- c("Africa" = "red", "Asia-Pacific" = "blue", "Central America/Caribbean" = "green", "EU-27" = "orange", "North America" = "yellow", "Other Europe" = "pink", "South America" = "purple")

cat1_count <- cat1 %>%
  count(region) %>%
  rename(value = n)

print(cat1_count)

cat1_pie <- ggplot(cat1_count, aes(x = "", y = value, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 1") +
  scale_fill_manual(values = custom_colors)

cat2_count <- cat2 %>%
  count(region) %>%
  rename(value = n)

cat2_pie <- ggplot(cat2_count, aes(x = "", y = value, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 2") +
  scale_fill_manual(values = custom_colors)

cat3_count <- cat3 %>%
  count(region) %>%
  rename(value = n)

cat3_pie <- ggplot(cat3_count, aes(x = "", y = value, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 3") +
  scale_fill_manual(values = custom_colors)

cat4_count <- cat4 %>%
  count(region) %>%
  rename(value = n)

cat4_pie <- ggplot(cat4_count, aes(x = "", y = value, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 4") +
  scale_fill_manual(values = custom_colors)


figure <- ggarrange(cat1_pie, cat2_pie, cat3_pie, cat4_pie,
                    ncol = 2, nrow = 2)
figure

#South America falls mainly in category 1, with 1 exception (Chile)
#Central America falls mainly in category 2 and 3 
#EU-27 falls only in category 1 and 3


#data$per_capita_gdp <- gsub(",", "", data$per_capita_gdp)
#data$per_capita_gdp <- gsub("\\$", "", data$per_capita_gdp)
#data = mutate(data, per_capita_gdp = as.numeric(per_capita_gdp))