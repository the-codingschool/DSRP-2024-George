install.packages("ggpubr")
install.packages("DescTools")
library(tidyr) 
library(tidyverse) 
library(janitor)
library(dplyr) 
library(ggplot2) 
library(ggpubr)
library(DescTools)

#Reading in data, cleaning names, and data summary
data <- read_csv("data/Global Ecological Footprint 2023.csv")
data = as.data.frame(data) |> 
  clean_names(case = "snake") |>
  rename(builtup_land_footprint = built_up_land_14,
         total_consumption_footprint = total_ecological_footprint_consumption,
         crop_land = cropland,
         fishing_land = fishing_ground,
         built_up_land_capacity = built_up_land_20,
         ) |>
  rename_with(~ str_replace(.x, "land", "capacity"), ends_with("land"), ) |>
  rename_with(~ str_replace(.x, "^number_of", "num"), starts_with("number_of"))

data

head(data)
str(data)
names(data)
summary(data)


#Groups countries based on two different definitions of sustainability 
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

ggplot(data, aes(x = category, y = cropland_footprint))+
  geom_boxplot()+
  labs(title = "Cropland Footprint by Category", 
       x = "Category",
       y = "Cropland Footprint")

#Category 1 and 3 have higher cropland footprint

ggplot(data, aes(x = category, y = grazing_footprint))+
  geom_boxplot()+
  ylim(0, 2)
  labs(title = "Grazing Footprint by Category", 
       x = "Category",
       y = "Grazing Footprint")
  
#Category 1 has higher grazing fooprint

ggplot(data, aes(x = category, y = forest_product_footprint))+
  geom_boxplot()+
  labs(title = "Forest Production Footprint by Category", 
       x = "Category",
       y = "Forest Production Footprint")

ggplot(data, aes(x = category, y = fish_footprint))+
  geom_boxplot()+
  labs(title = "Fish Footprint by Category", 
       x = "Category",
       y = "Fish Footprint")

ggplot(data, aes(x = category, y = builtup_land_footprint))+
  geom_boxplot()+
  labs(title = "Built Up Land Footprint by Category", 
       x = "Category",
       y = "Built Up Land Footprint")

ggplot(data, aes(x = category, y = carbon_footprint))+
  geom_boxplot()+
  labs(title = "Carbon Footprint by Category", 
       x = "Category",
       y = "Carbon Land Footprint")

carbon_df <- select(data, category, carbon_footprint)
print(carbon_df)
anova_result_carbon <- aov(carbon_footprint ~ category, data = carbon_df, na.action = na.exclude)
summary(anova_result_carbon)

ScheffeTest(anova_result_biocapacity)
#2-1,3-1,4-1

#cropland, grazing, carbon*


#Region in each group
ggplot(data, aes(x = category, fill = region)) +
  geom_bar()

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

cat1_count <- cat1 %>%
  count(income_group) %>%
  rename(value = n)

cat1_pie <- ggplot(cat1_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 1") +
  scale_fill_manual(values = custom_colors)

cat2_count <- cat2 %>%
  count(income_group) %>%
  rename(value = n)

cat2_pie <- ggplot(cat2_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 2") +
  scale_fill_manual(values = custom_colors)

cat3_count <- cat3 %>%
  count(income_group) %>%
  rename(value = n)

cat3_pie <- ggplot(cat3_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 3") +
  scale_fill_manual(values = custom_colors)

cat4_count <- cat4 %>%
  count(income_group) %>%
  rename(value = n)

cat4_pie <- ggplot(cat4_count, aes(x = "", y = value, fill = income_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Category 4") +
  scale_fill_manual(values = custom_colors)


figure <- ggarrange(cat1_pie, cat2_pie, cat3_pie, cat4_pie,
                    ncol = 2, nrow = 2)
figure

#data$per_capita_gdp <- gsub(",", "", data$per_capita_gdp)
#data$per_capita_gdp <- gsub("\\$", "", data$per_capita_gdp)
#data = mutate(data, per_capita_gdp = as.numeric(per_capita_gdp))