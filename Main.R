install.packages("ggpubr")

library(tidyr) 
library(tidyverse) 
library(janitor)
library(dplyr) 
library(ggplot2) 
library(ggpubr)

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


#Groups countries based on sustainability 
data <- mutate(data, category = case_when(num_countries_required < 1 & num_earths_required > 1 ~ "1",
                                  num_countries_required < 1 & num_earths_required < 1 ~ "2", 
                                  num_countries_required > 1 & num_earths_required > 1 ~ "3",
                                  num_countries_required > 1 & num_earths_required < 1 ~ "4"))

cat1 <- filter(data, category == "1") 
cat2 <- filter(data, category == "2")
cat3 <- filter(data, category == "3")
cat4 <- filter(data, category == "4")

cat1 #sustainable, but mainly due to higher resource availability
print(range(cat1$total_consumption_footprint)) # 1.5 8.1
print(range(cat1$total_biocapacity)) #1.718858 85.646110
print(range(cat1$ecological_deficit_or_reserve)) #0.006197676 84.099056890
cat2 #sustainable, normal resource availability
print(range(cat2$total_consumption_footprint)) # 0.6 1.5
print(range(cat2$total_biocapacity)) #1.106373 7.831302
print(range(cat2$ecological_deficit_or_reserve)) #0.06758994 6.71295416
cat3 #unsustainable, normal resource availability
print(range(cat3$total_consumption_footprint)) # 1.5 13.1
print(range(cat3$total_biocapacity)) #0.1041268 5.9670893
print(range(cat3$ecological_deficit_or_reserve)) #-12.08733856  -0.06900837
cat4 #unsustainable, but mainly due to lower resource availability
print(range(cat4$total_consumption_footprint)) # 0.6 1.5
print(range(cat4$total_biocapacity)) #[1] 0.2143352 1.1316615
print(range(cat4$ecological_deficit_or_reserve)) #-1.13994368 -0.01019427

#1.5 is the cutoff between planet sustainability and unsustainability


#Resource usage within each group. 

ggplot(data, aes(x = category, y = cropland_footprint))+
  geom_boxplot()+
  labs(title = "Cropland Footprint by Category", 
       x = "Category",
       y = "Cropland Footprint")

ggplot(data, aes(x = category, y = grazing_footprint))+
  geom_boxplot()+
  labs(title = "Grazing Footprint by Category", 
       x = "Category",
       y = "Grazing Footprint")

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
  labs(title = "Built Up Land Footprint by Category", 
       x = "Category",
       y = "Built Up Land Footprint")

#cropland, grazing, carbon*


#Income in each group 
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