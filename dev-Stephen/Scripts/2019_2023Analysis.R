library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)

merge_data23 <- rename_with(data23, ~paste0(. , "23"), .cols = -country)

merge_data19 <-  rename_with(data19, ~paste0(. , "19"), .cols = -country) |>
  subset(select = -data_quality19)

#Merge Datasets
merged_df <- full_join(merge_data23, merge_data19, by = "country")
merged_df
merged_df <- mutate(merged_df, d_per_capita_gdp = (per_capita_gdp23 - per_capita_gdp19)/per_capita_gdp19, 
                    d_num_earths = (num_earths_required23 - num_earths_required19)/num_earths_required19,
                    d_num_countries = (num_countries_required23 - num_countries_required19)/num_countries_required19)
merged_df$d_num_earths[merged_df$d_num_earths == Inf] <- NA
merged_df$d_num_countries[merged_df$d_num_countries == Inf] <- NA


saveRDS(merged_df, "2019_2023Analysis")



#Differences in Between Years

#Measurements
t_test_num_earth_df <- na.omit(select(merged_df, num_earths_required19, num_earths_required23))
num_earth_t_test <- t.test(t_test_num_earth_df$num_earths_required19 , t_test_num_earth_df$num_earths_required23 , paired = TRUE)
num_earth_t_test
#NA

t_test_num_countries_df <- na.omit(select(merged_df, num_countries_required19, num_countries_required23))
num_countries_t_test <- t.test(t_test_num_countries_df$num_countries_required19 , t_test_num_countries_df$num_countries_required23 , paired = TRUE)
num_countries_t_test
#p-value = 0.01398

t_test_fp_df <- na.omit(select(merged_df, total_consumption_footprint19 , total_consumption_footprint23))
fp_t_test <- t.test(t_test_fp_df$total_consumption_footprint19 , t_test_fp_df$total_consumption_footprint23 , paired = TRUE)
fp_t_test
#NA

t_test_cap_df <- na.omit(select(merged_df, total_biocapacity19 , total_biocapacity23))
cap_t_test <- t.test(t_test_cap_df$total_biocapacity19 , t_test_cap_df$total_biocapacity23 , paired = TRUE)
cap_t_test
#p-value = 0.003226

changed_cat <- filter(merged_df, category19 != category23)

#Resources

#Crop
crop_df <- select(merged_df, starts_with("cropland"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
crop_df <- mutate(crop_df, d_cropland_footprint = (cropland_footprint23 - cropland_footprint19)/cropland_footprint19, 
                  d_cropland_capacity = (cropland_capacity23 - cropland_capacity19)/cropland_capacity19)
crop_df$d_cropland_footprint[crop_df$d_cropland_footprint == Inf] <- NA
crop_df$d_cropland_capacity[crop_df$d_cropland_capacity == Inf] <- NA

t_test_crop_df <- na.omit(crop_df)

crop_fp_t_test <- t.test(t_test_crop_df$cropland_footprint19, t_test_crop_df$cropland_footprint19, paired = TRUE)
crop_fp_t_test
crop_cap_t_test <- t.test(t_test_crop_df$cropland_capacity19, t_test_crop_df$cropland_capacity23, paired = TRUE)
crop_cap_t_test

#Grazing
grazing_df <- select(merged_df, starts_with("grazing"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
grazing_df <- mutate(grazing_df, d_grazing_footprint = (grazing_footprint23 - grazing_footprint19)/grazing_footprint19, 
                     d_grazing_capacity = (grazing_capacity23 - grazing_capacity19)/grazing_capacity19)
grazing_df$d_grazing_footprint[grazing_df$d_grazing_footprint == Inf] <- NA
grazing_df$d_grazing_capacity[grazing_df$d_grazing_capacity == Inf] <- NA

t_test_grazing_df <- na.omit(grazing_df)

grazing_fp_t_test <- t.test(t_test_grazing_df$grazing_footprint19 , t_test_grazing_df$grazing_footprint23 , paired = TRUE)
grazing_fp_t_test
grazing_cap_t_test <- t.test(t_test_grazing_df$grazing_capacity19, t_test_grazing_df$grazing_capacity23, paired = TRUE)
grazing_cap_t_test
#p-value = 0.0001616

#Forest
forest_df <- select(merged_df, starts_with("forest"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
forest_df <- mutate(forest_df, d_forest_footprint = (forest_footprint23 - forest_footprint19)/forest_footprint19, 
                    d_forest_capacity = (forest_capacity23 - forest_capacity19)/forest_capacity19)
forest_df$d_forest_footprint[forest_df$d_forest_footprint == Inf] <- NA
forest_df$d_forest_capacity[forest_df$d_forest_capacity == Inf] <- NA

t_test_forest_df <- na.omit(forest_df)

forest_fp_t_test <- t.test(t_test_forest_df$forest_footprint19 , t_test_forest_df$forest_footprint23 , paired = TRUE)
forest_fp_t_test

forest_cap_t_test <- t.test(t_test_forest_df$forest_capacity19, t_test_forest_df$forest_capacity23, paired = TRUE)
forest_cap_t_test
#p-value = 0.005889

#Fish
fish_df <- select(merged_df, starts_with("fish"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
fish_df <- mutate(fish_df, d_fish_footprint = (fish_footprint23 - fish_footprint19)/fish_footprint19, 
                  d_fish_capacity = (fish_capacity23 - fish_capacity19)/fish_capacity19)
fish_df$d_fish_footprint[fish_df$d_fish_footprint == Inf] <- NA
fish_df$d_fish_capacity[fish_df$d_fish_capacity == Inf] <- NA

t_test_fish_df <- na.omit(fish_df)

fish_fp_t_test <- t.test(t_test_fish_df$fish_footprint19 , t_test_fish_df$fish_footprint23 , paired = TRUE)
fish_fp_t_test
fish_cap_t_test <- t.test(t_test_fish_df$fish_capacity19, t_test_fish_df$fish_capacity23, paired = TRUE)
fish_cap_t_test
#p-value = 0.000235

#Land 
builtup_land_df <- select(merged_df, starts_with("builtup"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
builtup_land_df <- mutate(builtup_land_df, d_land_footprint = (builtup_land_footprint23 - builtup_land_footprint19)/builtup_land_footprint19, 
                          d_land_capacity = (builtup_land_capacity23 - builtup_land_capacity19)/builtup_land_capacity19)
builtup_land_df$d_land_footprint[builtup_land_df$d_land_footprint == Inf] <- NA
builtup_land_df$d_land_capacity[builtup_land_df$d_land_capacity == Inf] <- NA

t_test_land_df <- na.omit(builtup_land_df)

land_fp_t_test <- t.test(t_test_land_df$builtup_land_footprint19 , t_test_land_df$builtup_land_footprint23 , paired = TRUE)
land_fp_t_test
land_cap_t_test <- t.test(t_test_land_df$built_land_capacity19, t_test_land_df$builtup_land_capacity23, paired = TRUE)
land_cap_t_test

#Carbon 
carbon_df <- select(merged_df, starts_with("carbon"), d_per_capita_gdp, category23, d_num_earths, d_num_countries)
carbon_df <- mutate(carbon_df, d_carbon_footprint = (carbon_footprint23 - carbon_footprint19)/carbon_footprint19)
carbon_df$d_carbon_footprint[carbon_df$d_carbon_footprint == Inf] <- NA

t_test_carbon_df <- na.omit(carbon_df)

carbon_fp_t_test <- t.test(t_test_carbon_df$carbon_footprint19 , t_test_carbon_df$carbon_footprint23 , paired = TRUE)
carbon_fp_t_test
#p-value = 0.03826



#Economic Changes

t_test_gdp_df <- na.omit(select(merged_df, per_capita_gdp19, per_capita_gdp23))
gdp_t_test <- t.test(t_test_gdp_df$per_capita_gdp19 , t_test_gdp_df$per_capita_gdp23 , paired = TRUE)
gdp_t_test
#p-value = 0.01752

#Crop Changes

ggplot(crop_df, aes(x = d_per_capita_gdp, y = d_cropland_footprint, color = category23))+
  geom_point()+
  labs(title = "Per Capita GDP Growth Vs Cropland Footprint Growth")+
  xlim(-0.25,0.25)

crop_fp_test <- cor.test(crop_df$d_per_capita_gdp, crop_df$d_cropland_footprint, use = "complete.obs")
print(crop_fp_test)
#NA

ggplot(crop_df, aes(x = d_per_capita_gdp, y = d_cropland_capacity, color = category23))+
  geom_point()+
  xlim(-0.25,0.25)

crop_cap_test <- cor.test(crop_df$d_per_capita_gdp, crop_df$d_cropland_capacity, use = "complete.obs")
print(crop_cap_test)
#NA

#Grazing Changes

ggplot(grazing_df, aes(x = d_per_capita_gdp, y = d_grazing_footprint, color = category23))+
  geom_point()+
  xlim(-0.25,0.25)

grazing_fp_test <- cor.test(grazing_df$d_per_capita_gdp, grazing_df$d_grazing_footprint, use = "complete.obs")
grazing_fp_test
#NA

ggplot(grazing_df, aes(x = d_per_capita_gdp, y = d_grazing_capacity, color = category23))+
  geom_point()+
  xlim(-0.25,0.25)

grazing_cap_test <- cor.test(grazing_df$d_per_capita_gdp, grazing_df$d_grazing_capacity, use = "complete.obs")
grazing_cap_test
#NA

#Forest Changes

ggplot(forest_df, aes(x = d_per_capita_gdp, y = d_forest_footprint, color = category23))+
  geom_point()+
  xlim(-0.25,0.25)

forest_fp_test <- cor.test(forest_df$d_per_capita_gdp, forest_df$d_forest_footprint, use = "complete.obs")
forest_fp_test
#NA

ggplot(forest_df, aes(x = d_per_capita_gdp, y = d_forest_capacity, color = category23))+
  geom_point()+
  xlim(-0.25,0.25)

forest_cap_test <- cor.test(forest_df$d_per_capita_gdp, forest_df$d_forest_capacity, use = "complete.obs")
forest_cap_test
#NA

#Fish Changes

ggplot(fish_df, aes(x = d_per_capita_gdp, y = d_fish_footprint, color = category23))+
  geom_point() +
  xlim(-0.25,0.25)

fish_fp_test <- cor.test(fish_df$d_per_capita_gdp, fish_df$d_fish_footprint, use = "complete.obs")
fish_fp_test
#NA

ggplot(fish_df, aes(x = d_per_capita_gdp, y = d_fish_capacity, color = category23))+
  geom_point() +
  xlim(-0.25,0.25)

fish_cap_test <- cor.test(fish_df$d_per_capita_gdp, fish_df$d_fish_capacity, use = "complete.obs")
fish_cap_test
#NA

#Built up Land Changes

ggplot(builtup_land_df, aes(x = d_per_capita_gdp, y = d_land_footprint, color = category23))+
  geom_point() +
  xlim(-0.25,0.25)

land_fp_test <- cor.test(builtup_land_df$d_per_capita_gdp, builtup_land_df$d_land_footprint, use = "complete.obs")
land_fp_test
#NA

ggplot(builtup_land_df, aes(x = d_per_capita_gdp, y = d_land_capacity, color = category23))+
  geom_point() +
  labs(title = "Per Capita GDP Growth Vs Built up Land Capacity Growth")+
  xlim(-0.25,0.25)+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

land_cap_test <- cor.test(builtup_land_df$d_per_capita_gdp, builtup_land_df$d_land_capacity, use = "complete.obs")
land_cap_test
#0.03824

#Carbon Changes

ggplot(carbon_df, aes(x = d_per_capita_gdp, y = d_carbon_footprint, color = category23))+
  geom_point() +
  xlim(-0.25,0.25)

carbon_fp_test <- cor.test(carbon_df$d_per_capita_gdp, carbon_df$d_carbon_footprint, use = "complete.obs")
carbon_fp_test
#NA



#Sustainability changes

#Crop
ggplot(crop_df, aes(x = d_cropland_footprint, d_num_earths, color = category23))+
  geom_point()+
  labs(title = "Change in Num Earths Required vs Change in Cropland Footprint")+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

crop_fp_earth_test <- cor.test(crop_df$d_cropland_footprint, crop_df$d_num_earths, use = "complete.obs")
crop_fp_earth_test
#p-value = 3.836e-05

ggplot(crop_df, aes(x = d_cropland_footprint, d_num_countries, color = category23))+
  labs(title = "Change in Num Countries Required vs Change in Cropland Footprint")+
  geom_point()

crop_fp_country_test <- cor.test(crop_df$d_cropland_footprint, crop_df$d_num_countries, use = "complete.obs")
crop_fp_country_test
#NA

ggplot(crop_df, aes(x = d_cropland_capacity, d_num_earths, color = category23))+
  geom_point()+
  labs(title = "Change in Num Earths Required vs Change in Cropland Capacity")+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

crop_cap_earth_test <- cor.test(crop_df$d_cropland_capacity, crop_df$d_num_earths, use = "complete.obs")
crop_cap_earth_test
#p-value = 0.0005279

ggplot(crop_df, aes(x = d_cropland_capacity, d_num_countries, color = category23))+
  geom_point()+
  labs(title = "Change in Num Countries Required vs Change in Cropland Capacity")+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

crop_cap_countries_test <- cor.test(crop_df$d_cropland_capacity, crop_df$d_num_countries, use = "complete.obs")
crop_cap_countries_test
#p-value = 0.03994

#Grazing
ggplot(grazing_df, aes(x = d_grazing_footprint, d_num_earths, color = category23))+
  geom_point()

grazing_fp_earth_test <- cor.test(grazing_df$d_grazing_footprint, grazing_df$d_num_earths, use = "complete.obs")
grazing_fp_earth_test
#NA

ggplot(grazing_df, aes(x = d_grazing_footprint, d_num_countries, color = category23))+
  geom_point()

grazing_fp_countries_test <- cor.test(grazing_df$d_grazing_footprint, grazing_df$d_num_countries, use = "complete.obs")
grazing_fp_countries_test
#NA

ggplot(grazing_df, aes(x = d_grazing_capacity, d_num_earths, color = category23))+
  geom_point()

grazing_cap_earth_test <- cor.test(grazing_df$d_grazing_capacity, grazing_df$d_num_earths, use = "complete.obs")
grazing_cap_earth_test
#NA

ggplot(grazing_df, aes(x = d_grazing_capacity, d_num_countries, color = category23))+
  geom_point()

grazing_cap_countries_test <- cor.test(grazing_df$d_grazing_capacity, grazing_df$d_num_countries, use = "complete.obs")
grazing_cap_countries_test
#NA

#Forest
ggplot(forest_df, aes(x = d_forest_footprint, d_num_earths, color = category23))+
  geom_point()

forest_fp_earth_test <- cor.test(forest_df$d_forest_footprint, forest_df$d_num_earths, use = "complete.obs")
forest_fp_earth_test
#NA

ggplot(forest_df, aes(x = d_forest_footprint, d_num_countries, color = category23))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

forest_fp_countries_test <- cor.test(forest_df$d_forest_footprint, forest_df$d_num_countries, use = "complete.obs")
forest_fp_countries_test
#p-value = 0.0234

ggplot(forest_df, aes(x = d_forest_capacity, d_num_earths, color = category23))+
  geom_point()

forest_cap_earth_test <- cor.test(forest_df$d_forest_capacity, forest_df$d_num_earths, use = "complete.obs")
forest_cap_earth_test
#NA

ggplot(forest_df, aes(x = d_forest_capacity, d_num_countries, color = category23))+
  geom_point()

forest_cap_countries_test <- cor.test(forest_df$d_forest_capacity, forest_df$d_num_countries, use = "complete.obs")
forest_cap_countries_test
#NA

#Fish
ggplot(fish_df, aes(x = d_fish_footprint, d_num_earths, color = category23))+
  geom_point()

fish_fp_earth_test <- cor.test(fish_df$d_fish_footprint, fish_df$d_num_earths, use = "complete.obs")
fish_fp_earth_test
#NA

ggplot(fish_df, aes(x = d_fish_footprint, d_num_countries, color = category23))+
  geom_point()

fish_fp_countries_test <- cor.test(fish_df$d_fish_footprint, fish_df$d_num_countries, use = "complete.obs")
fish_fp_countries_test
#NA

ggplot(fish_df, aes(x = d_fish_capacity, d_num_earths, color = category23))+
  geom_point()

fish_cap_earth_test <- cor.test(fish_df$d_fish_capacity, fish_df$d_num_earths, use = "complete.obs")
fish_cap_earth_test
#NA

ggplot(fish_df, aes(x = d_fish_capacity, d_num_countries, color = category23))+
  geom_point()

fish_cap_countries_test <- cor.test(fish_df$d_fish_capacity, fish_df$d_num_countries, use = "complete.obs")
fish_cap_countries_test
#NA

#Built up Land
builtup_land_df
ggplot(builtup_land_df, aes(x = d_land_footprint, d_num_earths, color = category23))+
  geom_point()

land_fp_earth_test <- cor.test(builtup_land_df$d_land_footprint, builtup_land_df$d_num_earths, use = "complete.obs")
land_fp_earth_test
#NA

ggplot(builtup_land_df, aes(x = d_land_footprint, d_num_countries, color = category23))+
  geom_point()

land_fp_countries_test <- cor.test(builtup_land_df$d_land_footprint, builtup_land_df$d_num_countries, use = "complete.obs")
land_fp_countries_test
#NA

ggplot(builtup_land_df, aes(x = d_land_capacity, d_num_earths, color = category23))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")

land_cap_earth_test <- cor.test(builtup_land_df$d_land_capacity, builtup_land_df$d_num_earths, use = "complete.obs")
land_cap_earth_test
#p-value = 0.0004447

ggplot(builtup_land_df, aes(x = d_land_capacity, d_num_countries, color = category23))+
  geom_point()

land_cap_countries_test <- cor.test(builtup_land_df$d_land_capacity, builtup_land_df$d_num_countries, use = "complete.obs")
land_cap_countries_test
#NA