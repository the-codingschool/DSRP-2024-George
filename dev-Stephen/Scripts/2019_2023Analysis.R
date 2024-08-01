library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)

merge_data23 <- rename_with(data23, ~paste0(. , "23"), .cols = -country)

merge_data19 <-  rename_with(data19, ~paste0(. , "19"), .cols = -country) |>
  subset(select = -data_quality19)

merge_data23
merge_data19

#Merge Datasets
merged_df <- full_join(merge_data23, merge_data19, by = "country")
merged_df



#Economic Changes
merged_df <- mutate(merged_df, d_per_capita_gdp = per_capita_gdp23/per_capita_gdp19)
filter(merged_df, category19 != category23)

#Crop Changes
crop_df <- select(merged_df, starts_with("cropland"), d_per_capita_gdp, category19, category23)
crop_df <- mutate(crop_df, d_cropland_footprint = cropland_footprint23/cropland_footprint19, 
                  d_cropland_capacity = cropland_capacity23/cropland_capacity19)
crop_df$d_cropland_footprint[crop_df$d_cropland_footprint == Inf] <- NA
crop_df$d_cropland_capacity[crop_df$d_cropland_capacity == Inf] <- NA

ggplot(crop_df, aes(x = d_per_capita_gdp, y = d_cropland_footprint, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

crop_fp_test <- cor.test(crop_df$d_per_capita_gdp, crop_df$d_cropland_footprint, use = "complete.obs")
print(crop_fp_test)
#NA

ggplot(crop_df, aes(x = d_per_capita_gdp, y = d_cropland_capacity, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

crop_cap_test <- cor.test(crop_df$d_per_capita_gdp, crop_df$d_cropland_capacity, use = "complete.obs")
print(crop_cap_test)
#NA

#Grazing Changes
grazing_df <- select(merged_df, starts_with("grazing"), d_per_capita_gdp, category19, category23)
grazing_df <- mutate(grazing_df, d_grazing_footprint = grazing_footprint23/grazing_footprint19, 
                  d_grazing_capacity = grazing_capacity23/grazing_capacity19)
grazing_df$d_grazing_footprint[grazing_df$d_grazing_footprint == Inf] <- NA
grazing_df$d_grazing_capacity[grazing_df$d_grazing_capacity == Inf] <- NA

ggplot(grazing_df, aes(x = d_per_capita_gdp, y = d_grazing_footprint, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

grazing_fp_test <- cor.test(grazing_df$d_per_capita_gdp, grazing_df$d_grazing_footprint, use = "complete.obs")
grazing_fp_test
#NA

ggplot(grazing_df, aes(x = d_per_capita_gdp, y = d_grazing_capacity, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

grazing_cap_test <- cor.test(grazing_df$d_per_capita_gdp, grazing_df$d_grazing_capacity, use = "complete.obs")
grazing_cap_test
#NA

#Forest Changes
forest_df <- select(merged_df, starts_with("forest"), d_per_capita_gdp, category19, category23)
forest_df <- mutate(forest_df, d_forest_footprint = forest_footprint23/forest_footprint19, 
                     d_forest_capacity = forest_capacity23/forest_capacity19)
forest_df$d_forest_footprint[forest_df$d_forest_footprint == Inf] <- NA
forest_df$d_forest_capacity[forest_df$d_forest_capacity == Inf] <- NA

ggplot(forest_df, aes(x = d_per_capita_gdp, y = d_forest_footprint, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

forest_fp_test <- cor.test(forest_df$d_per_capita_gdp, forest_df$d_forest_footprint, use = "complete.obs")
forest_fp_test
#NA

ggplot(forest_df, aes(x = d_per_capita_gdp, y = d_forest_capacity, color = category23))+
  geom_point()+
  xlim(0.75, 1.25)

forest_cap_test <- cor.test(forest_df$d_per_capita_gdp, forest_df$d_forest_capacity, use = "complete.obs")
forest_cap_test
#NA

#Fish Changes
fish_df <- select(merged_df, starts_with("fish"), d_per_capita_gdp, category19, category23)
fish_df <- mutate(fish_df, d_fish_footprint = fish_footprint23/fish_footprint19, 
                    d_fish_capacity = fish_capacity23/fish_capacity19)
fish_df$d_fish_footprint[fish_df$d_fish_footprint == Inf] <- NA
fish_df$d_fish_capacity[fish_df$d_fish_capacity == Inf] <- NA

ggplot(fish_df, aes(x = d_per_capita_gdp, y = d_fish_footprint, color = category23))+
  geom_point() +
  xlim(0.75, 1.25)

fish_fp_test <- cor.test(fish_df$d_per_capita_gdp, fish_df$d_fish_footprint, use = "complete.obs")
fish_fp_test
#NA

ggplot(fish_df, aes(x = d_per_capita_gdp, y = d_fish_capacity, color = category23))+
  geom_point() +
  xlim(0.75, 1.25)

fish_cap_test <- cor.test(fish_df$d_per_capita_gdp, fish_df$d_fish_capacity, use = "complete.obs")
fish_cap_test
#NA

#Built up Land Changes
builtup_land_df <- select(merged_df, starts_with("builtup"), d_per_capita_gdp, category19, category23)
builtup_land_df <- mutate(builtup_land_df, d_land_footprint = builtup_land_footprint23/builtup_land_footprint19, 
                  d_land_capacity = builtup_land_capacity23/builtup_land_capacity19)
builtup_land_df$d_land_footprint[builtup_land_df$d_land_footprint == Inf] <- NA
builtup_land_df$d_land_capacity[builtup_land_df$d_land_capacity == Inf] <- NA

ggplot(builtup_land_df, aes(x = d_per_capita_gdp, y = d_land_footprint, color = category23))+
  geom_point() +
  xlim(0.75, 1.25)

land_fp_test <- cor.test(builtup_land_df$d_per_capita_gdp, builtup_land_df$d_land_footprint, use = "complete.obs")
land_fp_test
#NA

ggplot(builtup_land_df, aes(x = d_per_capita_gdp, y = d_land_capacity, color = category23))+
  geom_point() +
  xlim(0.75, 1.25)

land_cap_test <- cor.test(builtup_land_df$d_per_capita_gdp, builtup_land_df$d_land_capacity, use = "complete.obs")
land_cap_test
#0.03824

#Carbon Changes
carbon_df <- select(merged_df, starts_with("carbon"), d_per_capita_gdp, category19, category23)
carbon_df <- mutate(carbon_df, d_carbon_footprint = carbon_footprint23/carbon_footprint19)
carbon_df$d_carbon_footprint[carbon_df$d_carbon_footprint == Inf] <- NA

ggplot(carbon_df, aes(x = d_per_capita_gdp, y = d_carbon_footprint, color = category23))+
  geom_point() +
  xlim(0.75, 1.25)

carbon_fp_test <- cor.test(carbon_df$d_per_capita_gdp, carbon_df$d_carbon_footprint, use = "complete.obs")
carbon_fp_test
#NA