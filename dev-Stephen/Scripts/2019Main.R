#Loading in libraries
library(dplyr)
library(ggplot2) 
library(ggpubr)
library(DescTools)
library(janitor)
library(stringr)

#Reading in data, cleaning names, and data summary
data <- read.csv("data/Global Ecological Footprint 2019.csv")
data = as.data.frame(data) |> 
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
data$per_capita_gdp <- gsub(",", "", data$per_capita_gdp)
data$per_capita_gdp <- gsub("\\$", "", data$per_capita_gdp)
data <- mutate(data, per_capita_gdp = as.numeric(per_capita_gdp))

#Groups countries based on different groups of sustainability 
data <- mutate(data, category = case_when(num_countries_required <= 1 & num_earths_required > 1 ~ "1",
                                          num_countries_required <= 1 & num_earths_required <= 1 ~ "2", 
                                          num_countries_required > 1 & num_earths_required > 1 ~ "3",
                                          num_countries_required > 1 & num_earths_required <= 1 ~ "4"))
data

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




#Resource usage and availability within each group. 

#Cropland
ggplot(data, aes(x = category, y = cropland_footprint))+
  geom_boxplot()+
  labs(title = "Cropland Footprint by Category", 
       x = "Category",
       y = "Cropland Footprint")

crop_fp_df <- select(data, category, cropland_footprint)
anova_result_crop_fp <- aov(cropland_footprint ~ category, data = crop_fp_df, na.action = na.exclude)
summary(anova_result_crop_fp)

ScheffeTest(anova_result_crop_fp)
#2-1, 4-1, 3-2, 4-3

ggplot(data, aes(x = category, y = cropland_capacity))+
  geom_boxplot()+
  labs(title = "Cropland Capacity by Category", 
       x = "Category",
       y = "Cropland Capacity")

crop_cap_df <- select(data, category, cropland_capacity)
anova_result_crop_cap <- aov(cropland_capacity ~ category, data = crop_cap_df, na.action = na.exclude)
summary(anova_result_crop_cap)

ScheffeTest(anova_result_crop_cap)
#2-1, 4-1, 3-1(New)

#Grazing
ggplot(data, aes(x = category, y = grazing_footprint))+
  geom_boxplot()+
  ylim(0, 2)+
  labs(title = "Grazing Footprint by Category", 
     x = "Category",
     y = "Grazing Footprint")

grazing_fp_df <- select(data, category, grazing_footprint)
anova_result_grazing_fp <- aov(grazing_footprint ~ category, data = grazing_fp_df, na.action = na.exclude)
summary(anova_result_grazing_fp)

ScheffeTest(anova_result_grazing_fp)  
#2-1, 3-1, 4-1

ggplot(data, aes(x = category, y = grazing_capacity))+
  geom_boxplot()+
  labs(title = "Grazing Capacity by Category", 
       x = "Category",
       y = "Cropland Capacity")

grazing_cap_df <- select(data, category, grazing_capacity)
anova_result_grazing_cap <- aov(grazing_capacity ~ category, data = grazing_cap_df, na.action = na.exclude)
summary(anova_result_grazing_cap)

ScheffeTest(anova_result_grazing_cap)
#2-1, 3-1, 4-1

#Forest
ggplot(data, aes(x = category, y = forest_footprint))+
  geom_boxplot()+
  labs(title = "Forest Footprint by Category", 
       x = "Category",
       y = "Forest Footprint")

forest_fp_df <- select(data, category, forest_footprint)
anova_result_forest_fp <- aov(forest_footprint ~ category, data = forest_fp_df, na.action = na.exclude)
summary(anova_result_forest_fp)

ScheffeTest(anova_result_forest_fp)
#2-1, 3-1, 4-1

ggplot(data, aes(x = category, y = forest_capacity))+
  geom_boxplot()+
  labs(title = "Forest Production Capacity by Category", 
       x = "Category",
       y = "Forest Production Capacity")

forest_cap_df <- select(data, category, forest_capacity)
anova_result_forest_cap <- aov(forest_capacity ~ category, data = forest_cap_df, na.action = na.exclude)
summary(anova_result_forest_cap)

ScheffeTest(anova_result_forest_cap)
#(No 2-1), 3-1, 4-1

#Fish
ggplot(data, aes(x = category, y = fish_footprint))+
  geom_boxplot()+
  labs(title = "Fish Footprint by Category", 
       x = "Category",
       y = "Fish Footprint")

fish_fp_df <- select(data, category, fish_footprint)
anova_result_fish_fp <- aov(fish_footprint ~ category, data = fish_fp_df, na.action = na.exclude)
summary(anova_result_fish_fp)
#None

ggplot(data, aes(x = category, y = fish_capacity))+
  geom_boxplot()+
  labs(title = "Fish Capacity by Category", 
       x = "Category",
       y = "Fish Capacity")

fish_cap_df <- select(data, category, fish_capacity)
anova_result_fish_cap <- aov(fish_capacity ~ category, data = fish_cap_df, na.action = na.exclude)
summary(anova_result_fish_cap)

ScheffeTest(anova_result_fish_cap)
#2-1, 3-1, 4-1

#Built Up Land
ggplot(data, aes(x = category, y = builtup_land_footprint))+
  geom_boxplot()+
  labs(title = "Built Up Land Footprint by Category", 
       x = "Category",
       y = "Built Up Land Footprint")

land_fp_df <- select(data, category, builtup_land_footprint)
anova_result_land_fp <- aov(builtup_land_footprint ~ category, data = land_fp_df, na.action = na.exclude)
summary(anova_result_land_fp)

ScheffeTest(anova_result_land_fp)
#4-3

ggplot(data, aes(x = category, y = builtup_land_capacity))+
  geom_boxplot()+
  labs(title = "Built Up Land Capacity by Category", 
       x = "Category",
       y = "Built Up Land Capacity")

land_cap_df <- select(data, category, builtup_land_capacity)
anova_result_land_cap <- aov(builtup_land_capacity ~ category, data = land_cap_df, na.action = na.exclude)
summary(anova_result_land_cap)

ScheffeTest(anova_result_land_cap)
#(No 3-2), 4-3

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
#2-1, 4-1, 3-2, 4-3, 3-1*(New)



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
  filter(income_group != "")%>% 
  count(income_group) %>%
  rename(value = n)

cat1_count <- cat1_count %>%
  mutate(value = rev(value), income_group = rev(income_group))

cat1.y.breaks <- (cumsum(cat1_count$value) - cat1_count$value/2)

cat1_pie <- ggplot(cat1_count, aes(x = 1, y = value, fill = income_group)) +
  ggtitle("Category 1 Income Groups") +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  coord_polar(theta = "y") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=cat1.y.breaks, 
    label = cat1_count$value
  )+
  scale_fill_manual(values = custom_colors)

cat1_pie

#Category 2
cat2_count <- cat2 %>%
  filter(income_group != "")%>% 
  count(income_group) %>%
  rename(value = n)

cat2_count <- cat2_count %>%
  mutate(value = rev(value), income_group = rev(income_group))

cat2.y.breaks <- (cumsum(cat2_count$value) - cat2_count$value/2)

cat2_pie <- ggplot(cat2_count, aes(x = 1, y = value, fill = income_group)) +
  ggtitle("Category 2 Income Groups") +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  coord_polar(theta = "y") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=cat2.y.breaks, 
    label = cat2_count$value
  )+
  scale_fill_manual(values = custom_colors)

cat2_pie
#Category 3

cat3_count <- cat3 %>%
  filter(income_group != "")%>% 
  count(income_group) %>%
  rename(value = n)

cat3_count <- cat3_count %>%
  mutate(value = rev(value), income_group = rev(income_group))

cat3.y.breaks <- (cumsum(cat3_count$value) - cat3_count$value/2)

cat3_pie <- ggplot(cat3_count, aes(x = 1, y = value, fill = income_group)) +
  ggtitle("Category 3 Income Groups") +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  coord_polar(theta = "y") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=cat3.y.breaks, 
    label = cat3_count$value
  )+
  scale_fill_manual(values = custom_colors)

cat3_pie
#Category 4 
cat4_count <- cat4 %>%
  filter(income_group != "")%>% 
  count(income_group) %>%
  rename(value = n)

cat4_count <- cat4_count %>%
  mutate(value = rev(value), income_group = rev(income_group))

cat4.y.breaks <- (cumsum(cat4_count$value) - cat4_count$value/2)

cat4_pie <- ggplot(cat4_count, aes(x = 1, y = value, fill = income_group)) +
  ggtitle("Category 4 Income Groups") +
  geom_bar(stat = "identity", color='black') +
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  coord_polar(theta = "y") +
  theme(axis.ticks=element_blank(),  
        axis.title=element_blank(),  
        axis.text.y=element_blank(),
        axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=cat4.y.breaks, 
    label = cat4_count$value
  )+
  scale_fill_manual(values = custom_colors)

cat4_pie
# #Stacked bar chart
# income_bar <- ggplot(data, aes(x = category, fill = income_group)) + 
#   geom_bar(position = "stack")

figure <- ggarrange(cat1_pie, cat2_pie, cat3_pie, cat4_pie,
                    ncol = 2, nrow = 3)
figure

#Category 1 (num_earth > 1) - UM, HI, LM, (few UM)
#Category 3 (num_earth > 1)- HI, UM LM, (few UM)

#Category 2 (num_earth < 1) - LI, LM
#Category 3 (num_earth < 1) - LI, LM, (few UM)

contingency_table <- table(data$category, data$income_group)

category <- unique(na.omit(data$category))
cat_comb <- combn(category, 2)

income <- unique(data$income_group[data$income_group != ""])
income_comb <- combn(income, 2)

p_values <- c()

for (i in 1:6){
  for (j in 1:6){
    cat_row <- cat_comb[, i]
    income_col <- income_comb[, j]
    new_table <- contingency_table[cat_row, income_col]
    result <- fisher.test(new_table)
    p_values <- append(p_values, result$p.value)
  }
}

new_p_values <- p.adjust(p_values, method = "bonferroni")

for (i in 1:6){
  for (j in 1:6){
    index <- (i - 1) * 6 + j
    index_p_value <- new_p_values[index]
    if (index_p_value < 0.05){
      cat_row <- cat_comb[, i]
      income_col <- income_comb[, j]
      d <- paste("Category: ", paste(cat_row, collapse = ", "), "Income: ", paste(income_col, collapse = ", "), "P-value: ", index_p_value)
      print(d)
    }
  }
}

# "Category:  4, 3 Income:  LI, UM P-value:  1.00658739101279e-10"
# "Category:  4, 3 Income:  LI, LM P-value:  0.00525563072410029"
# "Category:  4, 3 Income:  LI, HI P-value:  2.89552200332041e-13"
# "Category:  4, 3 Income:  UM, LM P-value:  0.0212365258609066"
# "Category:  4, 3 Income:  LM, HI P-value:  0.000524388870312732"
# "Category:  4, 1 Income:  LI, UM P-value:  1.80137174458349e-06"
# "Category:  4, 1 Income:  LI, HI P-value:  5.13390947206297e-05"
# "Category:  3, 2 Income:  LI, UM P-value:  1.23922020351727e-06"
# "Category:  3, 2 Income:  LI, HI P-value:  2.31302147457325e-07"
# "Category:  3, 2 Income:  UM, LM P-value:  0.00442818636888718"
# "Category:  3, 2 Income:  LM, HI P-value:  0.00126241765075288"
# "Category:  2, 1 Income:  LI, UM P-value:  0.000281640556240099"
# "Category:  2, 1 Income:  LI, HI P-value:  0.00796107916850951"
#No Change

#Connection with footprints

#Cropland connection with income
ggplot(data, aes(x = income_group, y = cropland_footprint))+
  geom_boxplot()

income_crop_fp_df <- select(data, income_group, cropland_footprint)
income_crop_fp <- aov(cropland_footprint ~ income_group, data = income_crop_fp_df, na.action = na.exclude)
summary(income_crop_fp)

ScheffeTest(income_crop_fp)
#LI-HI, LM-HI, UM-HI, UM-LI, UM-LM

ggplot(data, aes(x = income_group, y = cropland_capacity))+
  geom_boxplot()

income_crop_cap_df <- select(data, income_group, cropland_capacity)
income_crop_cap <- aov(cropland_capacity ~ income_group, data = income_crop_cap_df, na.action = na.exclude)
summary(income_crop_cap)

ScheffeTest(income_crop_cap)
#LI-HI,UM-LI

#Forest connection with income
ggplot(data, aes(x = income_group, y = forest_footprint))+
  geom_boxplot()

income_forest_fp_df <- select(data, income_group, forest_footprint)
income_forest_fp <- aov(forest_footprint ~ income_group, data = income_forest_fp_df, na.action = na.exclude)
summary(income_forest_fp)

ScheffeTest(income_forest_fp)
#LI-HI, LM-HI, UM-HI

ggplot(data, aes(x = income_group, y = forest_capacity))+
  geom_boxplot()

income_forest_cap_df <- select(data, income_group, forest_capacity)
income_forest_cap <- aov(forest_capacity ~ income_group, data = income_forest_cap_df, na.action = na.exclude)
summary(income_forest_cap)

ScheffeTest(income_forest_cap)
#NA

#Carbon connection with income
ggplot(data, aes(x = income_group, y = carbon_footprint))+
  geom_boxplot()

income_carbon_fp_df <- select(data, income_group, carbon_footprint)
income_carbon_fp <- aov(carbon_footprint ~ income_group, data = income_carbon_fp_df, na.action = na.exclude)
summary(income_carbon_fp)

ScheffeTest(income_carbon_fp)
#LI-HI, LM-HI, UM-HI, UM-LI, UM-LM

#Grazing connection with income
ggplot(data, aes(x = income_group, y = grazing_footprint))+
  geom_boxplot()

income_grazing_fp_df <- select(data, income_group, grazing_footprint)
income_grazing_fp <- aov(grazing_footprint ~ income_group, data = income_grazing_fp_df, na.action = na.exclude)
summary(income_grazing_fp)
#NA

ggplot(data, aes(x = income_group, y = grazing_capacity))+
  geom_boxplot()

income_grazing_cap_df <- select(data, income_group, grazing_capacity)
income_grazing_cap <- aov(grazing_capacity ~ income_group, data = income_grazing_cap_df, na.action = na.exclude)
summary(income_grazing_cap)
#NA

#Built Up Land connection with income
ggplot(data, aes(x = income_group, y = builtup_land_footprint))+
  geom_boxplot()

income_land_fp_df <- select(data, income_group, builtup_land_footprint)
income_land_fp<- aov(builtup_land_footprint ~ income_group, data = income_land_fp_df, na.action = na.exclude)
summary(income_land_fp)

ScheffeTest(income_land_fp)
#LI-HI

ggplot(data, aes(x = income_group, y = builtup_land_capacity))+
  geom_boxplot()

income_land_cap_df <- select(data, income_group, builtup_land_capacity)
income_land_cap <- aov(builtup_land_capacity ~ income_group, data = income_land_cap_df, na.action = na.exclude)
summary(income_land_cap)

ScheffeTest(income_land_cap)
#LI-HI

#Fish connection with income
ggplot(data, aes(x = income_group, y = fish_footprint))+
  geom_boxplot()

income_fish_fp_df <- select(data, income_group, fish_footprint)
income_fish_fp <- aov(fish_footprint ~ income_group, data = income_fish_fp_df, na.action = na.exclude)
summary(income_fish_fp)
#NA

ggplot(data, aes(x = income_group, y = fish_capacity))+
  geom_boxplot()

income_fish_cap_df <- select(data, income_group, fish_capacity)
income_fish_cap <- aov(fish_capacity ~ income_group, data = income_fish_cap_df, na.action = na.exclude)
summary(income_fish_cap)

ScheffeTest(income_fish_cap)
#LI-HI, LM-HI