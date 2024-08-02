library(caTools)
library(readr)
library(ggplot2)
library(downloader)
library(caTools)
library(FNN)
library(readxl)

clean_data <- readRDS("clean_data.rds")
clean_data


median(clean_data$hdi, na.rm = TRUE)

high_hdi <- subset(clean_data, hdi >= 0.73)
low_hdi <- subset(clean_data, hdi < 0.73)

# change cutoff

str(clean_data)

## Independent two-sample T-test
## Null hypothesis: The low HDI and the high HDI have no significant difference
## in the number of earths required

t.test(high_hdi$number_of_earths_required, low_hdi$number_of_earths_required, mu = mean(clean_data$number_of_earths_required, na.rm = TRUE))



## Independent two-sample T-test
## Null hypothesis: The low HDI and the high HDI have no significant difference
## in their ecological reserve
t.test(high_hdi$ecological_deficit_or_reserve, low_hdi$ecological_deficit_or_reserve, mu = mean(clean_data$ecological_deficit_or_reserve, na.rm = TRUE))


table(clean_data$income_group)
## Anova test
## Null hypothesis: there is no significant difference in the earths required 
## between the different income groups
earths_aov <- aov(data = clean_data, number_of_earths_required ~ income_group)
hist(earths_aov$residuals)

summary(earths_aov)
TukeyHSD(earths_aov)

## x axis is predicted value, y axis is residuals
## plot earths required vs residuals once prediction done



## F-value = variance between groups / within group -- high value means diff significant

## Anova test
## Null hypothesis: there is no significant difference in the ecological reserve 
## between the different income groups
reserve_aov <- aov(data = clean_data, ecological_deficit_or_reserve ~ income_group)
hist(reserve_aov$residuals)
summary(reserve_aov)
TukeyHSD(reserve_aov)


## do linear regression between gdp per capita, hdi, and earths required
lr_split <- sample.split(clean_data$number_of_earths_required, SplitRatio = 0.8)
lr_train_data <- subset(clean_data, lr_split == TRUE)
lr_test_data <- subset(clean_data, lr_split == FALSE)

lr_model <- lm(number_of_earths_required ~ hdi + per_capita_gdp, data = lr_train_data)

lr_pred <- predict(lr_model, newdata = lr_test_data)


ggplot(data = lr_test_data, aes(x = number_of_earths_required, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red", lwd = 1) +
  labs(title = "Real vs Predicted Values",
       x = "True Progression",
       y = "Predicted Progression")

lr_mse <- mean((lr_test_data$number_of_earths_required - lr_pred)^2, na.rm = TRUE)
lr_r2 <- summary(lr_model)$r.squared

lr_mse
lr_r2

## transformations for variables to make relationship linear

str(clean_data)


knn_data <- clean_data[, c('income_group', 'ecological_deficit_or_reserve', 'number_of_earths_required')]

knn_data <- na.omit(knn_data)

nrow(knn_data)

## knn model that predicts the class through the earths required and ecological reserve

knn_split <- sample.split(knn_data$income_group, SplitRatio = 0.8)
knn_train_data <- subset(knn_data, knn_split == TRUE)
knn_test_data <- subset(knn_data, knn_split == FALSE)


knn_model <- knn(train <- knn_train_data[, c('number_of_earths_required', 'ecological_deficit_or_reserve')],
                 test <- knn_test_data[, c('number_of_earths_required', 'ecological_deficit_or_reserve')],
                 cl = knn_train_data$income_group)


pred <- knn_model

actual <- knn_test_data$income_group

cm <- table(actual, pred)
length(actual)
length(pred)
actual
pred

cm
sum(diag(cm)) / sum(cm)


## anova, f test
## different regions vs sustainability, continent

