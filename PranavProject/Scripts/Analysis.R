data <- readRDS("clean_data.rds")
data


median(clean_data$hdi, na.rm = TRUE)

high_hdi <- subset(clean_data, hdi >= 0.73)
low_hdi <- subset(clean_data, hdi < 0.73)

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

## do linear regression between gdp per capita and earths required


