# STATISTICAL ANALYSIS

library(tidyverse)
library(broom) # tidy
library(olsrr) 

# HP Testing ----
# Is there enough statistical evidence to show that Italy's pollution levels
# exceed the EU guidelines? What about the North, Center and South of Italy?

# importing cleaned data
PM10 <- read_csv("data/cleaned/Ita-PM10.csv") %>% select(-1)
PM25 <- read_csv("data/cleaned/Ita-PM25.csv") %>% select(-1)
O3 <- read_csv("data/cleaned/Ita-O3.csv") %>% select(-1)
NO2 <- read_csv("data/cleaned/Ita-NO2.csv") %>% select(-1)

# Let mu0 be the EU reference guideline for a specific pollutant 
# H0: mu <= mu0 vs H1 : mu > mu0, T-test, alpha0 = 0.05

# Entire Italy 
test <- function(df, mu0){
  tt <- t.test(df$AirPollutionLevel,
         alternative = "greater",
         mu = mu0)
  return(tt)
}

tt_pm10 <- test(df = PM10, mu0 = 50)
tt_pm25 <- test(df = PM25, mu0 = 25)
tt_o3 <- test(df = O3, mu0 = 120)
tt_no2 <- test(df = NO2, mu0 = 40)

pollutant <- c("PM10", "PM2.5", "O3", "NO2")
result <- c("Retain H0", "Retain H0", "Retain H0", "Retain H0")
summary_italy <- cbind(pollutant,
                 rbind(tidy(tt_pm10), tidy(tt_pm25), tidy(tt_o3), tidy(tt_no2)),
                 result)

remove(tt_pm10, tt_pm25, tt_o3, tt_no2)

# Northern, Central, Southern Italy for each pollutant

# PM10
pm10_north <- PM10 %>% filter(Zone == "North")
pm10_center <- PM10 %>% filter(Zone == "Center")
pm10_south <- PM10 %>% filter(Zone == "South")

tt_pm10_north <- test(df = pm10_north, mu0 = 50)
tt_pm10_center <- test(df = pm10_center, mu0 = 50)
tt_pm10_south <- test(df = pm10_south, mu0 = 50)

zones <- c("North", "Center", "South")
result <- c("Retain H0", "Retain H0", "Retain H0")
summary_pm10 <- cbind(zones,
                      rbind(tidy(tt_pm10_north), tidy(tt_pm10_center), tidy(tt_pm10_south)),
                      result)
remove(tt_pm10_north, tt_pm10_center, tt_pm10_south, pm10_north, pm10_center, pm10_south)

# PM2.5
pm25_north <- PM25 %>% filter(Zone == "North")
pm25_center <- PM25 %>% filter(Zone == "Center")
pm25_south <- PM25 %>% filter(Zone == "South")

tt_pm25_north <- test(df = pm25_north, mu0 = 25)
tt_pm25_center <- test(df = pm25_center, mu0 = 25)
tt_pm25_south <- test(df = pm25_south, mu0 = 25)

summary_pm25 <- cbind(zones,
                      rbind(tidy(tt_pm25_north), tidy(tt_pm25_center), tidy(tt_pm25_south)),
                      result)

remove(tt_pm25_north, tt_pm25_center, tt_pm25_south, pm25_north, pm25_center, pm25_south)

# O3
o3_north <- O3 %>% filter(Zone == "North")
o3_center <- O3 %>% filter(Zone == "Center")
o3_south <- O3 %>% filter(Zone == "South")

tt_o3_north <- test(df = o3_north, mu0 = 120)
tt_o3_center <- test(df = o3_center, mu0 = 120)
tt_o3_south <- test(df = o3_south, mu0 = 120)

res_o3 <- c("Reject H0", "Retain H0", "Retain H0")
summary_o3 <- cbind(zones,
                    rbind(tidy(tt_o3_north), tidy(tt_o3_center), tidy(tt_o3_south)),
                    res_o3)

remove(tt_o3_north, tt_o3_center, tt_o3_south, o3_north, o3_center, o3_south)

# NO2
no2_north <- NO2 %>% filter(Zone == "North")
no2_center <- NO2 %>% filter(Zone == "Center")
no2_south <- NO2 %>% filter(Zone == "South")

tt_no2_north <- test(df = no2_north, mu0 = 40)
tt_no2_center <- test(df = no2_center, mu0 = 40)
tt_no2_south <- test(df = no2_south, mu0 = 40)

summary_no2 <- cbind(zones,
                    rbind(tidy(tt_no2_north), tidy(tt_no2_center), tidy(tt_no2_south)),
                    result)

remove(tt_no2_north, tt_no2_center, tt_no2_south, no2_north, no2_center, no2_south,
       pollutant, res_o3, result, zones, test, PM10, PM25, NO2, O3)

# the necessary results are in the files left so far, that is
# summary_italy, summary_no2, summary_o3, summary_pm10, summary_pm25
remove(summary_italy, summary_no2, summary_o3, summary_pm10, summary_pm25)


#------------------------REGRESSIONS

# Causes vs Air Pollution Levels ----

# importing cleaned data
causes <- read_csv("data/cleaned/Europe-Causes.csv") %>% select(-1)

#correlation matrix causes (converted to data frame)
cor_causes <- causes %>% 
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>% 
  as_tibble(rownames = "variable") %>% 
  select(variable, pm10_avg, pm25_avg, o3_avg, no2_avg) %>% 
  head(10)

# Linear Regression

## PM10 ----
# remove missing values and select main columns
causes_pm10 <- causes %>% select(-c(Country, no2_avg, o3_avg, pm25_avg)) %>% na.omit()

# take log of pm10
par(mfrow = c(2,3))
hist(causes_pm10$pm10_avg, xlab = "PM10 / [ug/m3]", main = "")
boxplot(causes_pm10$pm10_avg, main = "avg_pm10 distribution")
qqnorm(causes_pm10$pm10_avg, main = "")
qqline(causes_pm10$pm10_avg, col = "blue", lwd = 0.5)
hist(log(causes_pm10$pm10_avg), xlab = "PM10 / [ug/m3]", main = "")
boxplot(log(causes_pm10$pm10_avg), main = "log(avg_pm10) distribution")
qqnorm(log(causes_pm10$pm10_avg), main = "")
qqline(log(causes_pm10$pm10_avg), col = "blue", lwd = 0.5)

causes_pm10$pm10_avg <- log(causes_pm10$pm10_avg)

# log(pm10) = b1*x1 + ...


# all variables (F-test)
model_ftest <- lm(pm10_avg ~ ., data = causes_pm10)
summary(model_ftest)

# Step-Down
model_stepdown <- ols_step_backward_aic(model_ftest,
                                      #prem = 0.05,
                                      progress = TRUE,
                                      details = TRUE)
par(mfrow = c(1, 3))
plot(model_stepdown$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepdown$rsquare, xlab = "Step", ylab = "R^2", main = "Step-down procedure PM10", type = "b", col = "blue")
plot(model_stepdown$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepdown$model)

# Step-Up
model_stepup <- ols_step_forward_p(model_ftest,
                                   penter = 0.05,
                                   progress = TRUE,
                                   details = TRUE)

par(mfrow = c(1, 3))
plot(model_stepup$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepup$rsquare, xlab = "Step", ylab = "R^2", main = "Step-up procedure PM10", type = "b", col = "blue")
plot(model_stepup$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepup$model)

# AIC and BIC
AIC(model_ftest, model_stepdown$model, model_stepup$model)
BIC(model_ftest, model_stepdown$model, model_stepup$model)

# Step-down and step-up both give the same optimal model
model_causes_pm10 <- model_stepdown
remove(model_ftest, model_stepdown, model_stepup)

### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_pm10$model$residuals, main = "", xlab = "PM10 Residuals")
qqnorm(model_causes_pm10$model$residuals, main = "")
qqline(model_causes_pm10$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_pm10$model$residuals) 
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_pm10$model, print_plot = TRUE)
ols_test_f(model_causes_pm10$model)

remove(causes_pm10, model_causes_pm10)

## PM2.5 ----

causes_pm25 <- causes %>% select(-c(Country, no2_avg, o3_avg, pm10_avg)) %>% na.omit()

# all variables (F-test)
model_ftest <- lm(pm25_avg ~ ., data = causes_pm25)
summary(model_ftest)

# Step-Down
model_stepdown <- ols_step_backward_p(model_ftest,
                                      prem = 0.05,
                                      progress = TRUE,
                                      details = TRUE)
par(mfrow = c(1, 3))
plot(model_stepdown$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepdown$rsquare, xlab = "Step", ylab = "R^2", main = "Step-down procedure PM2.5", type = "b", col = "blue")
plot(model_stepdown$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepdown$model)

# Step-Up
model_stepup <- ols_step_forward_p(model_ftest,
                                   penter = 0.05,
                                   progress = TRUE,
                                   details = TRUE)

par(mfrow = c(1, 3))
plot(model_stepup$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepup$rsquare, xlab = "Step", ylab = "R^2", main = "Step-up procedure PM2.5", type = "b", col = "blue")
plot(model_stepup$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepup$model)

# AIC and BIC
AIC(model_ftest, model_stepdown$model, model_stepup$model)
BIC(model_ftest, model_stepdown$model, model_stepup$model)

# Step-down and step-up both give the same optimal model
model_causes_pm25 <- model_stepdown
remove(model_ftest, model_stepdown, model_stepup)

### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_pm25$model$residuals, main = "", xlab = "PM2.5 Residuals")
qqnorm(model_causes_pm25$model$residuals, main = "")
qqline(model_causes_pm25$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_pm25$model$residuals) 
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_pm25$model, print_plot = TRUE)
ols_test_f(model_causes_pm25$model)

remove(causes_pm25, model_causes_pm25)

## O3 ----

causes_o3 <- causes %>% select(-c(Country, no2_avg, pm10_avg, pm25_avg)) %>% na.omit()

# take log of o3
par(mfrow = c(2,3))
hist(causes_o3$o3_avg, xlab = "O3 / [ug/m3]", main = "")
boxplot(causes_o3$o3_avg, main = "avg_o3 distribution")
qqnorm(causes_o3$o3_avg, main = "")
qqline(causes_o3$o3_avg, col = "blue", lwd = 0.5)
hist(log(causes_o3$o3_avg), xlab = "O3 / [ug/m3]", main = "")
boxplot(log(causes_o3$o3_avg), main = "log(avg_o3) distribution")
qqnorm(log(causes_o3$o3_avg), main = "")
qqline(log(causes_o3$o3_avg), col = "blue", lwd = 0.5)

causes_o3$o3_avg <- log(causes_o3$o3_avg)

# all variables (F-test)
model_ftest <- lm(o3_avg ~ ., data = causes_o3)
summary(model_ftest)

# Step-Down
model_stepdown <- ols_step_backward_p(model_ftest,
                                      prem = 0.05,
                                      progress = TRUE,
                                      details = TRUE)
par(mfrow = c(1, 3))
plot(model_stepdown$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepdown$rsquare, xlab = "Step", ylab = "R^2", main = "Step-down procedure O3", type = "b", col = "blue")
plot(model_stepdown$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepdown$model)

# Step-Up
model_stepup <- ols_step_forward_p(model_ftest,
                                   penter = 0.05,
                                   progress = TRUE,
                                   details = TRUE)

par(mfrow = c(1, 3))
plot(model_stepup$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepup$rsquare, xlab = "Step", ylab = "R^2", main = "Step-up procedure O3", type = "b", col = "blue")
plot(model_stepup$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepup$model)

# AIC and BIC
AIC(model_ftest, model_stepdown$model, model_stepup$model)
BIC(model_ftest, model_stepdown$model, model_stepup$model)

# Step-down and step-up both give the same optimal model
model_causes_o3 <- model_stepdown
remove(model_ftest, model_stepdown, model_stepup)

### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_o3$model$residuals, main = "", xlab = "O3 Residuals")
qqnorm(model_causes_o3$model$residuals, main = "")
qqline(model_causes_o3$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_o3$model$residuals) 
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_o3$model, print_plot = TRUE)
ols_test_f(model_causes_o3$model)

remove(causes_o3, model_causes_o3)

## NO2 ----

causes_no2 <- causes %>% select(-c(Country, pm25_avg, o3_avg, pm10_avg)) %>% na.omit()

# all variables (F-test)
model_ftest <- lm(no2_avg ~ ., data = causes_no2)
summary(model_ftest)

# Step-Down
model_stepdown <- ols_step_backward_p(model_ftest,
                                      prem = 0.05,
                                      progress = TRUE,
                                      details = TRUE)
par(mfrow = c(1, 3))
plot(model_stepdown$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepdown$rsquare, xlab = "Step", ylab = "R^2", main = "Step-down procedure NO2", type = "b", col = "blue")
plot(model_stepdown$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepdown$model)

# Step-Up
model_stepup <- ols_step_forward_p(model_ftest,
                                   penter = 0.05,
                                   progress = TRUE,
                                   details = TRUE)

par(mfrow = c(1, 3))
plot(model_stepup$aic, xlab = "Step", ylab = "AIC", type = "b", col = "blue")
plot(model_stepup$rsquare, xlab = "Step", ylab = "R^2", main = "Step-up procedure NO2", type = "b", col = "blue")
plot(model_stepup$adjr, xlab = "Step", ylab = "Adjusted R^2", type = "b", col = "blue")

summary(model_stepup$model)

# AIC and BIC
AIC(model_ftest, model_stepdown$model, model_stepup$model)
BIC(model_ftest, model_stepdown$model, model_stepup$model)

# Step-down and step-up both give the same optimal model
model_causes_no2 <- model_stepdown
remove(model_ftest, model_stepdown, model_stepup)

### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_no2$model$residuals, main = "", xlab = "NO2 Residuals")
qqnorm(model_causes_no2$model$residuals, main = "")
qqline(model_causes_no2$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_no2$model$residuals) 
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_no2$model, print_plot = TRUE)
ols_test_f(model_causes_no2$model)

remove(causes_no2, model_causes_no2)
remove(causes, cor_causes)

# Air Pollution Levels vs Effects ----
effects <- read_csv("data/cleaned/Europe-Effects.csv") %>% select(-1)

# adding column with total average pollution in the air
effects$total_avg <- effects$pm10_avg + effects$pm25_avg + effects$no2_avg + effects$o3_avg
#quick inspection of its normality
qqnorm(effects$total_avg)
qqline(effects$total_avg)

# taking log transformations just like in the "causes" part
effects$pm10_avg <- log(effects$pm10_avg) 
effects$o3_avg <- log(effects$o3_avg)

#correlation matrix effects (converted to data frame)
cor_effects <- effects %>% 
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>% 
  as_tibble(rownames = "variable") %>% 
  select(variable, pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg) %>% 
  head(7)

## Model----
# Y = b1*log(pm10) + b2*pm2.5 + b3*no2 + b4*log(o3) + b5(pm10 + pm2.5 + no2 + o3) + e

set1 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Total deaths per 1000 people`) %>% na.omit()
model1 <- lm(`Total deaths per 1000 people` ~ ., data = set1)
summary(model1)
set2 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Birth mortality per 1000 live births`) %>% na.omit()
model2 <- lm(`Birth mortality per 1000 live births` ~ ., data = set2)
summary(model2)
set3 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Life expectancy in years`) %>% na.omit()
model3 <- lm(`Life expectancy in years` ~ ., data = set3)
summary(model3)
set4 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (per 1000 people)`) %>% na.omit()
model4 <- lm(`Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (per 1000 people)` ~ ., data = set4)
summary(model4)
set5 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (per 1000 people)`) %>% na.omit()
model5 <- lm(`Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (per 1000 people)` ~ ., data = set5)
summary(model5)
set6 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (per 1000 people)`) %>% na.omit()
model6 <- lm(`Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (per 1000 people)` ~ ., data = set6)
summary(model6)
set7 <- effects %>% select(pm10_avg, pm25_avg, o3_avg, no2_avg, total_avg, `Deaths - Tracheal, bronchus, and lung cancer - Sex: Both - Age: All Ages (per 1000 people)`) %>% na.omit()
model7 <- lm(`Deaths - Tracheal, bronchus, and lung cancer - Sex: Both - Age: All Ages (per 1000 people)` ~ ., data = set7)
summary(model7)

remove(set1, set2, set3, set4, set5, set6, set7)

#creating summary table of the models
models <- list(model1 = model1,
               model2 = model2,
               model3 = model3,
               model4 = model4,
               model5 = model5,
               model6 = model6,
               model7 = model7)

summary <- purrr::map_df(models, broom::tidy, .id = "model") %>% 
  mutate("Keep Covariate" = case_when(
    p.value <= 0.05 ~ TRUE,
    p.value > 0.05 ~ FALSE
  )) %>% 
  select(-c(std.error, statistic)) %>% 
  rename("Y" = model,
         "Covariate" = term,
         "Coefficient" = estimate,
         "p-value" = p.value)

# model1 is perfect
# model2 passes the F-test (so this model is better than nothing), but no covariates are useful at level 5%
# model3 is perfect
# model4 is perfect
# model5 has only the no2_avg variable, but doesn't pass the f-test
# model6 doesn't pass the F-test and has no relevant covariates
# model7 is perfect

# Diagnostic

# Model1
# Normality of residuals
par(mfrow = c(1,2))
hist(model1$residuals, main = "", xlab = "Model1 Residuals")
qqnorm(model1$residuals, main = "")
qqline(model1$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model1$residuals) 
#Homoscedasticity of residuals
plot(model1$fitted.values, model1$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model2
# Normality of residuals
par(mfrow = c(1,2))
hist(model2$residuals, main = "", xlab = "Model2 Residuals")
qqnorm(model2$residuals, main = "")
qqline(model2$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model2$residuals) 
#Homoscedasticity of residuals
plot(model2$fitted.values, model4$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model3
# Normality of residuals
par(mfrow = c(1,2))
hist(model3$residuals, main = "", xlab = "Model3 Residuals")
qqnorm(model3$residuals, main = "")
qqline(model3$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model3$residuals) 
#Homoscedasticity of residuals
plot(model3$fitted.values, model3$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model4
# Normality of residuals
par(mfrow = c(1,2))
hist(model4$residuals, main = "", xlab = "Model4 Residuals")
qqnorm(model4$residuals, main = "")
qqline(model4$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model4$residuals) 
#Homoscedasticity of residuals
plot(model4$fitted.values, model4$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model5
# Normality of residuals
par(mfrow = c(1,2))
hist(model5$residuals, main = "", xlab = "Model5 Residuals")
qqnorm(model5$residuals, main = "")
qqline(model5$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model5$residuals) 
#Homoscedasticity of residuals
plot(model5$fitted.values, model5$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model6
# Normality of residuals
par(mfrow = c(1,2))
hist(model6$residuals, main = "", xlab = "Model6 Residuals")
qqnorm(model6$residuals, main = "")
qqline(model6$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model6$residuals) 
#Homoscedasticity of residuals
plot(model6$fitted.values, model6$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Model7
# Normality of residuals
par(mfrow = c(1,2))
hist(model7$residuals, main = "", xlab = "Model7 Residuals")
qqnorm(model7$residuals, main = "")
qqline(model7$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model7$residuals) 
#Homoscedasticity of residuals
plot(model7$fitted.values, model7$res, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
