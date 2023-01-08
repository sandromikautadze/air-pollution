# STATISTICAL ANALYSIS

library(tidyverse)
library(broom) # tidy
library(olsrr)
library(DataExplorer)

# TESTING - Questions 1 and 2 ---- 

# importing cleaned data
PM10 <- read_csv("data/cleaned/Ita-PM10.csv") %>% select(-1)
PM25 <- read_csv("data/cleaned/Ita-PM25.csv") %>% select(-1)
O3 <- read_csv("data/cleaned/Ita-O3.csv") %>% select(-1)
NO2 <- read_csv("data/cleaned/Ita-NO2.csv") %>% select(-1)

# Let mu0 be the EU reference guideline for a specific pollutant 
# H0: mu < mu0 vs H1 : mu >= mu0, T-test, alpha0 = 0.05

## Entire Italy - Question 1 ----
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

## Northern, Central, Southern Italy for each pollutant - Question 2 ----

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


# REGRESSIONS - Questions 3 ----

## Causes vs Air Pollution Levels

# pollutant = b1*x1 + ... + bn*xn + e where the bi are the coefficients and the xi the causes

# importing cleaned data
causes <- read_csv("data/cleaned/Europe-Causes.csv") %>% select(-1)

plot_missing(causes)

# correlation matrix causes (converted to data frame)
cor_causes <- causes %>% 
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>% 
  as_tibble(rownames = "variable") %>% 
  select(variable, pm10_avg, pm25_avg, o3_avg, no2_avg) %>% 
  head(10)

# Linear Regression

### PM10 ----
# remove missing values and select main columns
causes_pm10 <- causes %>% select(-c(Country, no2_avg, o3_avg, pm25_avg)) %>% na.omit()

# pm10 = b1*x1 + ... + b10*x10 + e

# all variables (F-test)
model_ftest <- lm(pm10_avg ~ ., data = causes_pm10)
summary(model_ftest)

# Step-Down
model_stepdown <- ols_step_backward_p(model_ftest,
                                      prem = 0.05,
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

#### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_pm10$model$residuals, main = "", xlab = "PM10 Residuals")
qqnorm(model_causes_pm10$model$residuals, main = "")
qqline(model_causes_pm10$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_pm10$model$residuals) # -3e-16
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_pm10$model, print_plot = TRUE)
ols_test_f(model_causes_pm10$model) # Var is homogeneous

remove(causes_pm10)

### PM2.5 ----

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

#### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_pm25$model$residuals, main = "", xlab = "PM2.5 Residuals")
qqnorm(model_causes_pm25$model$residuals, main = "")
qqline(model_causes_pm25$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_pm25$model$residuals)  # 2.3e-16
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_pm25$model, print_plot = TRUE)
ols_test_f(model_causes_pm25$model) # Var is not homogeneous (outlier?)

remove(causes_pm25)

### O3 ----

causes_o3 <- causes %>% select(-c(Country, no2_avg, pm10_avg, pm25_avg)) %>% na.omit()

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

#### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_o3$model$residuals, main = "", xlab = "O3 Residuals")
qqnorm(model_causes_o3$model$residuals, main = "")
qqline(model_causes_o3$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_o3$model$residuals) #1.7e-16
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_o3$model, print_plot = TRUE)
ols_test_f(model_causes_o3$model) # Var is not homogeneous

remove(causes_o3)

### NO2 ----

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

#### Regression Diagnostic ----

# Normality of residuals
par(mfrow = c(1,2))
hist(model_causes_no2$model$residuals, main = "", xlab = "NO2 Residuals")
qqnorm(model_causes_no2$model$residuals, main = "")
qqline(model_causes_no2$model$residuals, col = "blue", lwd = 0.5)
# Mean Zero check
mean(model_causes_no2$model$residuals)  #-1.8e-16
#Homoscedasticity of residuals
ols_plot_resid_fit(model_causes_no2$model, print_plot = TRUE)
ols_test_f(model_causes_no2$model) # Var is homogenous

remove(causes_no2)
remove(causes, cor_causes)

#creating summary table of the models
models <- list("PM10" = model_causes_pm10$model,
               "PM2.5" = model_causes_pm25$model,
               "O3" = model_causes_o3$model,
               "NO2" = model_causes_no2$model)

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
