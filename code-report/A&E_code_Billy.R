################################################################################
# R codes: Scotland A&E Attendances & Wait-Time Forecasting (2007–2024)
# By: Yanfu Ding
################################################################################
# Load necessary libraries
library(tidyverse)
library(zoo)
library(tsibble)
library(dplyr)
library(fpp3)
library(feasts)
library(slider)
library(mgcv)
library(ggplot2)
library(lubridate)
library(gratia)
library(knitr)



### Data preparation
# Read the relevant dataset
mae_2007 <- read.csv("~/Desktop/A_PHS/Data/monthly_ae_activity_202404.csv")

# Remove quality flag (QF) columns and rows with NA values
qf_mae_2007 <- grep("QF", names(mae_2007), value = TRUE)
mae_2007 <- mae_2007[,!(names(mae_2007) %in% qf_mae_2007)] |> na.omit()

# Remove the 'Country' column as it is the same for all rows
mae_2007 <- mae_2007[,-which(colnames(mae_2007) == "Country")]

# Delete the column 'NumberOfAttendancesAll' and its corresponding columns
mae_2007 <- mae_2007[,-c(5:8)]

# Rename the columns for clarity
colnames(mae_2007) <- c("Month", "HBT", "TreatLoc", "DeptType", 
                        "AttendEp", "Within4Ep", "Over4Ep", "PctWithin4Ep", 
                        "Over8Ep", "PctOver8Ep", "Over12Ep", "PctOver12Ep")

# Abbreviate "DepartmentType" for simplicity
mae_2007$DeptType <- ifelse(mae_2007$DeptType == "Emergency Department", "ED", 
                            "MIUo")

# Simplify 'HBT' to take the last two digits
mae_2007$HBT <- substr(mae_2007$HBT, nchar(mae_2007$HBT) - 1, 
                       nchar(mae_2007$HBT))

# Convert 'HBT', 'TreatLoc', and 'DeptType' columns to factors
mae_2007$HBT <- as.factor(mae_2007$HBT)
mae_2007$TreatLoc <- as.factor(mae_2007$TreatLoc)
mae_2007$DeptType <- as.factor(mae_2007$DeptType)

# Extract columns of interest for analysis
mae_2007_Attend <- mae_2007[,1:5] # For total attendance
mae_2007_p <- mae_2007[,1:6] # For percentage of long waits

# Convert the 'Month' column to a "yearmon" format
mae_2007_Attend <- mae_2007_Attend |> 
  mutate(Month = as.character(Month),         
         Month = as.yearmon(Month, "%Y%m"),
         Month = yearmonth(as.Date(Month)))

mae_2007_p <- mae_2007_p |> 
  mutate(Month = as.character(Month),         
         Month = as.yearmon(Month, "%Y%m"),
         Month = yearmonth(as.Date(Month)))   

# Identify and aggregate duplicates to ensure unique key-index pairs
mae_2007_Attend <- mae_2007_Attend |>
  group_by(Month, HBT, TreatLoc, DeptType) |>
  summarise(AttendEp = sum(AttendEp), .groups = 'drop')

mae_2007_p <- mae_2007_p |>
  group_by(Month, HBT, TreatLoc, DeptType) |>
  summarise(AttendEp = sum(AttendEp), Within4Ep = sum(Within4Ep), 
            .groups = 'drop')

# Convert to "tsibble" for time series analysis
mae_2007_Attend <- mae_2007_Attend |>
  as_tsibble(key = c(HBT, TreatLoc, DeptType), index = Month)

mae_2007_p <- mae_2007_p |>
  as_tsibble(key = c(HBT, TreatLoc, DeptType), index = Month)



### Section 1
# Aggregate total monthly attendances and calculate percentage within 4 hours
agg_mae_2007_p <- mae_2007_p |>
  summarise(TAttendEp = sum(AttendEp), TWithin4Ep = sum(Within4Ep), 
            .groups = 'drop') |>
  mutate(Percentage = TWithin4Ep/TAttendEp)

# Percentage of A&E short waits
autoplot(agg_mae_2007_p, Percentage) +
  labs(y = "Percentage < 4h", title = "Percentage under 4 hours")

### Section 2
# Aggregate total monthly attendances
agg_mae_2007_Attend <- mae_2007_Attend |>
  summarise(TAttendEp = sum(AttendEp))

# Monthly A&E Attendances in Scotland
autoplot(agg_mae_2007_Attend, TAttendEp) +
  labs(y = "Total Attendances", title = "Monthly Total Attendances")

# Decompose the time series into trend, seasonal, and residual components
decomp <- agg_mae_2007_Attend |>
  model(stl = STL(TAttendEp ~ trend(window = 7) + season(window = "periodic")
                  )) |>
  components()
autoplot(decomp) + labs(title = "Seasonal Trend Decomposition of Total 
                        Attendances")

# Plot ACF and PACF for the time series(Monthly A&E Attendances)
agg_mae_2007_Attend |> feasts::ACF(TAttendEp) |> autoplot() 
agg_mae_2007_Attend |> feasts::PACF(TAttendEp) |> autoplot()

# Calculate and plot the 12-month moving average of total attendances
agg_mae_2007_Attend <- agg_mae_2007_Attend |>
  mutate(TAttendEp_MA = slide_dbl(TAttendEp, mean, .before = 11, 
                                  .complete = TRUE))
ggplot(agg_mae_2007_Attend, aes(x = Month)) +
  geom_line(aes(y = TAttendEp), color = "blue") +
  geom_line(aes(y = TAttendEp_MA), color = "red") +
  labs(y = "Total Attendances", title = "12-Month Moving Average of 
       Total Attendances") +
  theme_minimal()

# Identify anomalies using STL decomposition
anoma <- agg_mae_2007_Attend |>
  model(stl = STL(TAttendEp ~ season(window = "periodic"))) |>
  components() |>
  as_tsibble() |>
  mutate(anomaly = if_else(remainder > 2 * sd(remainder) | 
                             remainder < -2 * sd(remainder), remainder, 
                           NA_real_))

# Plot the anomalies
ggplot(anoma, aes(x = Month, y = TAttendEp)) +
  geom_line(color = "blue") +
  geom_point(aes(y = anomaly), color = "red") +
  labs(y = "Total Attendances", title = "Anomaly Detection in 
       Total Attendances") +
  theme_minimal()


### Section 4(1): Monthly attendances
# Add COVID-19 period factor
covid_start <- yearmonth("2020 Mar")
covid_end <- yearmonth("2022 Feb")
agg_mae_2007_Attend <- agg_mae_2007_Attend |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start
                                     & Month <= covid_end, 1, 0), 
                              levels = c(0, 1)))

# Add lagged variable to capture autocorrelation
agg_mae_2007_Attend <- agg_mae_2007_Attend |>
  mutate(Lag1 = lag(TAttendEp, 1))

# Rescale Month variable to avoid large numeric values
agg_mae_2007_Attend <- agg_mae_2007_Attend |>
  mutate(Time = as.numeric(Month) - min(as.numeric(Month))) |> 
  mutate(nMonth = month(Month))

# Split the data into training and testing sets
train_data <- agg_mae_2007_Attend |> filter(Month < yearmonth("2023 Nov"))
test_data <- agg_mae_2007_Attend |> filter(Month >= yearmonth("2023 Nov"))

# Define the GAM model with seasonality, lag, and COVID-19 indicator
gam_model <- gam(TAttendEp ~ s(Time, bs = "cs") + s(nMonth, bs = "cc") + 
                   CovidPeriod + Lag1, data = train_data)

# Predict on the training set
train_data <- train_data |>
  mutate(Fitted = predict(gam_model, newdata = train_data))

# Predict on the test set
test_data <- test_data |>
  mutate(Predicted = predict(gam_model, newdata = test_data))

# Calculate RMSE
rmse <- mean((test_data$TAttendEp - test_data$Predicted)^2) |> sqrt()

# Plot: Monthly Attendances: Fitted vs Predicted vs Actual
ggplot() +
  geom_line(data = train_data, aes(x = Month, y = TAttendEp, color = "Actual"), 
            linewidth = 1) +
  geom_line(data = train_data, aes(x = Month, y = Fitted, color = "Fitted"), 
            linewidth = 1) +
  geom_line(data = test_data, aes(x = Month, y = TAttendEp, color = "Actual"), 
            linewidth = 1) +
  geom_line(data = test_data, aes(x = Month, y = Predicted, 
                                  color = "Predicted"), size = 1) +
  labs(title = "Monthly Attendances: Fitted vs Predicted vs Actual",
       y = "Attendances",
       x = "Month") +
  scale_color_manual(name = "Type", 
                     values = c("Actual" = "black","Fitted" = "blue", 
                                "Predicted" = "green")) +
  theme_minimal()

# Prepare data for the second model
agg_mae_2007_Attend2 <- mae_2007_Attend |> 
  summarise(TAttendEp = sum(AttendEp))

agg_mae_2007_Attend2 <- agg_mae_2007_Attend2 |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start 
                                     & Month <= covid_end, 1, 0), 
                              levels = c(0, 1)))

# Add multiple lagged variables to capture autocorrelation
agg_mae_2007_Attend2 <- agg_mae_2007_Attend2 |>
  mutate(Lag1 = lag(TAttendEp, 1),
         Lag2 = lag(TAttendEp, 2),
         Lag3 = lag(TAttendEp, 3))

# Rescale "Month"
agg_mae_2007_Attend2 <- agg_mae_2007_Attend2 |>
  mutate(Time = as.numeric(Month) - min(as.numeric(Month))) |> 
  mutate(nMonth = month(Month))

# Training and test sets
train_data2 <- agg_mae_2007_Attend2 |> filter(Month < yearmonth("2023 Nov"))
test_data2 <- agg_mae_2007_Attend2 |> filter(Month >= yearmonth("2023 Nov"))

# Define the GAM model with factor smooth interaction, 
# seasonality, lags, and COVID-19 indicator
gam_model2 <- gam(TAttendEp ~ s(Time, bs = "cs") + s(nMonth, bs = "cc") + 
                    s(Time, by = CovidPeriod) + Lag1 + Lag2 + Lag3 + 
                    CovidPeriod, 
                  data = train_data2)

# Predict: training
train_data2 <- train_data2 |>
  mutate(Fitted = predict(gam_model2, newdata = train_data2))

# Predict: testing
test_data2 <- test_data2 |>
  mutate(Predicted = predict(gam_model2, newdata = test_data2))

# Calculate RMSE
rmse2 <- sqrt(mean((test_data2$TAttendEp - test_data2$Predicted)^2))

# Plot the results for the second model
ggplot() +
  geom_line(data = train_data2, aes(x = Month, y = TAttendEp, 
                                    color = "Actual"), size = 1) +
  geom_line(data = train_data2, aes(x = Month, y = Fitted, color = "Fitted"), 
            size = 1) +
  geom_line(data = test_data2, aes(x = Month, y = TAttendEp, color = "Actual"),
            size = 1) +
  geom_line(data = test_data2, aes(x = Month, y = Predicted, 
                                   color = "Predicted"), size = 1) +
  labs(title = "Monthly Attendances: Fitted vs Predicted vs Actual",
       y = "Attendances",
       x = "Month") +
  scale_color_manual(name = "Type", 
                     values = c("Actual" = "black", "Fitted" = "blue", 
                                "Predicted" = "green")) +
  theme_minimal()

# Prepare data for the third model
agg_mae_2007_Attend3 <- mae_2007_Attend |> 
  summarise(TAttendEp = sum(AttendEp))

agg_mae_2007_Attend3 <- agg_mae_2007_Attend3 |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start & 
                                       Month <= covid_end, 1, 0), 
                              levels = c(0, 1)))

agg_mae_2007_Attend3 <- agg_mae_2007_Attend3 |>
  mutate(Lag1 = lag(TAttendEp, 1),
         Lag2 = lag(TAttendEp, 2),
         Lag3 = lag(TAttendEp, 3))

agg_mae_2007_Attend3 <- agg_mae_2007_Attend3 |>
  mutate(Time = as.numeric(Month) - min(as.numeric(Month))) |> 
  mutate(nMonth = month(Month))

# Add quarterly dummy variables
agg_mae_2007_Attend3 <- agg_mae_2007_Attend3 |>
  mutate(Quarter = factor(quarter(Month), levels = c(1, 2, 3, 4))) 

# Training and test sets
train_data3 <- agg_mae_2007_Attend3 |> filter(Month < yearmonth("2023 Nov"))
test_data3 <- agg_mae_2007_Attend3 |> filter(Month >= yearmonth("2023 Nov"))

# Define the GAM model with additional seasonal components
gam_model3 <- gam(TAttendEp ~ s(Time, bs = "cs", k = 20) + 
                    s(nMonth, bs = "cc", k = 12) + 
                    s(Time, by = CovidPeriod, k = 10) + 
                    Lag1 + Lag2 + Lag3 + CovidPeriod +
                    Quarter,
                  data = train_data3)

# Predict: training
train_data3 <- train_data3 |>
  mutate(Fitted = predict(gam_model3, newdata = train_data3))

# Predict: testing 
test_data3 <- test_data3 |>
  mutate(Predicted = predict(gam_model3, newdata = test_data3))

# Calculate RMSE for the third model
rmse3 <- sqrt(mean((test_data3$TAttendEp - test_data3$Predicted)^2))

# Plot the results for the third model
ggplot() +
  geom_line(data = train_data3, aes(x = Month, y = TAttendEp, 
                                    color = "Actual"), size = 1) +
  geom_line(data = train_data3, aes(x = Month, y = Fitted, 
                                    color = "Fitted"), size = 1) +
  geom_line(data = test_data3, aes(x = Month, y = TAttendEp, 
                                   color = "Actual"), size = 1)  +
  geom_line(data = test_data3, aes(x = Month, y = Predicted, 
                                   color = "Predicted"), size = 1) +
  labs(title = "Monthly Attendances: Fitted vs Predicted vs Actual",
       y = "Attendances",
       x = "Month") +
  scale_color_manual(name = "Type", values = c("Actual" = "black", 
                                               "Fitted" = "blue", 
                                               "Predicted" = "green")) +
  theme_minimal()

# Create a table to list RMSE and AIC for the three models
model_metrics <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  RMSE = c(rmse, rmse2, rmse3),  
  AIC = c(AIC(gam_model), AIC(gam_model2), AIC(gam_model3))       
)

knitr::kable(model_metrics, caption = "Model Performance Metrics (Total Attendan
             ces)\\label{table1}", format = "latex")  |>
  kable_styling(font_size = 10) |> 
  kable_styling(latex_options = "HOLD_position") |>
  kable_styling(latex_options = "repeat_header")

# Check the basis dimension (k) for the smooth terms in the model
kable(k.check(gam_model3), caption = "Basis Dimension (k) Checking Results
      \\label{table2}") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  kable_styling(latex_options = "HOLD_position")

# Check the GAM model using simulation-based diagnostic plots
appraise(gam_model3, method = 'simulate')

# Prepare data for final model fitting and forecasting
agg_mae_2007_Attendf <- mae_2007_Attend |> 
  summarise(TAttendEp = sum(AttendEp))

agg_mae_2007_Attendf <- agg_mae_2007_Attendf |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start & 
                                       Month <= covid_end, 1, 0), 
                              levels = c(0, 1)))

agg_mae_2007_Attendf <- agg_mae_2007_Attendf |>
  mutate(Lag1 = lag(TAttendEp, 1),
         Lag2 = lag(TAttendEp, 2),
         Lag3 = lag(TAttendEp, 3))

agg_mae_2007_Attendf <- agg_mae_2007_Attendf |>
  mutate(Time = as.numeric(Month) - min(as.numeric(Month))) |> 
  mutate(nMonth = month(Month))

agg_mae_2007_Attendf <- agg_mae_2007_Attendf |>
  mutate(Quarter = factor(quarter(Month), levels = c(1, 2, 3, 4))) 

# Use the whole dataset for training
train_dataf <- agg_mae_2007_Attendf

# Final GAM model (Attendance)
gam_modelf <- gam(TAttendEp ~ s(Time, bs = "cs", k = 10) + 
                    s(nMonth, bs = "cc", k = 12) + 
                    s(Time, by = CovidPeriod, k = 10) + 
                    Lag1 + Lag2 + Lag3 + CovidPeriod + 
                    Quarter, 
                  data = train_dataf)

# Parametric coefficients of the final model
kable(summary(gam_modelf)$p.table,  
      caption = "Attendance: Summary of Parametric Coefficient
      s\\label{table3}") |>
  kable_styling(latex_options = "HOLD_position") |>
  kable_styling(latex_options = "repeat_header")

# Smooth terms of the final model
kable(summary(gam_modelf)$s.table, 
      caption = "Attendance: Summary of Smooth Terms\\label{table4}") |>
  kable_styling(latex_options = "HOLD_position") |>
  kable_styling(latex_options = "repeat_header")

# Plot the smooth terms from the final model
draw(gam_modelf, scales = "free")

# Predict on the training set
train_dataf <- train_dataf |>
  mutate(Fitted = predict(gam_modelf, newdata = train_dataf))

# Generate future dates for the forecast period
future_dates <- data.frame(Month = yearmonth(seq(as.Date("2024-05-01"), 
                                                 as.Date("2024-10-01"), 
                                                 by = "month")))

# Create the required features for the future dates
future_dates <- future_dates |>
  mutate(Time = as.numeric(Month) - min(as.numeric(Month)),
         nMonth = month(Month),
         CovidPeriod = factor(ifelse(Month >= covid_start & 
                                       Month <= covid_end, 1, 0), 
                              levels = c(0, 1)),
         Quarter = factor(quarter(Month), levels = c(1, 2, 3, 4)))

# Ensure correct temporal ordering and select latest lag values
latest_lags <- train_dataf |>
  arrange(Month) |>
  slice(n()) |>
  dplyr::select(Lag1, Lag2, Lag3)

future_dates <- future_dates |>
  mutate(Lag1 = latest_lags$Lag1, Lag2 = latest_lags$Lag2, 
         Lag3 = latest_lags$Lag3)

# Predictions and prediction intervals
pred <- predict(gam_modelf, newdata = future_dates, type = "link", 
                se.fit = TRUE)

future_dates <- future_dates |>
  mutate(Predicted = pred$fit,
         lower = pred$fit - 1.96 * pred$se.fit,
         upper = pred$fit + 1.96 * pred$se.fit)

# Combine training data with future dates for plotting
combined_data <- bind_rows(
  train_dataf |> mutate(Type = "Fitted"),
  future_dates |> mutate(Type = "Predicted", TAttendEp = NA, Fitted = NA)
)

# Plotting the results with prediction intervals
ggplot() +
  geom_line(data = combined_data |> filter(Type == "Fitted"), 
            aes(x = Month, y = TAttendEp, color = "Actual"), linewidth = 1) +
  geom_line(data = combined_data |> filter(Type == "Fitted"), 
            aes(x = Month, y = Fitted, color = "Fitted"), linewidth = 1, 
            linetype = 2) +
  geom_line(data = combined_data |> filter(Type == "Predicted"), 
            aes(x = Month, y = Predicted, color = "Predicted"), size = 1, 
            linetype = 1) +
  geom_ribbon(data = future_dates, aes(x = Month, ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "red") +
  labs(title = "Monthly Attendances: Actual vs Fitted vs Predicted",
       y = "Attendances",
       x = "Month") +
  scale_color_manual(name = "Type", values = c("Actual" = "blue", 
                                               "Fitted" = "black", 
                                               "Predicted" = "red")) +
  theme_minimal()

kable(combined_data[203:208,c(1,12:14)], 
      caption = "Forecast of Total Attendances: 
      May 2024 to Oct 2024 \\label{table5}", 
      col.names = c("Month","Forecast", "95% Lower Bound", 
                    "95% Upper Bound")) |>
  kable_styling(latex_options = "HOLD_position")



### Section 4(2): Monthly percentages
# Prepare data for percentage analysis
agg_mae_2007_p1 <- agg_mae_2007_p |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start & 
                                       Month <= covid_end, 1, 0), 
                              levels = c(0, 1)),
         Time = as.numeric(Month) - min(as.numeric(Month)),
         Quarter = factor(quarter(Month)))

# Training and test sets
train_data_p1 <- agg_mae_2007_p1 |> filter(Month < yearmonth("2023 Nov"))
test_data_p1 <- agg_mae_2007_p1 |> filter(Month >= yearmonth("2023 Nov"))

# Fit the GAM model for percentages (Model 4)
gam_model_p1 <- gam(Percentage ~ s(Time, bs = "cs", k = 50) + 
                      s(nMonth, bs = "cc", k = 12)+
                      s(Time, by = CovidPeriod, k = 20) + CovidPeriod, 
                    data = train_data_p1, family = betar(link = "logit"))

# Predict on the training set
train_data_p1 <- train_data_p1 |>
  mutate(Fitted = predict(gam_model_p1, newdata = train_data_p1, 
                          type = "response"))

# Predict on the test set
test_data_p1 <- test_data_p1 |>
  mutate(Predicted = predict(gam_model_p1, newdata = test_data_p1, 
                             type = "response"))

# Calculate RMSE for the test set
rmse_p1 <- sqrt(mean((test_data_p1$Percentage - test_data_p1$Predicted)^2))

# Combine training and test sets for plotting
combined_p1 <- bind_rows(train_data_p1 |> mutate(Type = "Fitted"),
                         test_data_p1 |> mutate(Type = "Predicted"))

# Plotting the results for percentages (Model 4)
ggplot() +
  geom_line(data = combined_p1 |> filter(Type == "Fitted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p1 |> filter(Type == "Fitted"), 
            aes(x = Month, y = Fitted, color = "Fitted"), size = 1, 
            linetype = "dashed") +
  geom_line(data = combined_p1 |> filter(Type == "Predicted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p1 |> filter(Type == "Predicted"), 
            aes(x = Month, y = Predicted, color = "Predicted"), size = 1, 
            linetype = 1) +
  labs(title = "Percentage of people waiting less than 4 hours: Actual 
       vs Fitted vs Predicted",
       y = "Percentage",
       x = "Month") +
  scale_color_manual(name = "Type", values = c("Actual" = "black", 
                                               "Fitted" = "blue", 
                                               "Predicted" = "red")) +
  theme_minimal()

# Prepare data for the second percentage model (Model 5)
agg_mae_2007_p2 <- agg_mae_2007_p |>
  mutate(CovidPeriod = factor(ifelse(Month >= covid_start & 
                                       Month <= covid_end, 1, 0), 
                              levels = c(0, 1)),
         Time = as.numeric(Month) - min(as.numeric(Month)),
         Quarter = factor(quarter(Month)))

# Training and test sets
train_data_p2 <- agg_mae_2007_p2 |> filter(Month < yearmonth("2023 Nov"))
test_data_p2 <- agg_mae_2007_p2 |> filter(Month >= yearmonth("2023 Nov"))

# Fit the second GAM model for percentages (Model 5)
gam_model_p2 <- gam(Percentage ~ s(Time, bs = "cs", k = 20) + 
                      s(nMonth, bs = "cc", k = 12),
                    data = train_data_p2, family = betar(link = "logit"))

# Predict on the training set
train_data_p2 <- train_data_p2 |>
  mutate(Fitted = predict(gam_model_p2, newdata = train_data_p2, 
                          type = "response"))

# Predict on the test set
test_data_p2 <- test_data_p2 |>
  mutate(Predicted = predict(gam_model_p2, newdata = test_data_p2, 
                             type = "response"))

# Calculate RMSE for Model 5
rmse2_p <- sqrt(mean((test_data_p2$Percentage - test_data_p2$Predicted)^2))

# Combine training and test sets for plotting
combined_p2 <- bind_rows(train_data_p2 |> mutate(Type = "Fitted"),
                         test_data_p2 |> mutate(Type = "Predicted"))

# Plotting the results for Model 5
ggplot() +
  geom_line(data = combined_p2 |> filter(Type == "Fitted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p2 |> filter(Type == "Fitted"), 
            aes(x = Month, y = Fitted, color = "Fitted"), size = 1, linetype = "dashed") +
  geom_line(data = combined_p2 |> filter(Type == "Predicted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p2 |> filter(Type == "Predicted"), 
            aes(x = Month, y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(title = "Percentage of people waiting less than 4 hours: Actual vs Fitted vs Predicted",
       y = "Percentage",
       x = "Month") +
  scale_color_manual(name = "Type", values = c("Actual" = "black", 
                                               "Fitted" = "blue", 
                                               "Predicted" = "red")) +
  theme_minimal()

# Create a dataframe to list RMSE for the percentage models
model_metricsp <- data.frame(
  Model = c("Model 4", "Model 5"),
  RMSE = c(rmse_p1, rmse2_p)
)

knitr::kable(model_metricsp,
             caption = "Model Performance Metrics (Percentage)\\label{table6}",
             format = "latex")  |>
  kable_styling(font_size = 10) |> 
  kable_styling(latex_options = "HOLD_position") |>
  kable_styling(latex_options = "repeat_header")

# Check the basis dimension (k) for the second percentage model
kable(k.check(gam_model_p2), caption = "Percentages: Basis Dimension (k) 
      Checking Results\\label{table7}") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  kable_styling(latex_options = "HOLD_position")

# Examine Model 5 using simulation-based diagnostic plots
appraise(gam_model_p2, method = "simulate")

# Prepare data for final percentage model fitting and forecasting
train_data_p <- agg_mae_2007_p |>
  mutate(CovidPeriod = factor(ifelse(
    Month >= covid_start & Month <= covid_end, 1, 0), levels = c(0, 1)),
    Time = as.numeric(Month) - min(as.numeric(Month)),
    nMonth = month(Month))

# Fit the final GAM model for percentages
gam_model_p <- gam(Percentage ~ s(Time, bs = "cs", k = 15) + 
                     s(nMonth, bs = "cc", k = 12),
                   data = train_data_p, family = betar(link = "logit"))

# Smooth terms of the final percentage model
kable(summary(gam_model_p)$s.table, caption = "Percentages: Summary of Smooth Terms\\label{table8}")  |>
  kable_styling(latex_options = "HOLD_position") |>
  kable_styling(latex_options = "repeat_header")

# Plot the smooth terms from the final percentage model
draw(gam_model_p, scales = 'free')

# Predict on the training set for the final percentage model
train_data_p <- train_data_p |>
  mutate(Fitted = predict(gam_model_p, newdata = train_data_p, 
                          type = "response"))

# Generate future dates for the forecast period for the final percentage model
future_dates <- data.frame(Month = seq(yearmonth("2024 May"), 
                                       yearmonth("2024 Nov"), by = 1))

# Create lag variables for the future dates for the final percentage model
future_dates <- future_dates |>
  mutate(CovidPeriod = factor(ifelse(
    Month >= covid_start & Month <= covid_end, 1, 0), levels = c(0, 1)),
    Time = as.numeric(Month) - min(as.numeric(train_data_p$Month)),
    nMonth = month(Month))

# Predict with prediction intervals for the final percentage model
pred <- predict(gam_model_p, newdata = future_dates, type = "link", 
                se.fit = TRUE)
future_dates <- future_dates |>
  mutate(Predicted = plogis(pred$fit),
         lower = plogis(pred$fit - 1.96 * pred$se.fit),
         upper = plogis(pred$fit + 1.96 * pred$se.fit))

# Combine training data with future dates for plotting
combined_p <- bind_rows(train_data_p |> mutate(Type = "Fitted"),
                        future_dates |> mutate(Type = "Predicted"))

# Plotting the results with prediction intervals for the final percentage model
ggplot() +
  geom_line(data = combined_p |> filter(Type == "Fitted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p |> filter(Type == "Fitted"), 
            aes(x = Month, y = Fitted, color = "Fitted"), size = 1, 
            linetype = "dashed") +
  geom_line(data = combined_p |> filter(Type == "Predicted"), 
            aes(x = Month, y = Percentage, color = "Actual"), size = 1) +
  geom_line(data = combined_p |> filter(Type == "Predicted"), 
            aes(x = Month, y = Predicted, color = "Predicted"), size = 1, 
            linetype = "dashed") +
  geom_ribbon(data = future_dates, aes(x = Month, ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "red") +
  labs(title = "Percentage of people waiting less than 4 hours: 
       Actual vs Fitted vs Predicted",
       y = "Percentage",
       x = "Month") +
  scale_color_manual(name = "Type", values = c("Actual" = "black", 
                                               "Fitted" = "blue", 
                                               "Predicted" = "red")) +
  theme_minimal()

kable(combined_p[203:208, c(1, 10:12)], caption = "Forecast of Monthly 
      Percentages: May 2024 to Oct 2024 \\label{table9}", 
      col.names = c("Month","Forecast", "95% Lower Bound", "95% Upper Bound")) |>
  kable_styling(latex_options = "HOLD_position")


### Section 5 (Figure shown in Appendix)
# Read the demographic data
mae_demo <- read.csv(
  "~/Desktop/A_PHS/Data/opendata_monthly_ae_demographics_202404.csv")

# Clean the demographic data by removing NA values
mae_demo <- mae_demo |>
  mutate(Age = na_if(Age, ""), Sex = na_if(Sex, "")) |> na.omit()

# Rename columns for clarity
names(mae_demo) <- c("Month", "Country", "HBT", "DeptType", "Age", 
                     "AgeQF", "Sex", "SexQF", "Depriv", "DeprivQF", "Attend")

# Simplify and transform columns in the demographic data
mae_demo$HBT <- substr(mae_demo$HBT, nchar(mae_demo$HBT) - 1, nchar(mae_demo$HBT))
mae_demo$DeptType <- ifelse(mae_demo$DeptType == "Emergency Department", 
                            "ED", "MIUo")
mae_demo$HBT <- as.factor(mae_demo$HBT)
mae_demo$DeptType <- as.factor(mae_demo$DeptType)
mae_demo$Depriv <- as.factor(mae_demo$Depriv)
mae_demo$Age <- as.factor(mae_demo$Age)
mae_demo$Sex <- as.factor(mae_demo$Sex)
mae_demo$Month <- yearmonth(as.yearmon(as.character(mae_demo$Month), format="%Y%m"))

# Plot Total Attendances over Time by Age Group
mae_demo_Age <- mae_demo |>
  group_by(Month, Age) |>
  summarise(TAttend = sum(Attend), .groups = 'drop')

ggplot(mae_demo_Age, aes(x = Month, y = TAttend, color = Age, group = Age)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Attendances Over Time by Age Group",
       x = "Month",
       y = "Total Attendances") +
  theme_minimal() +
  scale_color_discrete(name = "Age Group")

# Plot Total Attendances over Time by Deprivation Group
mae_demo_Depriv <- mae_demo |>
  group_by(Month, Depriv) |>
  summarise(TAttend = sum(Attend), .groups = 'drop')

ggplot(mae_demo_Depriv, aes(x = Month, y = TAttend, color = Depriv, 
                            group = Depriv)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Attendances Over Time by Deprivation Group",
       x = "Month",
       y = "Total Attendances") +
  theme_minimal() +
  scale_color_discrete(name = "Deprivation Group")

# Plot Total Attendances over Time by Department Type
mae_demo_Dept <- mae_demo |>
  group_by(Month, DeptType) |>
  summarise(TAttend = sum(Attend), .groups = 'drop')

ggplot(mae_demo_Dept, aes(x = Month, y = TAttend, color = DeptType, 
                          group = DeptType)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Attendances Over Time by Department Type",
       x = "Month",
       y = "Total Attendances") +
  theme_minimal() +
  scale_color_discrete(name = "Department Type")

# Plot Total Attendances over Time by Health Board of Treatment (HBT)
mae_demo_HBT <- mae_demo |>
  group_by(Month, HBT) |>
  summarise(TAttend = sum(Attend), .groups = 'drop')

ggplot(mae_demo_HBT, aes(x = Month, y = TAttend, color = HBT, group = HBT)) +
  geom_line(linewidth = 1) +
  labs(title = "Total Attendances Over Time by Health Board of Treatment (HBT)",
       x = "Month",
       y = "Total Attendances") +
  theme_minimal() +
  scale_color_discrete(name = "HBT") +
  facet_wrap(~ HBT, scales = "free_y") # Separate panels for each HBT


                                

