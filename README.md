# Scotland A&E Attendances & Wait-Time Forecasting (2007–2024)

## Overview
This project investigates the monthly demand and performance of Accident and Emergency (A&E) departments across Scotland. Following the 2007 Scottish Government national standard, 95% of patients should be seen within four hours. This analysis uses **Generalised Additive Models (GAMs)** to decompose seasonal trends, account for COVID-19 disruptions, and provide 6-month forecasts.

## Key Achievements

- **High Explanatory Power**: The final models explained **90.9%** of the deviance in total attendances and **96.1%** of the deviance in wait-time percentages.


- **Post-COVID Recovery Analysis**: Quantified a significant decline in wait-time performance starting in 2017, which sharply accelerated during the 2020 pandemic.


- **Validation**: Implemented basis dimension ($k$) checking and residual diagnostics (Q-Q plots, histograms) to ensure model stability and prevent overfitting.



## Methodology & Tech Stack

The analysis was performed in **R** using the following ecosystem:

- **Modelling**: `mgcv` (Generalised Additive Models).


- **Time Series Analysis**: `fpp3`, `tsibble`.


- **Visualisation**: `ggplot2`, `gratia` (GAM-specific plots), `cowplot`.


- **Data Source**: Open data from [**Public Health Scotland (PHS)**](https://www.opendata.nhs.scot/dataset/monthly-accident-and-emergency-activity-and-waiting-times/resource/37ba17b1-c323-492c-87d5-e986aae9ab59?inner_span=True)



### Model Structure

The project compared multiple model iterations, with the most successful configurations including:

1. **Cyclic Cubic Splines**: To capture 12-month seasonal periodic patterns.


2. **Autoregressive Terms**: Lagged variables to account for historical impact on future values.


3. **Binary Indicators**: Specifically addressing the anomalous shifts during the COVID-19 period.




## Results & Forecasting

The model predicts a fluctuating trend for 2024, with total attendances remaining stable but wait-time performance projected to stay below pre-pandemic levels.

| **Metric**            | **Adjusted $R^2$** | **Deviance Explained** |
| --------------------- | --------------- | ---------------------- |
| **Total Attendances** | 0.897           | 90.9%                  |
| **Wait-Time % (<4h)** | 0.958           | 96.1%                  |


## Repo Structure

- `/code-report`: R scripts for data cleaning, EDA, and GAM fitting; final report detailing the statistical logic and findings.
- `/data`: data used in this project.
- `README.md`: Project summary and quick-start guide.

## Quick Start

```{r}
# Prepare data for the third model of Attendance

agg_mae_2007_Attend3 <- mae_2007_Attend |> 
  # 1. Aggregate total attendances 
  summarise(TAttendEp = sum(AttendEp)) |>

  # 2. Define COVID-19 period and Quarterly factors
  mutate(
    CovidPeriod = factor(ifelse(Month >= covid_start & Month <= covid_end, 1, 0), 
                         levels = c(0, 1)),
    Quarter = factor(quarter(Month), levels = c(1, 2, 3, 4))
  ) |> 
  
  # 3. Generate autoregressive lag terms
  mutate(
    Lag1 = lag(TAttendEp, 1),
    Lag2 = lag(TAttendEp, 2),
    Lag3 = lag(TAttendEp, 3)
  ) |> 
  
  # 4. Create time and seasonal predictors
  mutate(
    Time = as.numeric(Month) - min(as.numeric(Month)),
    nMonth = month(Month)
  )

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
```
