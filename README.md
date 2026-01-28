# Scotland A&E Attendances & Wait-Time Forecasting (2007–2024)

## Project Overview
This project investigates the monthly demand and performance of Accident and Emergency (A&E) departments across Scotland. Following the 2007 Scottish Government national standard, 95% of patients should be seen within four hours. This analysis uses **Generalised Additive Models (GAMs)** to decompose seasonal trends, account for COVID-19 disruptions, and provide 6-month forecasts.

## Key Technical Achievements

- **High Explanatory Power**: The final models explained **90.9%** of the deviance in total attendances and **96.1%** of the deviance in wait-time percentages.


- **Post-COVID Recovery Analysis**: Quantified a significant decline in wait-time performance starting in 2017, which sharply accelerated during the 2020 pandemic.


- **Validation**: Implemented basis dimension ($k$) checking and residual diagnostics (Q-Q plots, histograms) to ensure model stability and prevent overfitting.



## Methodology & Tech Stack

The analysis was performed in **R** using the following ecosystem:

- **Modelling**: `mgcv` (Generalised Additive Models).


- **Time Series Analysis**: `fpp3`, `tsibble`.


- **Visualisation**: `ggplot2`, `gratia` (GAM-specific plots), `cowplot`.


- **Data Source**: Open data from **Public Health Scotland (PHS)**.



### Model Structure

The project compared multiple model iterations, with the most successful configurations including:

1. **Cyclic Cubic Splines**: To capture 12-month seasonal periodic patterns.


2. **Autoregressive Terms**: Lagged variables to account for historical impact on future values.


3. **Binary Indicators**: Specifically addressing the anomalous shifts during the COVID-19 period.




## Results & Forecasting

The model predicts a fluctuating trend for 2024, with total attendances remaining stable but wait-time performance projected to stay below pre-pandemic levels.

| **Metric**            | **Adjusted R2** | **Deviance Explained** |
| --------------------- | --------------- | ---------------------- |
| **Total Attendances** | 0.897           | 90.9%                  |
| **Wait-Time % (<4h)** | 0.958           | 96.1%                  |


## Repository Structure

- `/code`: R scripts for data cleaning, EDA, and GAM fitting.
- `/report`: Final PDF report detailing the statistical logic and findings.
- `README.md`: Project summary and quick-start guide.


