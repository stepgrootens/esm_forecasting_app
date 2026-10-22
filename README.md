# esm_forecasting_app
Forecasting individual experience sampling (ESM) data in R. Includes a Shiny app to explore data and compare Naive, ARIMA and ARIMAX forecasts, plus a mixed-effects analysis of within-person associations between ESM variables.

## Repository Structure

```text
pips-group-project/
├── README.md
├── main.Rmd
├── app/
│   ├── app.R
│   └── R/
│       ├── data_preparation.R
│       ├── data_exploration.R
│       ├── forecasting.R
│       ├── plotting.R
│       └── cross_validation.R
└── analysis/
    ├── main_analysis.R
    ├── functions.R
    └── results/
        ├── *.csv
        ├── *.txt
        ├── *.png
        └── *.rds
```

