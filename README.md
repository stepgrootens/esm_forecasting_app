# Exploring and Forecasting Experience Sampling Data

**PIPS Group Project**  
**Authors:** Antonia Gavanescu, Step Grootens, Aysen Aykut

## Overview

Experience Sampling Methodology (ESM) collects repeated reports of people's emotions, thoughts, and experiences in daily life. This project demonstrates two complementary approaches to ESM data:

1. **Multilevel regression:** Examine within-person and between-person associations among psychological variables using the OpenESM `0013_hoebeke` dataset.
2. **Individual-level forecasting:** Use an interactive R Shiny application to explore a participant's ESM time series, forecast future values, and evaluate predictive accuracy.

Multilevel regression examines associations in repeated observations nested within participants; forecasting predicts future observations from earlier measurements. Neither analysis alone establishes causality.

## Project structure

```text
PIPS-groupproject-ESM-data/
├── main.Rmd                    # Runnable project walkthrough
├── app/
│   ├── app.R                   # Shiny application
│   └── R/                      # Preparation, exploration, forecasting, plotting, CV
├── analysis/
│   ├── main_analysis.R         # Statistical analysis script
│   ├── functions.R             # Analysis helper functions
│   └── results/                # Saved analysis outputs
└── data/                       # Example ESM datasets
```

## Getting started

Open `PIPS-groupproject-ESM-data.Rproj` in RStudio and work from the project root. Install the R packages required by the scripts if prompted.

- **Run the walkthrough:** Open `main.Rmd` and click **Knit** to generate an HTML document with the descriptive analyses and multilevel model. The app launch chunk is deliberately not evaluated when knitting.
- **Launch the Shiny app:** Run `shiny::runApp("app")` in the RStudio Console.
- **Run the standalone statistical workflow:** Inspect `analysis/main_analysis.R` before running `source("analysis/main_analysis.R")`; it may write files into `analysis/results/`.

The statistical demonstration uses the OpenESM dataset `0013_hoebeke`. Its loading function is defined in `analysis/functions.R`; successful execution depends on the data and required packages being available.

## Multilevel regression analysis

The analysis examines **negativity** as an outcome, with **perseverance, self-criticism, brooding, and replaying** as predictors. It separates within-person deviations from between-person averages, includes a participant random intercept, and includes assessment day when available. The walkthrough produces descriptive summaries, a missingness table, a negativity histogram, model estimates, and a coefficient plot.

## Shiny forecasting app

Upload a CSV containing a single participant's chronologically ordered ESM assessments. Select a numeric target, optionally select external predictors, and specify the number of assessments per day and the target scale boundaries.

The app supports:

- **Data preparation and exploration:** Missingness summaries, LOCF imputation, descriptive statistics, histograms, and Pearson correlations.
- **Forecasting:** Naïve, ARIMA, and ARIMAX forecasts over a seven-day horizon, with forecast plots and prediction intervals.
- **Evaluation:** Expanding-window cross-validation and mean absolute error (MAE) at one-day and seven-day horizons.

**Assumptions and limitations:** The forecasting workflow assumes equally spaced assessments. LOCF cannot fill missing values at the start of a series. For ARIMAX, future predictor values are replaced by historical means, which is a simplifying assumption. Forecasts are participant-specific and should not be interpreted as causal effects.

## Code organisation

| File | Purpose |
|---|---|
| `app/R/data_preparation.R` | Input preparation and missing-value handling |
| `app/R/data_exploration.R` | Exploratory summaries and visualisations |
| `app/R/forecasting.R` | Naïve, ARIMA, and ARIMAX models |
| `app/R/cross_validation.R` | Expanding-window forecast evaluation |
| `app/R/plotting.R` | Forecast visualisation |
| `analysis/functions.R` | Multilevel analysis helper functions |
| `analysis/main_analysis.R` | Standalone statistical analysis workflow |

## Data source

The statistical analysis uses the `0013_hoebeke` dataset from [OpenESM](https://openesmdata.org/). Consult the original dataset documentation for study details and data-use conditions (Hoebeke, Y., Blanchard, M. A., Contreras, A., & Heeren, A. (2022). An experience sampling measure of the key features of rumination. Clinical Neuropsychiatry, 19(5), 288–297. https://doi.org/10.36131/cnfioritieditore20220504)


