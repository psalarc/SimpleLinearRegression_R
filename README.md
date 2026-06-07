# Car Price Prediction — Linear Regression in R

A statistical regression analysis project that models used car prices using a progression of linear models, from simple univariate regression through to polynomial and interaction-term models. Built in R as part of coursework at Rider University (MSD325).

---

## Project Overview

The goal of this project is to identify the key drivers of used car prices and build an accurate predictive model using ordinary least squares (OLS) regression. The analysis begins with single-predictor models and systematically adds complexity — testing multiple predictors, interaction terms, polynomial features, and multicollinearity diagnostics — until arriving at a well-specified final model.

---

## Dataset

**File:** `data/NewCarPricePrediction.csv`

The dataset contains records of used cars with the following features:

| Feature | Description |
|---|---|
| `Price` | Target variable — sale price of the car |
| `Mileage` | Odometer reading |
| `Prod..year` | Year of manufacture |
| `Manufacturer` | Car brand |
| `Category` | Vehicle category (sedan, SUV, etc.) |
| `Engine.volume` | Engine displacement |
| `Cylinders` | Number of engine cylinders |
| `Gear.box.type` | Transmission type |
| `Wheel` | Steering wheel position |
| `Leather.interior` | Whether the car has leather interior |

---

## Methodology

The analysis follows a structured, iterative model-building approach:

**1. Exploratory Data Analysis**
- Missing value detection and removal (`drop_na`)
- Boxplots for `Mileage` and `Price` to inspect distributions and outliers
- Scatter plots of key predictors against `Price`
- Correlation matrix visualised with `corrplot`

**2. Univariate Regression Models**
- `lm.fit1`: Price ~ Mileage
- `lm.fit2`: Price ~ Cylinders
- `lm.fit3`: Price ~ Engine.volume

**3. Multiple Regression Models**
- `lm.fit4` through `lm.fit6`: Progressive addition of predictors (year, manufacturer, category, transmission, wheel, leather interior), refining the feature set based on p-values and adjusted R²

**4. Diagnostics**
- Variance Inflation Factor (VIF) via `car::vif()` to detect multicollinearity in `lm.fit6`

**5. Interaction & Polynomial Models**
- `lm.fit7`: Prod..year × Manufacturer interaction
- `lm.fit8`: Prod..year × Mileage interaction
- `lm.fit9`: Prod..year × Mileage + Engine.volume × Gear.box.type interactions
- `lm.fit10`: Quadratic term for production year — `I(Prod..year^2)`
- `lm.fit11`: 5th-degree polynomial for production year + interaction terms

**6. Model Comparison**
- ANOVA F-test comparing `lm.fit6`, `lm.fit9`, and `lm.fit11` to assess whether added complexity is statistically justified
- Residual diagnostic plots (`par(mfrow=c(2,2)); plot(lm.fit11)`) for the final model

---

## Repository Structure

```
SimpleLinearRegression_R/
├── data/
│   └── NewCarPricePrediction.csv     # Raw dataset
├── scripts/
│   └── Project1.R                    # Full R analysis script
└── reports/
    ├── Project1.html                 # Rendered HTML report with output
    └── MSD325SimpleLinearRegressionProject_PabloSalar.pdf  # Final project report
```

---

## Technologies

- **Language:** R
- **Libraries:** `dplyr`, `tidyverse`, `tidyr`, `corrplot`, `plotly`, `reactable`, `car`, `htmlwidgets`

---

## Key Takeaways

- Mileage, production year, manufacturer, and gear box type are among the strongest predictors of used car price
- A 5th-degree polynomial on production year with interaction terms (`lm.fit11`) produced the best fit, confirmed by ANOVA comparison
- VIF diagnostics on the base multiple regression model showed no severe multicollinearity issues
- Residual analysis confirmed approximate normality and homoscedasticity of errors in the final model
