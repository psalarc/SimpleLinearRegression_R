# Car Price Prediction — Linear Regression in R

Iterative OLS regression analysis in R that models used car prices across 11 progressively complex model specifications — from simple univariate regression through polynomial and interaction-term models. Includes VIF multicollinearity diagnostics, ANOVA model comparison, and full residual analysis.

**Stack:** R · dplyr · tidyverse · corrplot · plotly · car (VIF) · RMarkdown

---

## Overview

This project identifies key drivers of used car prices and builds an accurate predictive model using ordinary least squares (OLS) regression. The analysis begins with single-predictor baselines and systematically adds complexity — testing multiple predictors, interaction terms, and polynomial features — until arriving at a well-specified, diagnostically validated final model.

---

## Dataset

**File:** `data/NewCarPricePrediction.csv`

| Feature | Description |
|---------|-------------|
| Price | Target variable — used car sale price |
| Mileage | Odometer reading |
| Prod..year | Year of manufacture |
| Manufacturer | Car brand |
| Category | Vehicle category (sedan, SUV, etc.) |
| Engine.volume | Engine displacement |
| Cylinders | Number of engine cylinders |
| Gear.box.type | Transmission type |
| Wheel | Steering wheel position |
| Leather.interior | Whether the car has leather interior |

---

## Methodology

### Model Progression (11 Specifications)

**1. Exploratory Data Analysis**
- Missing value detection and removal (`drop_na`)
- - Boxplots for Mileage and Price to inspect distributions and outliers
  - - Scatter plots of key predictors against Price
    - - Correlation matrix visualised with `corrplot`
     
      - **2. Univariate Regression Models**
      - - `lm.fit1`: Price ~ Mileage
        - - `lm.fit2`: Price ~ Cylinders
          - - `lm.fit3`: Price ~ Engine.volume
           
            - **3. Multiple Regression Models**
            - - `lm.fit4` through `lm.fit6`: Progressive addition of predictors (year, manufacturer, category, transmission, wheel, leather interior), refined by p-values and adjusted R²
             
              - **4. Diagnostics**
              - - VIF via `car::vif()` to detect multicollinearity in `lm.fit6`
               
                - **5. Interaction & Polynomial Models**
                - - `lm.fit7`: `Prod..year × Manufacturer` interaction
                  - - `lm.fit8`: `Prod..year × Mileage` interaction
                    - - `lm.fit9`: Combined interaction terms
                      - - `lm.fit10`: Quadratic term — `I(Prod..year^2)`
                        - - `lm.fit11`: 5th-degree polynomial for production year + interaction terms
                         
                          - **6. Model Comparison**
                          - - ANOVA F-test comparing `lm.fit6`, `lm.fit9`, and `lm.fit11` to assess whether added complexity is statistically justified
                            - - Residual diagnostic plots for the final model
                             
                              - ---

                              ## Key Findings

                              - Mileage, production year, manufacturer, and gear box type are among the strongest predictors of used car price
                              - - A 5th-degree polynomial on production year with interaction terms (`lm.fit11`) produced the best fit, confirmed by ANOVA comparison
                                - - VIF diagnostics on the base multiple regression model showed no severe multicollinearity
                                  - - Residual analysis confirmed approximate normality and homoscedasticity of errors in the final model
                                   
                                    - ---

                                    ## Repository Structure

                                    ```
                                    SimpleLinearRegression_R/
                                    ├── data/
                                    │   └── NewCarPricePrediction.csv    # Raw dataset
                                    ├── scripts/
                                    │   └── Project1.R                   # Full R analysis script
                                    ├── reports/
                                    │   ├── Project1.html                # Rendered HTML report with output
                                    │   └── LinearRegressionReport.pdf   # Final project report
                                    └── README.md
                                    ```

                                    ---

                                    ## Technologies

                                    | Tool | Purpose |
                                    |------|---------|
                                    | R (base stats) | OLS regression, ANOVA, residual diagnostics |
                                    | car package | VIF multicollinearity diagnostics |
                                    | dplyr / tidyverse | Data wrangling and preprocessing |
                                    | corrplot | Correlation matrix visualization |
                                    | plotly / reactable | Interactive visualizations |
                                    | RMarkdown | Reproducible analysis report |

                                    ---

                                    ## Setup

                                    ```r
                                    # Install dependencies:
                                    install.packages(c("dplyr", "tidyverse", "corrplot", "plotly", "car", "reactable", "htmlwidgets"))

                                    # Run analysis:
                                    source("scripts/Project1.R")
                                    ```
