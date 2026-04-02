# Investigating the Effect of Public Spending on Education on School Vaccination Rates

### Overview

This repository contains the data cleaning, exploratory data analysis, and statistical modeling workflow for the CHL5222 final project. The analysis investigates the association between public education spending and school-level vaccination rates (both MMR and overall) across 32 US states.

The dataset used in this project originates from a Wall Street Journal collection of state health department records for the 2017-2018 and 2018-2019 school years, merged with 2016 US Census Bureau data on state and local government education finances. The dataset includes more than 46,000 schools, with over 42,000 reporting at least one vaccination measure, and contains information on overall vaccination coverage, measles, mumps, and rubella (MMR) vaccination coverage, school characteristics, and state-level measures such as public spending on elementary and secondary education and the estimated school-aged population.

### Research Question

The goal of this repository is to investigate the following research question:

> Does how much each state spend on public education affect their state's school vaccination rates?

### Repository Structure

-   `data/original`: Contains the row dataset (`vaccine.csv`).

-   `data/analytical`: Destination folder for processed data (`vaccine_clean_final.csv` and `vaccine_clean_imputed.csv`).

-   `R/01_data_cleaning.R` (and `Markdown/01_data_cleaning.Rmd` equivalent): Script for data cleaning, missing value analysis, and exploratory data analysis.

-   `R/02_analysis_vaccine_models.R` (and `Markdown/02_analysis_vaccine_models.Rmd` equivalent): Script for feature engineering, primary analysis, secondary analysis, sensitivity analysis, missing data analysis (representativeness check), and model validation

### Prerequisites

To ensure fully reproducible results, please ensure the following R packages are installed before running the scripts:

-   **Data Manipulation & Visualization:** `tidyverse`, `forcats`, `ggplot2`, `ggrepel`

-   **Missing Data Handling:** `naniar`, `mice`

-   **Statistical Modeling:** `lme4`, `lmerTest`, `broom.mixed`, `performance`

-   **Reporting & Formatting:** `knitr`, `kableExtra`, `here`

``` r
{r}
install.packages(c(
  "tidyverse", "forcats", "ggplot2", "ggrepel", # Data Manipulation & Visualization
  "naniar", "mice",                             # Missing Data Handling
  "lme4", "lmerTest", "broom.mixed", "performance", # Statistical Modeling
  "knitr", "kableExtra", "here"                 # Reporting & Formatting
))
```

### Execution Instruction

To reproduce the analysis, run the scripts in the following sequential order:

#### Step 1: Data Preparation

Run `R/01_data_cleaning.R` or `Markdown/01_data_cleaning.Rmd`.

-   **Input:** `data/original/vaccine.csv`

-   **Action:** Cleans missing value indicators (`"null"`, `-1`, blank strings), removes exact duplicates, resolves duplicate school-year-location groups, and performs multiple imputation for missing variables.

-   **Output:** Generates the cleaned datasets (`vaccine_clean_final.csv` and `vaccine_clean_imputed.csv`) in the `data/analytical/` directory, alongside Table 1 (Descriptive Statistics) and the exploratory figures included in the final report.

#### Step 2: Modeling and Analysis

Run `R/02_analysis_vaccine_models.R` or `Markdown/02_analysis_vaccine_models.Rmd`.

-   **Input:** `data/analytical/vaccine_clean_imputed.csv`

-   **Action:** Performs feature engineering to calculate standardized education spending per student (`spending_scaled`). Applies a log-reflected transformation to correct left-skewness in vaccination outcomes. Fits random-intercept linear mixed models (accounting for school type and clustering by state), conducts missing data bias checks, and performs model validation.

-   **Output:** Generates the final formatted output, including Table 2 (Random Intercepts Linear Mixed Model Estimates).

### Data Dictionary

The original dataset contains the following variables:

| Variable | Description | Notes |
|:---|:---|:---|
| state | School's state |  |
| year | School year for the vaccination rates | 2017-18 for CO, CT, MN, MT, NJ, NY, ND, PA, SD, UT, WA. 2018-19 for all others. |
| county | School's county |  |
| name | School name |  |
| type | Whether the school is Public, Private, Charter, BOCES, Kindergarten, or Nonpublic |  |
| mmr | School's Measles, Mumps, and Rubella (MMR) vaccination rate | Missing values originally indicated with `-1` |
| overall | School's overall vaccination rate | Missing values originally indicated with `-1` |
| schagepop2016 | State's estimate of school-aged population in 2016 |  |
| statespending2016 | Public spending on elementary and secondary education by state in 2016 (in \$1,000s) |  |
