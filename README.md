# Remote Work and Wage Premium: A Bayesian Causal Analysis

## Overview
This project estimates the causal effect of remote work on hourly wages in the post-pandemic U.S. labor market using a two-stage Bayesian Propensity Score Analysis (BPSA). The analysis is based on microdata from the January 2025 Current Population Survey (CPS) Outgoing Rotation Group (ORG), and forms a final project for the Bayesian Analysis course at the University of Milan.

## Objectives
- Estimate the Average Treatment Effect (ATE) of remote work on wages.
- Incorporate estimation uncertainty from the design stage into outcome modeling.
- Use Bayesian logistic regression (Metropolis–Hastings) and Bayesian linear regression (Gibbs sampling).
- Provide posterior distributions of causal effects with full MCMC diagnostics.

## Repository Contents
- `code.R`: Main R script for data preprocessing, Bayesian estimation, and visualization.
- `data/`: Folder to store CPS extracts downloaded using `epiextractr`.
- `paper.pdf`: Final project report with methodology, diagnostics, and results.
- `README.md`: This documentation file.

## Dependencies

Install required R packages as follows:

```R
# epiextractr for CPS data
install.packages("epiextractr", repos = c("https://economic.r-universe.dev", "https://cloud.r-project.org"))

# Other packages
install.packages(c(
  "dplyr", "tidyverse", "VIM", "psych", "ggcorrplot", 
  "MCMCpack", "coda", "HDInterval"
))
```

## Data Source

The analysis uses the January 2025 CPS ORG dataset, which contains detailed individual-level information on wages, telework status, demographics, and job characteristics. The data was obtained using the epiextractr package developed by the Economic Policy Institute.

### Download Instructions

To reproduce the analysis:
  1. Install the required packages as above.
  2. Download CPS data:
```R
     download_cps("org", "data")
```
  3. Load and preprocess the data:
```R
     cps_data <- load_org(
       2025,
       year, month, age, female, citizen, wbhao, married,
       metstat, statecensus, educ, cow1, emp, union,
       agechild, mind03, telework, wageotc, hourslwt,
       .extracts_dir = "data"
     )
```
