# ------------------------------------------------------------------------------
# 1. Setup & Data Loading
# ------------------------------------------------------------------------------
# install.packages("epiextractr", repos = c("https://economic.r-universe.dev", "https://cloud.r-project.org"))
library(epiextractr)   # For CPS data
library(VIM)           # Hot deck imputation
library(psych)         # Descriptive statistics
library(MCMCpack)      # MCMClogit
library(coda)          # MCMC diagnostics
library(tidyverse)     # Data manipulation
library(ggcorrplot)    # Correlation heatmap
library(HDInterval)    # HDI intervals

# Set path
setwd("/Bayesian Analysis/project")

# Uncomment the following line to download data automatically in 'data' folder:
# You do not have to do it if you already have file 'epi_cpsorg_2025_1.feather'
# download_cps("org", "data")

cps_data <- load_org(
  2025,
  year, month, age, female, citizen, wbhao, married,
  metstat, statecensus, educ, cow1, emp, union,
  agechild, mind03, telework, wageotc, hourslwt,
  .extracts_dir = "data" # folder with data
) %>%
  as_factor() %>%
  filter(
    emp == "Employed",
    !cow1 %in% c("Without pay"),
    month == 1
  ) %>%
  mutate(
    telework = case_when(
      telework == "Teleworked" ~ TRUE,
      telework == "Did not telework" ~ FALSE,
      TRUE ~ NA
    ),
    age = ifelse(age == "80+", 80, as.numeric(age)),
    hourslwt = ifelse(hourslwt == "99+", 99, as.numeric(hourslwt))
  ) %>%
  rename(
    children = agechild,
    hours = hourslwt,
    state = statecensus,
    sex = female,
    race = wbhao,
    metropolitan = metstat,
    education = educ,
    job_class = cow1,
    industry = mind03,
    wage = wageotc
  ) %>%
  select(age, sex, married, children, citizen, race, metropolitan, state,
         education, union, job_class, industry, hours, telework, wage) %>%
  droplevels()

# ------------------------------------------------------------------------------
# Missing Data Assessment & Imputation
# ------------------------------------------------------------------------------
cat("Initial Missing Data Proportions:\n")
print(cps_data %>% summarise_all(~ mean(is.na(.))), width = Inf)

set.seed(123)
cps_data <- hotdeck(
  cps_data,
  variable = c("union", "hours", "telework", "wage"),
  domain_var = c("age", "sex", "race", "education")
) %>% as_tibble()

cat("Post-Imputation Missing Data Proportions:\n")
print(cps_data %>% summarise_all(~ mean(is.na(.))), width = Inf)

# Remove any remaining records with missing values and zero wages
cps_data <- cps_data %>%
  filter(!if_any(c(metropolitan, union, hours, telework, wage), is.na)) %>%
  mutate(is_imputed = rowSums(select(., ends_with("_imp"))) > 0) %>%
  select(age, sex, married, children, citizen, race, metropolitan, state,
         education, union, job_class, industry, hours, telework, wage, is_imputed)

cat("Data Structure:\n")
str(cps_data)

# ------------------------------------------------------------------------------
# 3. Exploratory Data Analysis (EDA)
# ------------------------------------------------------------------------------
# Create log-transformed wage variable
cps_data <- cps_data %>% mutate(log_wage = log(wage))

## 3.1 Summary Statistics
# Continuous Variables
continuous_vars <- cps_data %>% select(age, hours, wage, log_wage)
cat("Summary Statistics (Continuous):\n")
print(summary(continuous_vars))
print(psych::describe(continuous_vars))

# Categorical Variables: Percentage Distribution
categorical_vars <- cps_data %>% select(sex, married, children, citizen, race, 
                                        metropolitan, state, education, union, 
                                        job_class, industry, telework)
cat("\nCategorical Variable Distributions (%):\n")
for (var in names(categorical_vars)) {
  cat("\n", var, ":\n")
  print(round(prop.table(table(categorical_vars[[var]])) * 100, 1))
}

## 3.2 Visualizations
# Define a simple theme for plots
plot_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# 3.2.1 Histograms & Density Plots for Wage & Log(Wage)
cont_vars <- c("wage", "log_wage")
binwidths <- c(5, 0.1)

for(i in seq_along(cont_vars)) {
  var <- cont_vars[i]
  bw <- binwidths[i]
  
  # Histogram
  p_hist <- ggplot(cps_data, aes_string(x = var)) +
    geom_histogram(binwidth = bw, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    plot_theme
  print(p_hist)
  
  # Density Plot
  p_dens <- ggplot(cps_data, aes_string(x = var)) +
    geom_density(fill = "lightblue") +
    labs(title = paste("Density Plot of", var), x = var, y = "Density") +
    plot_theme
  print(p_dens)
}

# 3.2.2 Boxplots of Log(Wage) by Selected Categorical Variables
cat_vars_box <- c("telework", "education", "sex", "married", "children", "race")
for (var in cat_vars_box) {
  p_box <- ggplot(cps_data, aes_string(x = var, y = "log_wage")) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Log(Wage) by", var), x = var, y = "Log(Wage)") +
    plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_box)
}

# 3.2.3 Scatter Plots: Log(Wage) vs Continuous Predictors, Colored by Telework
scatter_vars <- c("age", "hours")
for (var in scatter_vars) {
  p_scatter <- ggplot(cps_data, aes_string(x = var, y = "log_wage", color = "as.factor(telework)")) +
    geom_jitter(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "black") +
    labs(title = paste("Log(Wage) vs", var, "by Telework Status"),
         x = var, y = "Log(Wage)", color = "Telework") +
    plot_theme
  print(p_scatter)
}

# ------------------------------------------------------------------------------
# 3. Advanced EDA: Correlation & Normality Check
# ------------------------------------------------------------------------------
# Prepare numeric variables (converting categorical flags to numeric)
numeric_vars <- cps_data %>%
  mutate(
    citizen_num = ifelse(citizen == "US citizen", 1, 0),
    married_num = ifelse(married == "Married", 1, 0),
    female = ifelse(sex == "Female", 1, 0),
    metro_num = ifelse(metropolitan == "Metropolitan", 1, 0)
  ) %>%
  select(age, telework, log_wage, female, citizen_num, married_num, metro_num)

# Correlation Heatmap
cor_matrix <- cor(numeric_vars, use = "complete.obs")
p_corr <- ggcorrplot::ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap")
print(p_corr)

# Q-Q Plot for Log(Wage)
p_qq <- ggplot(cps_data, aes(sample = log_wage)) +
  stat_qq(shape = 16, size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q-Q Plot of Log(Wage)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  plot_theme
print(p_qq)

# ------------------------------------------------------------------------------
# 4. Design Stage: Bayesian PS Estimation & MCMC Diagnostics
# ------------------------------------------------------------------------------
# Set number of MCMC chains
n_chains <- 4
mcmc_list_raw <- vector("list", n_chains)

# Run Bayesian logistic regression (MCMClogit) directly on 'cps_data'
# The inline formula uses telework as the treatment indicator and includes all relevant predictors.
for (i in seq_len(n_chains)) {
  chain_seed <- sample.int(1e6, 1)
  mcmc_list_raw[[i]] <- MCMClogit(
    telework ~ age + sex + married + children + citizen + race + metropolitan + state +
      education + union + job_class + industry + hours,
    data    = cps_data,
    burnin  = 5000,      # Number of burn-in iterations
    mcmc    = 125000,    # Total MCMC iterations
    thin    = 500,       # Thinning interval
    tune    = 0.242,     # Tuning parameter for proposal step
    b0      = 0,         # Prior mean
    B0      = 0.1,       # Prior precision (weakly informative)
    verbose = 25000,     # Frequency of progress messages
    seed    = chain_seed
  )
}

# Convert raw chains to an mcmc.list and combine all chains into one mcmc object
mcmc_list <- mcmc.list(lapply(mcmc_list_raw, as.mcmc))
combined_chain <- as.mcmc(do.call(rbind, lapply(mcmc_list, as.matrix)))
summary(combined_chain)
# ------------------------------------------------------------------------------
# CODA
# ------------------------------------------------------------------------------
# 1. Trace & Density Plots: Display the evolution and distribution of parameters across chains.
plot(mcmc_list)

# 2. Autocorrelation Diagnostics:
autocorr_results <- autocorr.diag(combined_chain, lags = c(0, 1, 5, 10, 100))
cat("Autocorrelation Diagnostics:\n")
print(autocorr_results)
autocorr.plot(combined_chain, lag.max = 50)

# 3. Gelman-Rubin Diagnostic:
#    - Print numeric results and display the Gelman-Rubin plot to assess convergence.
gelman_results <- gelman.diag(mcmc_list, autoburnin = FALSE)
cat("Gelman-Rubin Diagnostic Results:\n")
print(gelman_results)

# ------------------------------------------------------------------------------
# Create Design Matrix & Compute Posterior Propensity Score Draws
# ------------------------------------------------------------------------------
# Build a design matrix for covariates (including the intercept) using model.matrix.
covars <- model.matrix(~ age + sex + married + children + citizen + race + metropolitan + state +
                         education + union + job_class + industry + hours, data = cps_data)

# Convert the combined MCMC chain to a numeric matrix of posterior coefficients.
posterior_coefs <- as.matrix(combined_chain)

# Ensure that the design matrix column names match the posterior coefficients' order.
common_cols <- intersect(colnames(covars), colnames(posterior_coefs))
covars <- covars[, colnames(posterior_coefs), drop = FALSE]

# Define the expit (logistic) function.
expit <- function(x) 1 / (1 + exp(-x))

# Compute the posterior propensity score draws.
# Result: An n x M matrix where each row is an individual and each column is a posterior draw.
ps_draws <- expit(covars %*% t(posterior_coefs))

# Inspect the dimensions and a sample of the posterior propensity score draws.
cat("Dimensions of PS Draws:", dim(ps_draws), "\n")
print(head(ps_draws[, 1:5]))

## ------------------------------------------------------------------------------
## 4. Analysis Stage
## ------------------------------------------------------------------------------

# Function to compute treatment effect draws for a given PS draw
compute_ate <- function(ps_draw, data, S = 200, strat_cut = 5) {
  # Create the analysis dataset with outcome, treatment, PS, and strata
  analysis_data <- data %>%
    mutate(
      outcome   = log(wage),
      treatment = telework,
      ps        = ps_draw,
      strata    = cut(ps,
                      breaks = quantile(ps, probs = seq(0, 1, length.out = strat_cut + 1)),
                      include.lowest = TRUE,
                      labels = FALSE)
    )
  
  # Retain strata that include both treatment groups
  valid_strata <- analysis_data %>%
    group_by(strata) %>%
    summarize(has_both = all(c(TRUE, FALSE) %in% unique(treatment)), .groups = "drop") %>%
    filter(has_both) %>%
    pull(strata)
  
  if (length(valid_strata) < 2) return(NULL)
  
  analysis_data <- analysis_data %>%
    filter(strata %in% valid_strata) %>%
    mutate(strata = factor(strata))
  
  # Calculate the proportion of observations in each stratum
  # The first stratum is  used as the reference level, so I exclude it when computing weights for interactions
  strata_props <- analysis_data %>%
    count(strata) %>%
    arrange(as.numeric(strata)) %>%
    filter(strata != levels(analysis_data$strata)[1]) %>%
    pull(n) / nrow(analysis_data)
  
  # Fit Bayesian linear regression with interaction between treatment and strata
  fit <- MCMCregress(
    outcome ~ treatment * strata,
    data    = analysis_data,
    burnin  = 500,
    mcmc    = S * 5,
    thin    = 5,
    verbose = 0
  )
  
  fit_matrix <- as.matrix(fit)
  beta_treatment <- fit_matrix[, "treatmentTRUE"]
  
  # Extract and order interaction coefficients by stratum
  interaction_cols <- grep("treatmentTRUE:strata", colnames(fit_matrix), value = TRUE)
  interaction_cols <- interaction_cols[order(as.numeric(gsub("treatmentTRUE:strata", "", interaction_cols)))]
  beta_interaction <- fit_matrix[, interaction_cols, drop = FALSE]
  weighted_interaction <- as.vector(beta_interaction %*% strata_props)
  
  # Treatment effect draws for the current PS draw
  treat_effect_draws <- beta_treatment + weighted_interaction
  
  list(
    treat_effect_draws = treat_effect_draws,
    fit = fit,
    balance_data = analysis_data
  )
}

# Apply the function to each column of ps_draws and remove any NULL results
results <- map(1:ncol(ps_draws), function(k) compute_ate(ps_draws[, k], cps_data)) %>%
  compact()

## CODA for a Representative Chain
rep_fit <- results[[1]]$fit
summary(rep_fit)
# 1. Trace & Density Plots: Display the evolution and distribution of parameters across chains.
plot(rep_fit)

# 2. Autocorrelation Diagnostics:
autocorr_results <- autocorr.diag(rep_fit, lags = c(0, 1, 5, 10, 100))
cat("Autocorrelation Diagnostics:\n")
print(autocorr_results)
autocorr.plot(rep_fit, lag.max = 50)

## Covariate Balance Check (by treatment group)
balance_data <- results[[1]]$balance_data
cat("\nCovariate Balance Summary (by Treatment):\n")
balance_summary <- balance_data %>%
  group_by(strata, treatment) %>%
  summarise(across(c(age, hours, outcome),
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-c(strata, treatment), names_to = c("variable", ".value"), names_sep = "_") %>%
  pivot_wider(names_from = treatment, values_from = c(mean, sd)) %>%
  mutate(diff_mean = abs(mean_TRUE - mean_FALSE),
         pooled_sd = sqrt((sd_TRUE^2 + sd_FALSE^2) / 2),
         std_diff = diff_mean / pooled_sd)

print(balance_summary)

## Treatment Effects Analysis
# Combine treatment effect draws across PS draws (each column is one design)
treat_effect_matrix <- do.call(cbind, map(results, "treat_effect_draws"))
all_effects <- as.vector(treat_effect_matrix)

# Plot the posterior distribution of treatment effects
ggplot(data.frame(TreatmentEffect = all_effects), aes(x = TreatmentEffect)) +
  geom_histogram(fill = "skyblue", alpha = 0.5, bins = 200) +
  labs(title = "Posterior Distribution of ATE",
       x = "Treatment Effect",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Decompose variance using Rubin's combining rules
within_var <- mean(apply(treat_effect_matrix, 2, stats::var))
between_var <- stats::var(colMeans(treat_effect_matrix))
K <- ncol(treat_effect_matrix)
total_var <- within_var + (1 + 1 / K) * between_var

cat("\nWithin-design Variance:", within_var, "\n")
cat("Between-design Variance:", between_var, "\n")
cat("Total Variance (Rubin's rules):", total_var, "\n")

# Summarize the posterior distribution of treatment effects
overall_mean <- mean(all_effects)
overall_ci <- quantile(all_effects, probs = c(0.025, 0.975))
hdi_interval <- hdi(all_effects, credMass = 0.95)
wald_interval <- c(overall_mean - 1.96 * sqrt(total_var), overall_mean + 1.96 * sqrt(total_var))

cat("\nCombined ATE Posterior Mean: ", round(overall_mean, 3), "\n",
    "95% Equi-tailed Credible Interval: ", round(overall_ci[1], 3), " to ", round(overall_ci[2], 3), "\n",
    "95% Highest Density Interval (HDI): ", round(hdi_interval[1], 3), " to ", round(hdi_interval[2], 3), "\n",
    "Wald-type Interval: ", round(wald_interval[1], 3), " to ", round(wald_interval[2], 3), "\n")
