# =============================================================================
# Submission-to-outcome time analysis
#
# Purpose:
#   Describe submission-to-approval times for CTA and PSA submissions, estimate
#   unadjusted differences using a study-cluster bootstrap, and conduct
#   covariate-adjusted mixed-effects analyses of the mean and variance.
#
# Outcome:
#   Calendar days from submission date to approval date.
#
# Notes:
#   - Update the input path for the environment in which the script is run.
#   - The analysis dataset should be stored separately from this script.
#   - The script assumes dates are recorded as day/month/year.
# =============================================================================


# 1. Packages and reproducibility ---------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(car)
library(glmmTMB)
library(emmeans)
library(gt)

set.seed(20261021)

# Set this to the directory containing the analysis CSV, or replace with a
# project-relative path (for example, here::here("data", "file.csv")).
data_file <- "IVDR_PSA_CTA_Publication_Analysis.csv"


# 2. Import and derive analysis variables -------------------------------------

# Dates are parsed using day/month/year order. The outcome is the elapsed
# calendar time between the submission and outcome dates.
df <- read_csv(
  data_file,
  na = c("", "NA", "N/A"),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  mutate(
    submission_date = dmy(submission_date),
    outcome_date = dmy(outcome_date),
    submission_to_outcome_days = as.numeric(
      outcome_date - submission_date
    ),
    year = factor(year(submission_date)),
    submission_type = factor(
      submission_type,
      levels = c("CTA", "PSA")
    ),
    country = factor(country),
    study_code = factor(study_code),
    phase = factor(phase),
    study_indication = factor(study_indication),
    regulatory_objective_lifecycle_state = factor(
      regulatory_objective_lifecycle_state,
      levels = c(
        "approved",
        "rejected",
        "withdrawn",
        "cancelled",
        "awaiting_outcome"
      )
    )
  )

# Report the initial record count as part of the analysis flow.
n_imported <- nrow(df)

# Retain approved submissions with a valid interval greater than three days.
# This excludes records without a final approved outcome, missing date
# intervals, and intervals of three days or less.
df <- df |>
  filter(
    regulatory_objective_lifecycle_state == "approved",
    !is.na(submission_to_outcome_days),
    submission_to_outcome_days > 3
  ) |>
  mutate(approval_time_days = submission_to_outcome_days)

n_after_outcome_and_interval_filter <- nrow(df)

# Complete-case analysis dataset for all variables used in the models.
approval_df <- df |>
  filter(
    !is.na(submission_type),
    !is.na(country),
    !is.na(year),
    !is.na(study_code),
    !is.na(approval_time_days),
    !is.na(phase),
    !is.na(submission_date),
    !is.na(outcome_date),
    !is.na(study_indication)
  ) |>
  droplevels()

n_analysis <- nrow(approval_df)

# Display the analysis flow and the distribution of observations by submission type
# and submission year.
tibble(
  stage = c(
    "Imported records",
    "After outcome and interval criteria",
    "Complete-case analysis dataset"
  ),
  n = c(
    n_imported,
    n_after_outcome_and_interval_filter,
    n_analysis
  )
)

approval_df |>
  count(submission_type, year)


# 3. Unadjusted descriptive statistics ---------------------------------------

# Median absolute deviation (MAD) is reported as a robust measure of spread.
median_absolute_deviation <- function(x) {
  median(abs(x - median(x, na.rm = TRUE)), na.rm = TRUE)
}

unadjusted_summary <- approval_df |>
  group_by(submission_type) |>
  summarise(
    n = n(),
    mean_days = mean(approval_time_days),
    median_days = median(approval_time_days),
    q1_days = quantile(approval_time_days, 0.25),
    q3_days = quantile(approval_time_days, 0.75),
    iqr_days = IQR(approval_time_days),
    mad_days = median_absolute_deviation(approval_time_days),
    p90_days = quantile(approval_time_days, 0.90),
    p95_days = quantile(approval_time_days, 0.95),
    n_over_150 = sum(approval_time_days > 150),
    n_over_180 = sum(approval_time_days > 180),
    pct_over_150 = mean(approval_time_days > 150),
    pct_over_180 = mean(approval_time_days > 180),
    .groups = "drop"
  )

# Descriptive summaries are presented separately by submission type.
unadjusted_summary |>
  gt() |>
  fmt_number(
    columns = c(
      mean_days, median_days, q1_days, q3_days,
      iqr_days, mad_days, p90_days, p95_days
    ),
    decimals = 1
  ) |>
  fmt_integer(
    columns = c(n, n_over_150, n_over_180)
  ) |>
  fmt_percent(
    columns = c(pct_over_150, pct_over_180),
    decimals = 1
  )


# 4. Study-cluster bootstrap for unadjusted submission type differences ---------------

# Study codes are resampled with replacement within each submission type.
# All observations belonging to a selected submission-type/study-code cluster
# are retained together, preserving within-study dependence. The number of
# clusters sampled in each submission type equals the number of observed clusters in
# that submission type.
B <- 10000

study_code_clusters <- approval_df |>
  distinct(submission_type, study_code)

# Calculate submission type-specific summaries and PSA-minus-CTA differences for a
# bootstrap sample.
bootstrap_statistics <- function(data) {
  data |>
    group_by(submission_type) |>
    summarise(
      n = n(),
      mean_days = mean(approval_time_days),
      median_days = median(approval_time_days),
      percentile_90_days = quantile(approval_time_days, 0.90),
      percentile_95_days = quantile(approval_time_days, 0.95),
      iqr_days = IQR(approval_time_days),
      mad_days = median_absolute_deviation(approval_time_days),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = submission_type,
      values_from = c(
        n, mean_days, median_days,
        percentile_90_days, percentile_95_days,
        iqr_days, mad_days
      ),
      names_sep = "_"
    ) |>
    mutate(
      difference_mean = mean_days_PSA - mean_days_CTA,
      difference_median = median_days_PSA - median_days_CTA,
      difference_percentile_90 =
        percentile_90_days_PSA - percentile_90_days_CTA,
      difference_percentile_95 =
        percentile_95_days_PSA - percentile_95_days_CTA,
      difference_iqr = iqr_days_PSA - iqr_days_CTA,
      difference_mad = mad_days_PSA - mad_days_CTA
    )
}

# Resample clusters separately within CTA and PSA. Joining by both submission type and
# study code ensures that each sampled cluster contributes all its records.
bootstrap_results <- map_dfr(seq_len(B), function(iteration) {
  sampled_clusters <- study_code_clusters |>
    group_split(submission_type, .keep = TRUE) |>
    map_dfr(function(cluster_data) {
      slice_sample(
        cluster_data,
        n = nrow(cluster_data),
        replace = TRUE
      )
    })
  
  bootstrap_data <- sampled_clusters |>
    left_join(
      approval_df,
      by = c("submission_type", "study_code"),
      relationship = "many-to-many"
    )
  
  bootstrap_statistics(bootstrap_data) |>
    mutate(iteration = iteration, .before = 1)
})

# Percentile confidence intervals are calculated from the bootstrap
# distribution. The point estimate is the corresponding difference in the
# original analysis dataset, rather than the median of the bootstrap draws.
bootstrap_ci <- function(x, estimate, conf = 0.95) {
  alpha <- 1 - conf
  
  tibble(
    estimate = estimate,
    lower_ci = quantile(
      x, probs = alpha / 2, na.rm = TRUE, names = FALSE
    ),
    upper_ci = quantile(
      x, probs = 1 - alpha / 2, na.rm = TRUE, names = FALSE
    )
  )
}

difference_variables <- c(
  "difference_mean",
  "difference_median",
  "difference_percentile_90",
  "difference_percentile_95",
  "difference_iqr",
  "difference_mad"
)

observed_differences <- bootstrap_statistics(approval_df)

difference_cis <- map_dfr(difference_variables, function(variable) {
  bootstrap_ci(
    x = bootstrap_results[[variable]],
    estimate = observed_differences[[variable]][[1]]
  ) |>
    mutate(statistic = variable, .before = 1)
})

difference_cis


# 5. Unadjusted comparison of variability ------------------------------------

# The Fligner–Killeen test is a rank-based test of equality of spread between
# submission types and is relatively robust to departures from normality.
fligner.test(
  approval_time_days ~ submission_type,
  data = approval_df
)

# The median-centred Levene test is an additional test of equality of variance.
# It assesses whether absolute deviations from group medians differ by submission type.
car::leveneTest(
  approval_time_days ~ submission_type,
  data = approval_df,
  center = median
)


# 6. Adjusted mixed-effects model of approval time ----------------------------

# Set reference categories so that model coefficients are interpreted relative
# to CTA, the specified reference year, and Phase III. Confirm that these
# levels are present in the analysis data before fitting the models.
approval_df <- approval_df |>
  mutate(
    submission_type = relevel(submission_type, ref = "CTA"),
    year = relevel(year, ref = "2025"),
    phase = relevel(phase, ref = "Phase III")
  )

# Additive Gaussian mixed-effects model:
#   - fixed effects adjust for submission type, year, country, and phase;
#   - a study-code random intercept accounts for clustering of observations
#     within studies;
#   - the dispersion model allows residual variance to differ by submission type.
model_additive <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

# Comparison model constraining residual variance to be common across submission types.
model_equal_variance <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ 1,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

# Likelihood-ratio test comparing the common-variance model with the model
# allowing submission type-specific residual variance.
anova(model_equal_variance, model_additive)

summary(model_additive)
summary(model_equal_variance)

# In a Gaussian glmmTMB model, dispersion coefficients are on the log residual
# variance scale. Exponentiating the PSA coefficient gives the estimated
# residual-variance ratio (PSA relative to CTA), conditional on the model.
disp_coef <- summary(model_additive)$coefficients$disp[
  "submission_typePSA", ]

log_variance_ratio <- disp_coef["Estimate"]
log_variance_se <- disp_coef["Std. Error"]

variance_ratio <- exp(log_variance_ratio)
variance_ratio_95CI <- exp(
  log_variance_ratio + c(-1, 1) * qnorm(0.975) * log_variance_se
)

tibble(
  variance_ratio_PSA_vs_CTA = variance_ratio,
  lower_95CI = variance_ratio_95CI[1],
  upper_95CI = variance_ratio_95CI[2]
)


# 7. Tests of year-related residual variance ---------------------------------

# These maximum-likelihood models test whether year improves the dispersion
# model after accounting for the same mean structure and study clustering.
model_var_submission_type <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

model_var_submission_type_year <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type + year,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

# Likelihood-ratio test for adding year to the dispersion model.
anova(model_var_submission_type, model_var_submission_type_year)
summary(model_var_submission_type_year)

# Test whether the submission type difference in residual variance varies by year.
model_var_submission_type_year_interaction <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type * year,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

anova(
  model_var_submission_type_year,
  model_var_submission_type_year_interaction
)

summary(model_var_submission_type_year_interaction)


# 8. Estimated residual-variance ratios by submission type and year ------------------

# Dispersion-model coefficients are on the log-variance scale. The following
# contrasts estimate year-specific variance ratios relative to 2025 within
# each submission type, with Wald confidence intervals and two-sided p-values.
b <- fixef(model_var_submission_type_year_interaction)$disp
V <- as.matrix(vcov(model_var_submission_type_year_interaction)$disp)

L_cta <- matrix(
  0, nrow = 2, ncol = length(b),
  dimnames = list(
    c("CTA 2023 vs 2025", "CTA 2024 vs 2025"),
    names(b)
  )
)
L_cta["CTA 2023 vs 2025", "year2023"] <- 1
L_cta["CTA 2024 vs 2025", "year2024"] <- 1

L_psa <- matrix(
  0, nrow = 2, ncol = length(b),
  dimnames = list(
    c("PSA 2023 vs 2025", "PSA 2024 vs 2025"),
    names(b)
  )
)
L_psa["PSA 2023 vs 2025", "year2023"] <- 1
L_psa["PSA 2023 vs 2025", "submission_typePSA:year2023"] <- 1
L_psa["PSA 2024 vs 2025", "year2024"] <- 1
L_psa["PSA 2024 vs 2025", "submission_typePSA:year2024"] <- 1

L <- rbind(L_cta, L_psa)

log_ratio <- drop(L %*% b)
log_ratio_se <- sqrt(diag(L %*% V %*% t(L)))
z_value <- log_ratio / log_ratio_se

variance_ratio_by_year <- data.frame(
  comparison = rownames(L),
  variance_ratio = exp(log_ratio),
  lower_95CI = exp(log_ratio - qnorm(0.975) * log_ratio_se),
  upper_95CI = exp(log_ratio + qnorm(0.975) * log_ratio_se),
  p_value = 2 * pnorm(abs(z_value), lower.tail = FALSE),
  row.names = NULL
)

variance_ratio_by_year


# PSA-to-CTA residual-variance ratios within year, from the fitted dispersion
# interaction model. Review emmeans output and package-version behavior before
# reporting these contrasts.
emm_disp <- emmeans(
  model_var_submission_type_year_interaction,
  ~ submission_type | year,
  component = "disp"
)

psa_cta_variance_contrasts <- contrast(
  emm_disp,
  method = "revpairwise",
  infer = c(TRUE, TRUE),
  type = "response"
)

psa_cta_variance_contrasts


# 9. Adjusted mean differences and year-by-submission type interaction ----------------

# This model allows the adjusted mean approval time to vary by submission type and year.
# The dispersion model also allows residual variance to vary by submission type and
# year. The interaction likelihood-ratio test compares this mean structure
# with an additive mean structure while retaining the same dispersion model.
model_year_interaction <- glmmTMB(
  approval_time_days ~ year * submission_type + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type * year,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

model_year_main_effects <- glmmTMB(
  approval_time_days ~ submission_type + year + country + phase +
    (1 | study_code),
  dispformula = ~ submission_type * year,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

anova(model_year_main_effects, model_year_interaction)

# Model-based adjusted means are estimated for each submission type-year combination.
# Within each submission type, year comparisons are reported without multiplicity
# adjustment, matching the original analysis.
emm_mean <- emmeans(
  model_year_interaction,
  ~ submission_type * year
)

year_comparisons_within_submission_type <- contrast(
  emm_mean,
  method = "revpairwise",
  by = "submission_type",
  adjust = "none"
) |>
  summary(infer = c(TRUE, TRUE))

year_comparisons_within_submission_type


# 10. Country-level random-effect sensitivity analyses ------------------------

# Fit submission type-specific models to assess whether adding a country random
# intercept improves fit within each submission type. These models include year and
# phase as fixed effects and assume a common residual variance.
cta_df <- approval_df |>
  filter(submission_type == "CTA") |>
  droplevels()

psa_df <- approval_df |>
  filter(submission_type == "PSA") |>
  droplevels()

cta_no_country <- glmmTMB(
  approval_time_days ~ year + phase,
  dispformula = ~ 1,
  family = gaussian(),
  data = cta_df
)

cta_country <- glmmTMB(
  approval_time_days ~ year + phase + (1 | country),
  dispformula = ~ 1,
  family = gaussian(),
  data = cta_df
)

psa_no_country <- glmmTMB(
  approval_time_days ~ year + phase,
  dispformula = ~ 1,
  family = gaussian(),
  data = psa_df
)

psa_country <- glmmTMB(
  approval_time_days ~ year + phase + (1 | country),
  dispformula = ~ 1,
  family = gaussian(),
  data = psa_df
)

anova(cta_no_country, cta_country)
anova(psa_no_country, psa_country)

VarCorr(cta_country)
VarCorr(psa_country)


# Joint model: common country-level variance across submission types.
country_common <- glmmTMB(
  approval_time_days ~ submission_type + year + phase +
    (1 | study_code) + (1 | country),
  dispformula = ~ 1,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

# Joint model: submission type-specific country-level variances. The uncorrelated
# random-effects specification allows separate country-level intercept
# variances by submission type.
country_separate <- glmmTMB(
  approval_time_days ~ submission_type + year + phase +
    (1 | study_code) + (0 + submission_type || country),
  dispformula = ~ 1,
  family = gaussian(),
  REML = FALSE,
  data = approval_df
)

# Likelihood-ratio test comparing common versus submission type-specific country
# variance structures.
anova(country_common, country_separate)
VarCorr(country_separate)



# =============================================================================
# Sensitivity analyses
#
# Each sensitivity analysis repeats the unadjusted variability tests and, where
# specified, compares a common-residual-variance model with a model allowing
# residual variance to differ by submission type.
#
# Likelihood-ratio comparisons use maximum likelihood and require the compared
# models to be fitted to the same observations with nested specifications.
# =============================================================================


# 1. Create sensitivity-analysis datasets -------------------------------------

# Phase III only
data_phase_III <- approval_df |>
  filter(phase == "Phase III") |>
  droplevels()

# Exclude submissions in 2025
data_exclude_2025 <- approval_df |>
  filter(lubridate::year(submission_date) != 2025) |>
  droplevels()

# Restrict to study codes represented in both submission submission types.
# The filter identifies study codes with at least one CTA and one PSA record.
data_both_submission_types <- approval_df |>
  group_by(study_code) |>
  filter(all(c("CTA", "PSA") %in% as.character(submission_type))) |>
  ungroup() |>
  droplevels()

# Consolidate PSA observations within study and country when the range of
# approval times is no greater than 10 days. For groups exceeding this
# threshold, retain the original records. CTA observations are unchanged.
psa_consolidated <- approval_df |>
  filter(submission_type == "PSA") |>
  group_by(study_code, country) |>
  group_modify(~ {
    if (max(.x$approval_time_days) - min(.x$approval_time_days) <= 10) {
      .x[1, ] |>
        mutate(
          approval_time_days = mean(.x$approval_time_days),
          n_consolidated = nrow(.x)
        )
    } else {
      .x |>
        mutate(n_consolidated = 1L)
    }
  }) |>
  ungroup()

cta_unchanged <- approval_df |>
  filter(submission_type == "CTA") |>
  mutate(n_consolidated = 1L)

data_consolidated_PSA <- bind_rows(
  cta_unchanged,
  psa_consolidated
) |>
  droplevels()

# Restrict to countries with at least five observations in each submission type.
country_counts <- approval_df |>
  count(country, submission_type) |>
  tidyr::pivot_wider(
    names_from = submission_type,
    values_from = n,
    values_fill = 0
  )

# Ensure both submission type columns exist, even if one is absent in a dataset.
if (!"CTA" %in% names(country_counts)) country_counts$CTA <- 0L
if (!"PSA" %in% names(country_counts)) country_counts$PSA <- 0L

eligible_countries <- country_counts |>
  filter(CTA >= 5, PSA >= 5) |>
  pull(country)

data_min_5_per_country <- approval_df |>
  filter(country %in% eligible_countries) |>
  droplevels()

# Record sample sizes for reporting.
sensitivity_sample_sizes <- tibble(
  analysis = c(
    "Phase III only",
    "Excluding 2025",
    "Study codes represented in both submission types",
    "PSA observations consolidated",
    "Countries with at least 5 records per submission type"
  ),
  n = c(
    nrow(data_phase_III),
    nrow(data_exclude_2025),
    nrow(data_both_submission_types),
    nrow(data_consolidated_PSA),
    nrow(data_min_5_per_country)
  )
)

sensitivity_sample_sizes


# 2. Reusable functions --------------------------------------------------------

# Run both unadjusted tests of differences in spread. The Fligner-Killeen test
# is rank-based; the median-centred Levene test compares absolute deviations
# from group medians.
run_unadjusted_variability_tests <- function(data, analysis_name) {
  data <- data |>
    filter(!is.na(approval_time_days), !is.na(submission_type)) |>
    droplevels()
  
  cat("\n\n### ", analysis_name, " ###\n", sep = "")
  
  print(
    fligner.test(
      approval_time_days ~ submission_type,
      data = data
    )
  )
  
  print(
    car::leveneTest(
      approval_time_days ~ submission_type,
      data = data,
      center = median
    )
  )
}

# Fit common- and submission type-specific-residual-variance models and compare them.
# `mean_formula` is supplied separately because the Phase III analysis uses
# study_indication, whereas the other specified models use phase.
run_variance_model_comparison <- function(
    data,
    mean_formula,
    analysis_name,
    random_study_intercept = TRUE) {
  
  data <- data |>
    filter(
      !is.na(approval_time_days),
      !is.na(submission_type)
    ) |>
    droplevels() |>
    mutate(
      submission_type = relevel(submission_type, ref = "CTA")
    )
  
  if (random_study_intercept) {
    mean_formula <- update(
      mean_formula,
      . ~ . + (1 | study_code)
    )
  }
  
  model_common_variance <- glmmTMB(
    formula = mean_formula,
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = data
  )
  
  model_by_submission_type_variance <- glmmTMB(
    formula = mean_formula,
    dispformula = ~ submission_type,
    family = gaussian(),
    REML = FALSE,
    data = data
  )
  
  cat("\n\n### ", analysis_name, " ###\n", sep = "")
  cat("\nLikelihood-ratio comparison:\n")
  print(anova(model_common_variance, model_by_submission_type_variance))
  
  cat("\nModel allowing submission type-specific residual variance:\n")
  print(summary(model_by_submission_type_variance))
  
  # The PSA dispersion coefficient is the log residual-variance ratio
  # relative to CTA. Exponentiation gives the PSA-to-CTA variance ratio.
  dispersion_coefficients <-
    summary(model_by_submission_type_variance)$coefficients$disp
  
  if ("submission_typePSA" %in% rownames(dispersion_coefficients)) {
    log_ratio <- dispersion_coefficients[
      "submission_typePSA", "Estimate"
    ]
    log_ratio_se <- dispersion_coefficients[
      "submission_typePSA", "Std. Error"
    ]
    
    variance_ratio_summary <- tibble(
      analysis = analysis_name,
      variance_ratio_PSA_vs_CTA = exp(log_ratio),
      lower_95CI = exp(log_ratio - qnorm(0.975) * log_ratio_se),
      upper_95CI = exp(log_ratio + qnorm(0.975) * log_ratio_se)
    )
    
    cat("\nPSA-to-CTA residual-variance ratio:\n")
    print(variance_ratio_summary)
  } else {
    warning(
      "The PSA dispersion coefficient was not found for: ",
      analysis_name
    )
    variance_ratio_summary <- NULL
  }
  
  invisible(list(
    common_variance_model = model_common_variance,
    submission_type_variance_model = model_by_submission_type_variance,
    variance_ratio = variance_ratio_summary
  ))
}


# 3. Phase III-only sensitivity analysis -------------------------------------

run_unadjusted_variability_tests(
  data_phase_III,
  "Phase III only"
)

# Phase is not included in this model because all observations are Phase III.
# The original sensitivity specification adjusts for year, country, and
# study_indication and includes a study-level random intercept.
phase_III_models <- run_variance_model_comparison(
  data = data_phase_III,
  mean_formula =
    approval_time_days ~ submission_type + year + country +
    study_indication,
  analysis_name = "Phase III only",
  random_study_intercept = TRUE
)


# 4. Excluding 2025 -----------------------------------------------------------

run_unadjusted_variability_tests(
  data_exclude_2025,
  "Excluding 2025"
)

exclude_2025_models <- run_variance_model_comparison(
  data = data_exclude_2025,
  mean_formula =
    approval_time_days ~ submission_type + year + country + phase,
  analysis_name = "Excluding 2025",
  random_study_intercept = TRUE
)


# 5. Study codes represented in both submission types -------------------------------

run_unadjusted_variability_tests(
  data_both_submission_types,
  "Study codes represented in both submission types"
)

both_submission_types_models <- run_variance_model_comparison(
  data = data_both_submission_types,
  mean_formula =
    approval_time_days ~ submission_type + year + country + phase,
  analysis_name = "Study codes represented in both submission types",
  random_study_intercept = TRUE
)


# 6. Consolidating PSA observations ------------------------------------------

run_unadjusted_variability_tests(
  data_consolidated_PSA,
  "PSA observations consolidated within study and country"
)

consolidated_PSA_models <- run_variance_model_comparison(
  data = data_consolidated_PSA,
  mean_formula =
    approval_time_days ~ submission_type + year + country + phase,
  analysis_name = "PSA observations consolidated",
  random_study_intercept = TRUE
)


# 7. Restricting to countries with at least five observations per submission type ------

run_unadjusted_variability_tests(
  data_min_5_per_country,
  "Countries with at least five observations per submission type"
)

# The original specification for this sensitivity analysis does not include a
# study_code random intercept. This is retained here; the fixed-effects model
# therefore does not account for within-study clustering.
min_5_models <- run_variance_model_comparison(
  data = data_min_5_per_country,
  mean_formula =
    approval_time_days ~ submission_type + year + country + phase,
  analysis_name = "At least five observations per country per submission type",
  random_study_intercept = FALSE
)


# 8. Country random-effect analyses ------------------------------------------

# These analyses assess whether adding country-level random intercepts changes
# model fit within each submission type. Models compared within a submission type use the same
# observations and fixed-effects structure.
run_country_random_effect_analysis <- function(data, analysis_name) {
  cta_data <- data |>
    filter(submission_type == "CTA") |>
    droplevels()
  
  psa_data <- data |>
    filter(submission_type == "PSA") |>
    droplevels()
  
  cta_without_country <- glmmTMB(
    approval_time_days ~ year + phase,
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = cta_data
  )
  
  cta_with_country <- glmmTMB(
    approval_time_days ~ year + phase + (1 | country),
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = cta_data
  )
  
  psa_without_country <- glmmTMB(
    approval_time_days ~ year + phase,
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = psa_data
  )
  
  psa_with_country <- glmmTMB(
    approval_time_days ~ year + phase + (1 | country),
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = psa_data
  )
  
  cat("\n\n### Country random-effect analysis: ", analysis_name, " ###\n",
      sep = "")
  cat("\nCTA: without versus with country random intercept\n")
  print(anova(cta_without_country, cta_with_country))
  print(VarCorr(cta_with_country))
  
  cat("\nPSA: without versus with country random intercept\n")
  print(anova(psa_without_country, psa_with_country))
  print(VarCorr(psa_with_country))
  
  invisible(list(
    cta_without_country = cta_without_country,
    cta_with_country = cta_with_country,
    psa_without_country = psa_without_country,
    psa_with_country = psa_with_country
  ))
}

consolidated_country_models <- run_country_random_effect_analysis(
  data_consolidated_PSA,
  "PSA observations consolidated"
)

min_5_country_models <- run_country_random_effect_analysis(
  data_min_5_per_country,
  "At least five observations per country per submission type"
)


# Joint comparison of common versus submission type-specific country-level variances.
# Both models include a study random intercept and are fitted by maximum
# likelihood to the same dataset.
run_joint_country_variance_comparison <- function(data, analysis_name) {
  common_country_variance <- glmmTMB(
    approval_time_days ~ submission_type + year + phase +
      (1 | study_code) + (1 | country),
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = data
  )
  
  separate_country_variances <- glmmTMB(
    approval_time_days ~ submission_type + year + phase +
      (1 | study_code) + (0 + submission_type || country),
    dispformula = ~ 1,
    family = gaussian(),
    REML = FALSE,
    data = data
  )
  
  cat("\n\n### Joint country variance comparison: ",
      analysis_name, " ###\n", sep = "")
  print(anova(common_country_variance, separate_country_variances))
  print(VarCorr(separate_country_variances))
  
  invisible(list(
    common_country_variance = common_country_variance,
    separate_country_variances = separate_country_variances
  ))
}

consolidated_joint_country_models <-
  run_joint_country_variance_comparison(
    data_consolidated_PSA,
    "PSA observations consolidated"
  )

min_5_joint_country_models <-
  run_joint_country_variance_comparison(
    data_min_5_per_country,
    "At least five observations per country per submission type"
  )
