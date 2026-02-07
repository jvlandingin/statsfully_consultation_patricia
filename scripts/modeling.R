# =============================================================================
# modeling.R
# Panel Data Analysis: Tax Structure Composition and SDG Outcomes
#
# This script performs all model fitting, testing, and diagnostics.
# Results are saved to outputs/analysis_results.rds for consumption by the
# Quarto report (thesis_analysis_v2.qmd).
# =============================================================================

library(targets)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)
library(broom)

# =============================================================================
# SECTION A: Data Loading
# =============================================================================

tar_load(tax_structure_and_sdg_complete)
tar_load(best_sdg_combination)

# =============================================================================
# SECTION B: Safe PCA Recomputation
# =============================================================================

# Extract selected variable names by SDG group
sdg3_vars <- best_sdg_combination$combination[
  grepl("^sdg3_", best_sdg_combination$combination)
]
sdg4_vars <- best_sdg_combination$combination[
  grepl("^sdg4_", best_sdg_combination$combination)
]

# Recompute PCA on the final complete data (avoids fragile bind_cols alignment)
sdg3_pca <- prcomp(
  tax_structure_and_sdg_complete[, sdg3_vars],
  scale. = TRUE
)
sdg4_pca <- prcomp(
  tax_structure_and_sdg_complete[, sdg4_vars],
  scale. = TRUE
)

cat(
  "SDG3 PC1 variance explained:",
  round(summary(sdg3_pca)$importance["Proportion of Variance", 1], 3),
  "\n"
)
cat(
  "SDG4 PC1 variance explained:",
  round(summary(sdg4_pca)$importance["Proportion of Variance", 1], 3),
  "\n"
)

# =============================================================================
# SECTION C: Build Panel Data
# =============================================================================

# Start from the complete data, attach PCA scores, classify tax, scale mods
panel_data <- tax_structure_and_sdg_complete |>
  # Drop individual SDG columns (replaced by PCA indices)
  select(-all_of(c(sdg3_vars, sdg4_vars))) |>
  # Attach PCA PC1 scores (safe: same rows, same order as input)
  mutate(
    sdg3_index = as.numeric(sdg3_pca$x[, 1]),
    sdg4_index = as.numeric(sdg4_pca$x[, 1])
  ) |>
  # Tax classification (10pp threshold rule)
  mutate(
    tax_composition = case_when(
      tax_general_consumption - tax_income_and_profits >= 10 ~
        "predominant_consumption_tax",
      tax_income_and_profits - tax_general_consumption >= 10 ~
        "predominant_income_tax",
      .default = "balanced_tax_mix"
    ),
    tax_composition = factor(
      tax_composition,
      levels = c(
        "balanced_tax_mix",
        "predominant_consumption_tax",
        "predominant_income_tax"
      )
    )
  ) |>
  # Remove raw tax variables

  select(
    -tax_general_consumption,
    -tax_income_and_profits,
    -tax_goods_and_services
  ) |>
  # Scale moderators (as.numeric avoids matrix-in-tibble issue)
  mutate(
    across(
      matches("^mod_"),
      ~ as.numeric(scale(.))
    )
  )

cat(
  "Panel data:",
  nrow(panel_data),
  "observations,",
  n_distinct(panel_data$country),
  "countries,",
  min(panel_data$year),
  "-",
  max(panel_data$year),
  "\n"
)
cat("Tax composition distribution:\n")
print(table(panel_data$tax_composition))

# =============================================================================
# SECTION D: Define Formulas
# =============================================================================

sdg3_formula <- sdg3_index ~
  tax_composition +
  mod_macroeconomic_population +
  mod_macroeconomic_population:tax_composition +
  mod_gdp_per_capita +
  mod_gdp_per_capita:tax_composition +
  mod_debt_to_gdp_ratio +
  mod_debt_to_gdp_ratio:tax_composition

sdg4_formula <- sdg4_index ~
  tax_composition +
  mod_macroeconomic_population +
  mod_macroeconomic_population:tax_composition +
  mod_gdp_per_capita +
  mod_gdp_per_capita:tax_composition +
  mod_debt_to_gdp_ratio +
  mod_debt_to_gdp_ratio:tax_composition

# =============================================================================
# SECTION E: Model Fitting
# =============================================================================

fit_all_models <- function(formula, data) {
  # Pooling model (needed for BP-LM test)
  pooling <- plm(
    formula,
    data = data,
    index = c("country", "year"),
    model = "pooling"
  )

  # Pooled OLS (for Chow test and comparison)
  ols <- lm(formula, data = data)

  # Fixed Effects
  fe <- plm(
    formula,
    data = data,
    index = c("country", "year"),
    model = "within"
  )

  # Random Effects (Wallace-Hussain due to near time-invariance)
  re <- plm(
    formula,
    data = data,
    index = c("country", "year"),
    model = "random",
    random.method = "walhus"
  )

  list(pooling = pooling, ols = ols, fe = fe, re = re)
}

# =============================================================================
# SECTION F: Model Selection Tests
# =============================================================================

run_selection_tests <- function(models) {
  # BP-LM: Pooled vs RE
  bplm <- plmtest(models$pooling, type = "bp")

  # Chow/F-test: Pooled vs FE
  chow <- pFtest(models$fe, models$ols)

  # Hausman: FE vs RE
  hausman <- tryCatch(
    phtest(models$fe, models$re),
    error = function(e) {
      cat("  Hausman test failed:", e$message, "\n")
      cat("  Defaulting to Fixed Effects\n")
      list(statistic = NA, p.value = 0, method = "Hausman (failed)")
    }
  )

  # Choose model based on Hausman
  if (is.na(hausman$p.value) || hausman$p.value < 0.05) {
    chosen_name <- "fe"
  } else {
    chosen_name <- "re"
  }

  list(
    bplm = bplm,
    chow = chow,
    hausman = hausman,
    chosen_name = chosen_name,
    chosen_model = models[[chosen_name]]
  )
}

# =============================================================================
# SECTION G: Cluster-Robust Standard Errors
# =============================================================================

compute_robust_inference <- function(chosen_model) {
  robust_vcov <- vcovHC(chosen_model, type = "HC1", cluster = "group")
  robust_coeftest <- lmtest::coeftest(chosen_model, vcov. = robust_vcov)

  list(vcov = robust_vcov, coeftest = robust_coeftest)
}

# =============================================================================
# SECTION H: Diagnostics
# =============================================================================

run_diagnostics <- function(chosen_model) {
  # Serial correlation
  bg <- pbgtest(chosen_model)

  # Cross-sectional dependence
  cd <- tryCatch(
    pcdtest(chosen_model, test = "cd"),
    error = function(e) {
      list(
        statistic = NA,
        p.value = NA,
        method = "Pesaran CD (insufficient panel overlap)"
      )
    }
  )

  # VIF
  vif_result <- tryCatch(
    car::vif(chosen_model),
    error = function(e) NULL
  )

  list(bg = bg, cd = cd, vif = vif_result)
}

# =============================================================================
# SECTION I: Hypothesis Testing via linearHypothesis
# =============================================================================

run_hypothesis_tests <- function(chosen_model, robust_vcov) {
  # Coefficient names for tax composition dummies
  consumption <- "tax_compositionpredominant_consumption_tax"
  income <- "tax_compositionpredominant_income_tax"

  # Interaction coefficient names
  consumption_pop <- "tax_compositionpredominant_consumption_tax:mod_macroeconomic_population"
  income_pop <- "tax_compositionpredominant_income_tax:mod_macroeconomic_population"
  consumption_gdp <- "tax_compositionpredominant_consumption_tax:mod_gdp_per_capita"
  income_gdp <- "tax_compositionpredominant_income_tax:mod_gdp_per_capita"
  consumption_debt <- "tax_compositionpredominant_consumption_tax:mod_debt_to_gdp_ratio"
  income_debt <- "tax_compositionpredominant_income_tax:mod_debt_to_gdp_ratio"

  # H1/H8: Joint significance of tax composition
  h1 <- linearHypothesis(
    chosen_model,
    c(paste(consumption, "= 0"), paste(income, "= 0")),
    vcov. = robust_vcov,
    test = "F"
  )

  # H2/H9: Income vs Consumption (contrast)
  h2 <- linearHypothesis(
    chosen_model,
    paste(income, "-", consumption, "= 0"),
    vcov. = robust_vcov,
    test = "F"
  )

  # H3/H10: Income vs Balanced (direct coefficient)
  h3 <- linearHypothesis(
    chosen_model,
    paste(income, "= 0"),
    vcov. = robust_vcov,
    test = "F"
  )

  # H4/H11: Balanced vs Consumption (direct coefficient)
  h4 <- linearHypothesis(
    chosen_model,
    paste(consumption, "= 0"),
    vcov. = robust_vcov,
    test = "F"
  )

  # H5/H12: Population moderation (joint test on both interactions)
  h5 <- linearHypothesis(
    chosen_model,
    c(paste(consumption_pop, "= 0"), paste(income_pop, "= 0")),
    vcov. = robust_vcov,
    test = "F"
  )

  # H6/H13: GDP moderation (joint test)
  h6 <- linearHypothesis(
    chosen_model,
    c(paste(consumption_gdp, "= 0"), paste(income_gdp, "= 0")),
    vcov. = robust_vcov,
    test = "F"
  )

  # H7/H14: Debt moderation (joint test)
  h7 <- linearHypothesis(
    chosen_model,
    c(paste(consumption_debt, "= 0"), paste(income_debt, "= 0")),
    vcov. = robust_vcov,
    test = "F"
  )

  list(h1 = h1, h2 = h2, h3 = h3, h4 = h4, h5 = h5, h6 = h6, h7 = h7)
}

# =============================================================================
# SECTION J: Execute and Package Results
# =============================================================================

cat("\n=== Fitting SDG3 models ===\n")
sdg3_models <- fit_all_models(sdg3_formula, panel_data)

cat("=== SDG3 model selection ===\n")
sdg3_selection <- run_selection_tests(sdg3_models)
cat("  Chosen model:", sdg3_selection$chosen_name, "\n")

cat("=== SDG3 diagnostics ===\n")
sdg3_diagnostics <- run_diagnostics(sdg3_selection$chosen_model)
cat(
  "  Serial correlation p-value:",
  round(sdg3_diagnostics$bg$p.value, 4),
  "\n"
)

cat("=== SDG3 robust inference ===\n")
sdg3_robust <- compute_robust_inference(sdg3_selection$chosen_model)

cat("=== SDG3 hypothesis tests ===\n")
sdg3_hypotheses <- run_hypothesis_tests(
  sdg3_selection$chosen_model,
  sdg3_robust$vcov
)

cat("\n=== Fitting SDG4 models ===\n")
sdg4_models <- fit_all_models(sdg4_formula, panel_data)

cat("=== SDG4 model selection ===\n")
sdg4_selection <- run_selection_tests(sdg4_models)
cat("  Chosen model:", sdg4_selection$chosen_name, "\n")

cat("=== SDG4 diagnostics ===\n")
sdg4_diagnostics <- run_diagnostics(sdg4_selection$chosen_model)
cat(
  "  Serial correlation p-value:",
  round(sdg4_diagnostics$bg$p.value, 4),
  "\n"
)

cat("=== SDG4 robust inference ===\n")
sdg4_robust <- compute_robust_inference(sdg4_selection$chosen_model)

cat("=== SDG4 hypothesis tests ===\n")
sdg4_hypotheses <- run_hypothesis_tests(
  sdg4_selection$chosen_model,
  sdg4_robust$vcov
)

# =============================================================================
# SECTION K: Pre-compute Display Tables
# =============================================================================

# ---------------------------------------------------------------------------
# Helpers for table construction
# ---------------------------------------------------------------------------

rename_terms <- function(term) {
  dplyr::case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "tax_compositionpredominant_consumption_tax" ~ "Consumption-predominant",
    term == "tax_compositionpredominant_income_tax" ~ "Income-predominant",
    term == "mod_macroeconomic_population" ~ "Population",
    term == "mod_gdp_per_capita" ~ "GDP per capita",
    term == "mod_debt_to_gdp_ratio" ~ "Debt-to-GDP ratio",
    term == "tax_compositionpredominant_consumption_tax:mod_macroeconomic_population" ~ "Consumption x Population",
    term == "tax_compositionpredominant_income_tax:mod_macroeconomic_population" ~ "Income x Population",
    term == "tax_compositionpredominant_consumption_tax:mod_gdp_per_capita" ~ "Consumption x GDP per capita",
    term == "tax_compositionpredominant_income_tax:mod_gdp_per_capita" ~ "Income x GDP per capita",
    term == "tax_compositionpredominant_consumption_tax:mod_debt_to_gdp_ratio" ~ "Consumption x Debt-to-GDP",
    term == "tax_compositionpredominant_income_tax:mod_debt_to_gdp_ratio" ~ "Income x Debt-to-GDP",
    TRUE ~ term
  )
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE       ~ ""
  )
}

# Tidy a model into a tibble using coeftest (works for lm and plm)
tidy_model <- function(model, model_label) {
  ct <- lmtest::coeftest(model)
  tibble(
    term = rownames(ct),
    estimate = ct[, 1],
    std.error = ct[, 2],
    statistic = ct[, 3],
    p.value = ct[, 4],
    model = model_label
  )
}

# Build 3-model comparison table
build_regression_table <- function(models) {
  ols_tidy <- tidy_model(models$ols, "Pooled OLS")
  fe_tidy <- tidy_model(models$fe, "Fixed Effects")
  re_tidy <- tidy_model(models$re, "Random Effects")

  bind_rows(ols_tidy, fe_tidy, re_tidy) |>
    filter(term != "(Intercept)") |>
    mutate(
      term = rename_terms(term),
      estimate_sig = paste0(round(estimate, 3), sig_stars(p.value))
    ) |>
    select(term, model, estimate_sig) |>
    tidyr::pivot_wider(names_from = model, values_from = estimate_sig) |>
    rename(Variable = term)
}

# Build robust SE table from coeftest result
build_robust_table <- function(robust_coeftest) {
  ct <- robust_coeftest
  p_raw <- ct[, 4]
  tibble(
    Variable = rename_terms(rownames(ct)),
    Estimate = round(ct[, 1], 3),
    `Robust SE` = round(ct[, 2], 3),
    `t-value` = round(ct[, 3], 3),
    `p-value` = format.pval(p_raw, digits = 3),
    Sig = sig_stars(p_raw)
  ) |>
    filter(Variable != "Intercept")
}

# Build model selection table
build_selection_table <- function(selection) {
  tibble(
    Test = c("Breusch-Pagan LM", "Chow/F-test", "Hausman"),
    Comparison = c("Pooled vs RE", "Pooled vs FE", "FE vs RE"),
    Statistic = c(
      round(selection$bplm$statistic, 3),
      round(selection$chow$statistic, 3),
      round(selection$hausman$statistic, 3)
    ),
    `p-value` = c(
      format.pval(selection$bplm$p.value, digits = 3),
      format.pval(selection$chow$p.value, digits = 3),
      format.pval(selection$hausman$p.value, digits = 3)
    ),
    Decision = c(
      ifelse(selection$bplm$p.value < 0.05,
             "RE preferred over Pooled", "Pooled adequate"),
      ifelse(selection$chow$p.value < 0.05,
             "Significant country effects", "No country effects"),
      ifelse(selection$hausman$p.value < 0.05,
             "Use Fixed Effects", "Use Random Effects")
    )
  )
}

# Build diagnostics table
build_diagnostics_table <- function(diagnostics) {
  bg <- diagnostics$bg
  cd <- diagnostics$cd
  tibble(
    Test = c("Breusch-Godfrey (Serial Correlation)",
             "Pesaran CD (Cross-sectional Dependence)"),
    Statistic = c(
      round(bg$statistic, 3),
      ifelse(is.na(cd$statistic), "N/A", round(cd$statistic, 3))
    ),
    `p-value` = c(
      format.pval(bg$p.value, digits = 3),
      ifelse(is.na(cd$p.value), "N/A", format.pval(cd$p.value, digits = 3))
    ),
    Result = c(
      ifelse(bg$p.value < 0.05, "Serial correlation detected",
             "No serial correlation"),
      ifelse(is.na(cd$p.value), "Insufficient panel overlap",
             ifelse(cd$p.value < 0.05, "Cross-sectional dependence detected",
                    "No cross-sectional dependence"))
    )
  )
}

# Build VIF table
build_vif_table <- function(vif_result) {
  if (is.null(vif_result)) return(NULL)
  tibble(
    Variable = rename_terms(rownames(vif_result)),
    GVIF = round(vif_result[, "GVIF"], 2),
    Df = vif_result[, "Df"],
    `Adjusted GVIF` = round(vif_result[, "GVIF^(1/(2*Df))"], 2)
  )
}

# Extract F-stat and p-value from linearHypothesis result
extract_hyp <- function(lh) {
  row <- lh[2, ]
  f_col <- intersect(c("F", "Chisq"), names(row))
  p_col <- intersect(c("Pr(>F)", "Pr(>Chisq)"), names(row))
  list(
    f_stat = as.numeric(row[[f_col[1]]]),
    p_value = as.numeric(row[[p_col[1]]])
  )
}

# Build hypothesis table
build_hypothesis_table <- function(hypotheses, h_offset = 0) {
  labels <- paste0("H", h_offset + 1:7)
  statements <- c(
    "Tax structure composition has a significant relationship with outcomes",
    "Income-predominant structures differ from consumption-predominant structures",
    "Income-predominant structures differ from balanced structures",
    "Balanced structures differ from consumption-predominant structures",
    "Population significantly moderates the tax structure-outcome relationship",
    "GDP per capita significantly moderates the tax structure-outcome relationship",
    "Debt-to-GDP ratio significantly moderates the tax structure-outcome relationship"
  )
  tests <- c(
    "Joint test on tax composition dummies",
    "Linear contrast: Income vs Consumption",
    "Coefficient test: Income-predominant",
    "Coefficient test: Consumption-predominant",
    "Joint test on population interaction terms",
    "Joint test on GDP per capita interaction terms",
    "Joint test on debt-to-GDP interaction terms"
  )

  hyp_results <- lapply(hypotheses, extract_hyp)

  tibble(
    Hypothesis = labels,
    Statement = statements,
    Test = tests,
    `F-statistic` = sapply(hyp_results, \(x) round(x$f_stat, 3)),
    `p-value` = sapply(hyp_results, \(x) format.pval(x$p_value, digits = 3)),
    Conclusion = sapply(hyp_results, \(x)
      ifelse(x$p_value < 0.05, "Reject H0", "Fail to reject H0")
    )
  )
}

# ---------------------------------------------------------------------------
# Build all tables
# ---------------------------------------------------------------------------

cat("\n=== Building display tables ===\n")

sdg3_tables <- list(
  selection = build_selection_table(sdg3_selection),
  regression = build_regression_table(sdg3_models),
  robust = build_robust_table(sdg3_robust$coeftest),
  diagnostics = build_diagnostics_table(sdg3_diagnostics),
  vif = build_vif_table(sdg3_diagnostics$vif),
  hypotheses = build_hypothesis_table(sdg3_hypotheses, h_offset = 0)
)

sdg4_tables <- list(
  selection = build_selection_table(sdg4_selection),
  regression = build_regression_table(sdg4_models),
  robust = build_robust_table(sdg4_robust$coeftest),
  diagnostics = build_diagnostics_table(sdg4_diagnostics),
  vif = build_vif_table(sdg4_diagnostics$vif),
  hypotheses = build_hypothesis_table(sdg4_hypotheses, h_offset = 7)
)

# ---------------------------------------------------------------------------
# Scalar values needed for conditional prose in the QMD
# ---------------------------------------------------------------------------

sdg3_scalars <- list(
  chosen_name = sdg3_selection$chosen_name,
  chosen_label = ifelse(sdg3_selection$chosen_name == "fe",
                        "Fixed Effects", "Random Effects"),
  hausman_p = sdg3_selection$hausman$p.value,
  bplm_p = sdg3_selection$bplm$p.value,
  chow_p = sdg3_selection$chow$p.value,
  bg_p = sdg3_diagnostics$bg$p.value,
  n_sig = sum(sdg3_tables$hypotheses$Conclusion == "Reject H0"),
  sig_labels = sdg3_tables$hypotheses |>
    filter(Conclusion == "Reject H0") |> pull(Hypothesis)
)

sdg4_scalars <- list(
  chosen_name = sdg4_selection$chosen_name,
  chosen_label = ifelse(sdg4_selection$chosen_name == "fe",
                        "Fixed Effects", "Random Effects"),
  hausman_p = sdg4_selection$hausman$p.value,
  bplm_p = sdg4_selection$bplm$p.value,
  chow_p = sdg4_selection$chow$p.value,
  bg_p = sdg4_diagnostics$bg$p.value,
  n_sig = sum(sdg4_tables$hypotheses$Conclusion == "Reject H0"),
  sig_labels = sdg4_tables$hypotheses |>
    filter(Conclusion == "Reject H0") |> pull(Hypothesis)
)

# PCA summaries (plain tibbles, no prcomp objects needed)
sdg3_pca_summary <- list(
  importance = summary(sdg3_pca)$importance,
  loadings_pc1 = sdg3_pca$rotation[, 1],
  pc1_var_pct = summary(sdg3_pca)$importance["Proportion of Variance", 1]
)

sdg4_pca_summary <- list(
  importance = summary(sdg4_pca)$importance,
  loadings_pc1 = sdg4_pca$rotation[, 1],
  pc1_var_pct = summary(sdg4_pca)$importance["Proportion of Variance", 1]
)

# Country-level info
n_changers <- panel_data |>
  group_by(country) |>
  summarise(n_cat = n_distinct(tax_composition)) |>
  filter(n_cat > 1) |>
  nrow()

# =============================================================================
# SECTION L: Package and Save
# =============================================================================

all_results <- list(
  panel_data = panel_data,
  sdg3_vars = sdg3_vars,
  sdg4_vars = sdg4_vars,
  sdg3_pca_summary = sdg3_pca_summary,
  sdg4_pca_summary = sdg4_pca_summary,
  n_changers = n_changers,
  sdg3 = list(
    tables = sdg3_tables,
    scalars = sdg3_scalars
  ),
  sdg4 = list(
    tables = sdg4_tables,
    scalars = sdg4_scalars
  )
)

saveRDS(all_results, "outputs/analysis_results.rds")
cat("\nResults saved to outputs/analysis_results.rds\n")
