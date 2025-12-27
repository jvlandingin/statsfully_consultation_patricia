#!/usr/bin/env Rscript
# Marginal Missingness Analysis
# Shows incremental cost of adding each variable to the model

# Load data
data <- readRDS('_targets/objects/tax_structure_and_sdg')

# Define variable groups
baseline_vars <- c(
  'tax_goods_and_services',
  'tax_general_consumption',
  'tax_income_and_profits',
  'mod_gdp_per_capita',
  'mod_macroeconomic_population',
  'mod_debt_to_gpt_ratio'
)

sdg3_vars <- grep('^sdg3_', names(data), value = TRUE)
sdg4_vars <- grep('^sdg4_', names(data), value = TRUE)

# Calculate baseline complete cases
baseline_complete <- complete.cases(data[, baseline_vars])
baseline_n <- sum(baseline_complete)

cat("="*80, "\n")
cat("MARGINAL MISSINGNESS ANALYSIS\n")
cat("="*80, "\n\n")

cat("Total observations:", nrow(data), "\n")
cat("Observations with complete baseline (tax + moderators):", baseline_n, "\n")
cat("Baseline variables:", paste(baseline_vars, collapse = ", "), "\n\n")

# Function to calculate marginal loss
calc_marginal <- function(var_name) {
  # How many baseline observations also have this variable?
  both_complete <- baseline_complete & !is.na(data[[var_name]])
  n_both <- sum(both_complete)

  # Marginal loss
  marginal_loss <- baseline_n - n_both
  pct_loss_from_baseline <- round(marginal_loss / baseline_n * 100, 1)

  # Overall missingness
  pct_missing_overall <- round(sum(is.na(data[[var_name]])) / nrow(data) * 100, 1)

  data.frame(
    variable = var_name,
    pct_missing_overall = pct_missing_overall,
    available_with_baseline = n_both,
    marginal_loss = marginal_loss,
    pct_loss_from_baseline = pct_loss_from_baseline
  )
}

# Calculate for all SDG variables
results <- bind_rows(
  lapply(c(sdg3_vars, sdg4_vars), calc_marginal)
)

# Sort by marginal loss (best to worst)
results <- results %>% arrange(marginal_loss)

# Save results
write.csv(results, 'marginal_missingness.csv', row.names = FALSE)

# Print formatted results
cat("\n", "="*80, "\n")
cat("RESULTS: Variables sorted by MARGINAL LOSS (best to worst)\n")
cat("="*80, "\n\n")

cat("Column Definitions:\n")
cat("  - pct_missing_overall: % missing across all 290 observations\n")
cat("  - available_with_baseline: # obs with BOTH baseline AND this variable\n")
cat("  - marginal_loss: # baseline obs LOST by adding this variable\n")
cat("  - pct_loss_from_baseline: % of baseline obs lost\n\n")

print(results, row.names = FALSE)

# Summary by category
cat("\n\n", "="*80, "\n")
cat("SUMMARY BY SDG CATEGORY\n")
cat("="*80, "\n\n")

cat("SDG 3 Health Variables:\n")
sdg3_results <- results %>% filter(grepl('^sdg3_', variable))
print(sdg3_results, row.names = FALSE)

cat("\n\nSDG 4 Education Variables:\n")
sdg4_results <- results %>% filter(grepl('^sdg4_', variable))
print(sdg4_results, row.names = FALSE)

# Cumulative analysis - what if we add variables in optimal order?
cat("\n\n", "="*80, "\n")
cat("CUMULATIVE ANALYSIS: Adding variables in optimal order\n")
cat("="*80, "\n\n")

cumulative_best <- data.frame(
  n_vars = integer(),
  variables_added = character(),
  complete_cases = integer()
)

current_vars <- baseline_vars
for (i in 1:min(5, nrow(results))) {
  next_var <- results$variable[i]
  current_vars <- c(current_vars, next_var)

  n_complete <- sum(complete.cases(data[, current_vars]))

  cumulative_best <- rbind(cumulative_best, data.frame(
    n_vars = i,
    variable_added = next_var,
    complete_cases = n_complete,
    pct_remaining = round(n_complete / baseline_n * 100, 1)
  ))
}

cat("If we add the BEST variables sequentially:\n\n")
print(cumulative_best, row.names = FALSE)

cat("\n\nInterpretation:\n")
cat("- Start with", baseline_n, "baseline observations\n")
cat("- Best 5 variables minimize row loss\n")
cat("- This shows the sample size you'd have for different model specifications\n")