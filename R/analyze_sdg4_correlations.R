#!/usr/bin/env Rscript
# Analyze correlations with sdg4_lower_secondary to determine best predictors for imputation

library(dplyr)
library(tidyr)
library(targets)

# Load data
tar_load(tax_structure_and_sdg)

# Calculate correlations with sdg4_lower_secondary
correlations <- tax_structure_and_sdg |>
  select(-country, -year) |>  # Exclude non-numeric identifiers
  summarise(across(everything(), ~cor(.x, sdg4_lower_secondary, use = "pairwise.complete.obs"))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") |>
  filter(variable != "sdg4_lower_secondary") |>  # Remove self-correlation
  arrange(desc(abs(correlation)))

# Print results
cat(strrep("=", 80), "\n")
cat("CORRELATIONS WITH sdg4_lower_secondary\n")
cat(strrep("=", 80), "\n\n")

cat("Sorted by absolute correlation (strongest to weakest):\n\n")
print(correlations, n = Inf)

# Identify strong correlations (|r| > 0.3)
strong_correlations <- correlations |>
  filter(abs(correlation) > 0.3)

cat("\n\n", strrep("=", 80), "\n")
cat("STRONG CORRELATIONS (|r| > 0.3)\n")
cat(strrep("=", 80), "\n\n")

if (nrow(strong_correlations) > 0) {
  print(strong_correlations, n = Inf)

  cat("\n\nRecommended predictors for imputation:\n")
  cat(paste("-", strong_correlations$variable, collapse = "\n"), "\n")
} else {
  cat("No strong correlations found.\n")
}

# Categorize by variable type
cat("\n\n", strrep("=", 80), "\n")
cat("CORRELATIONS BY VARIABLE TYPE\n")
cat(strrep("=", 80), "\n\n")

cat("SDG4 Education variables:\n")
sdg4_cors <- correlations |> filter(grepl("^sdg4_", variable))
print(sdg4_cors, n = Inf)

cat("\n\nSDG3 Health variables:\n")
sdg3_cors <- correlations |> filter(grepl("^sdg3_", variable))
print(sdg3_cors, n = Inf)

cat("\n\nModerating variables:\n")
mod_cors <- correlations |> filter(grepl("^mod_", variable))
print(mod_cors, n = Inf)

cat("\n\nTax variables:\n")
tax_cors <- correlations |> filter(grepl("^tax_", variable))
print(tax_cors, n = Inf)