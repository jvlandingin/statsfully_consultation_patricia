# Quick Missingness Analysis
# Load the integrated dataset and analyze missingness patterns

library(targets)
library(dplyr)
library(tidyr)

# Load data
tar_load(tax_structure_and_sdg)

# Variable groups
tax_vars <- c("tax_goods_and_services", "tax_general_consumption", "tax_income_and_profits")
sdg3_vars <- names(tax_structure_and_sdg)[grepl("^sdg3_", names(tax_structure_and_sdg))]
sdg4_vars <- names(tax_structure_and_sdg)[grepl("^sdg4_", names(tax_structure_and_sdg))]
mod_vars <- names(tax_structure_and_sdg)[grepl("^mod_", names(tax_structure_and_sdg))]

cat("=== DATASET OVERVIEW ===\n")
cat("Dimensions:", nrow(tax_structure_and_sdg), "rows x", ncol(tax_structure_and_sdg), "columns\n")
cat("Countries:", n_distinct(tax_structure_and_sdg$country), "\n")
cat("Years:", min(tax_structure_and_sdg$year), "-", max(tax_structure_and_sdg$year), "\n\n")

cat("=== VARIABLE COUNTS BY GROUP ===\n")
cat("Tax variables:", length(tax_vars), "\n")
cat("SDG 3 Health:", length(sdg3_vars), "\n")
cat("SDG 4 Education:", length(sdg4_vars), "\n")
cat("Moderating:", length(mod_vars), "\n\n")

cat("=== SDG 4 VARIABLES (NEW) ===\n")
cat(paste(sdg4_vars, collapse = "\n"), "\n\n")

# Overall missingness summary
cat("=== OVERALL MISSINGNESS BY VARIABLE ===\n")
overall_miss <- tax_structure_and_sdg |>
  summarise(
    across(
      -c(country, year),
      list(
        pct_missing = ~ round(mean(is.na(.)) * 100, 1)
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "pct_missing"
  ) |>
  mutate(variable = gsub("_pct_missing$", "", variable)) |>
  arrange(desc(pct_missing)) |>
  mutate(
    group = case_when(
      grepl("^tax_", variable) ~ "Tax",
      grepl("^sdg3_", variable) ~ "SDG3",
      grepl("^sdg4_", variable) ~ "SDG4",
      grepl("^mod_", variable) ~ "Mod",
      TRUE ~ "Other"
    ),
    severity = case_when(
      pct_missing > 50 ~ "CRITICAL",
      pct_missing > 30 ~ "HIGH",
      pct_missing > 15 ~ "MODERATE",
      TRUE ~ "LOW"
    )
  )

print(overall_miss, n = 50)

cat("\n=== MISSINGNESS SUMMARY BY GROUP ===\n")
overall_miss |>
  group_by(group) |>
  summarise(
    n_vars = n(),
    min_miss = min(pct_missing),
    median_miss = median(pct_missing),
    max_miss = max(pct_missing),
    n_critical = sum(pct_missing > 50),
    n_high = sum(pct_missing > 30 & pct_missing <= 50),
    n_moderate = sum(pct_missing > 15 & pct_missing <= 30),
    n_low = sum(pct_missing <= 15)
  ) |>
  print()

cat("\n=== SDG4 MISSINGNESS DETAIL ===\n")
overall_miss |>
  filter(group == "SDG4") |>
  select(variable, pct_missing, severity) |>
  print(n = 20)

cat("\n=== USABLE VARIABLES (< 30% missing) ===\n")
usable <- overall_miss |>
  filter(pct_missing < 30) |>
  arrange(pct_missing)
cat("Count:", nrow(usable), "\n")
print(usable, n = 30)

cat("\n=== PROBLEMATIC VARIABLES (>= 50% missing) ===\n")
problematic <- overall_miss |>
  filter(pct_missing >= 50)
cat("Count:", nrow(problematic), "\n")
print(problematic, n = 30)