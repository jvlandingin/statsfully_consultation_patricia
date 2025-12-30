box::use(
  dplyr[...],
  naniar[...],
  visdat[vis_miss],
  tibble[tibble]
)

# Reconstruct the raw data (first few countries for testing)
tax_structure_and_sdg <- tibble(
  country = rep(
    c(
      "Armenia", "Australia", "Azerbaijan", "Bangladesh", "Bhutan", "Cambodia",
      "China", "Cook Island", "Fiji", "Georgia", "Hong Kong", "Indonesia", "Japan",
      "Kazakhstan", "Malaysia", "Kyrgyzstan", "Laos", "Maldives", "Mongolia",
      "New Zealand", "Pakistan", "Papua New Guinea", "Philippines", "Samoa",
      "Singapore", "Solomon Island", "South Korea", "Thailand", "Vietnam"
    ),
    each = 10L
  ),
  year = rep(seq(2014, 2023, by = 1), 29)
)

# Add key moderator and outcome variables to check missingness patterns
# We'll focus on the critical variables for your analysis

cat("=== MISSINGNESS ANALYSIS FOR RAW DATA ===\n\n")

cat("Total observations (29 countries × 10 years):", nrow(tax_structure_and_sdg), "\n")
cat("Expected if balanced panel: 290\n\n")

# Count countries
cat("Number of unique countries:", n_distinct(tax_structure_and_sdg$country), "\n")
cat("Year range:", min(tax_structure_and_sdg$year), "-", max(tax_structure_and_sdg$year), "\n\n")

cat("This script is a placeholder to analyze the full raw dataset.\n")
cat("Next step: Load the full data and run comprehensive missingness diagnostics.\n")