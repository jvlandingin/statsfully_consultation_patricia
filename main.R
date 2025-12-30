box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  targets[...],
  constructive[...],
  psych[KMO, cortest.bartlett]
)

tar_load(tax_structure_and_sdg)

# Research solution to missingness problem -------------------------------

glimpse(tax_structure_and_sdg)

tax_structure_and_sdg |>
  select(
    contains("sdg4_")
  ) |>
  summary()

# PCA --------------------------------------------------------------------

sdg3_fields <- tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  ) |>
  na.omit()

sdg3_fields_for_pca <-
  sdg3_fields |>
  select(matches("sdg3_"))

pca_sdg3 <- prcomp(sdg3_fields_for_pca, scale. = TRUE)

summary(pca_sdg3)

sdg3_fields$sdg3_index <- pca_sdg3$x[, "PC1"]


# install.packages("openxlsx2")
# openxlsx2::write_xlsx(
#   tax_structure_and_sdg,
#   file = "data/tax_structure_and_sdg.xlsx"
# )

sdg4_fields <-
  tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg4_lower_secondary,
    sdg4_prop_trained_teachers_primary,
    sdg4_prop_trained_teachers_pre_primary
  ) |>
  na.omit()

sdg3_and_sdg4 <-
  sdg4_fields |>
  inner_join(
    sdg3_fields,
    by = join_by(country, year)
  )

pca_sdg4 <-
  sdg3_and_sdg4 |>
  select(
    contains("sdg4_")
  ) |>
  prcomp(scale. = TRUE)

summary(pca_sdg4)

sdg_indices <-
  sdg3_and_sdg4 |>
  mutate(
    sdg4_index = pca_sdg4$x[, "PC1"]
  ) |>
  select(
    country,
    year,
    sdg3_index,
    sdg4_index
  )

# Get tax and moderating variables
sdg_idx_mod_and_tax <-
  sdg_indices |>
  inner_join(
    tax_structure_and_sdg |>
      select(
        country,
        year,
        contains("mod_"),
        contains("tax_")
      ),
    by = join_by(country, year)
  )

sdg_idx_mod_and_tax |>
  vis_dat()


construct(sdg_idx_mod_and_tax)
construct(tax_structure_and_sdg)

# plan 2 -----------------------------------------------------------------

#' Decision: Use sdg4_lower_secondary as single indicator instead of PCA composite
#'
#' Rationale for NOT using PCA for SDG4:
#'
#' 1. EXCESSIVE MISSINGNESS (33-51% across available indicators)
#'    - sdg4_lower_secondary: 96/290 missing (33%)
#'    - sdg4_prop_trained_teachers_primary: 113/290 missing (39%)
#'    - sdg4_prop_trained_teachers_pre_primary: 147/290 missing (51%)
#'    - Imputing 39-51% of data for dependent variable construction creates
#'      synthetic data that may drive results rather than reflect reality
#'
#' 2. CONCEPTUAL INCOHERENCE (mixing inputs and outputs)
#'    - sdg4_lower_secondary measures STUDENT OUTCOMES (educational attainment)
#'    - Teacher training measures SYSTEM INPUTS (institutional quality)
#'    - These represent different constructs that don't necessarily correlate:
#'      * Countries can have highly trained teachers but low completion (retention issues)
#'      * Countries can have untrained teachers but high completion (cultural factors)
#'    - PCA assumes variables measure the same underlying dimension - violated here
#'
#' 3. INTERPRETATION AMBIGUITY
#'    - A composite index of completion + teacher training lacks clear meaning
#'    - Not "education quality" (no learning outcomes/test scores)
#'    - Not "education access" (only secondary level)
#'    - Not "system capacity" (missing infrastructure, spending)
#'    - Results would be difficult to interpret and defend
#'
#' 4. METHODOLOGICAL RISK
#'    - Imputing large portions of dependent variable data, then using PCA on
#'      imputed values, then regressing on that synthetic index creates
#'      compounded uncertainty that's difficult to quantify
#'    - Sensitivity to imputation method choice
#'    - Thesis committee likely to question whether results reflect data or
#'      modeling assumptions
#'
#' Solution: Use sdg4_lower_secondary alone
#' - Clear interpretation: secondary education completion rate
#' - Best available coverage (33% missing vs 39-51%)
#' - Well-established SDG indicator
#' - Can be imputed more defensibly as single variable
#' - Simplifies analysis and interpretation

#' Let's see how we can imputate sdg4_lower_secondary

# Check the missingness structure of `sdg4_lower_secondary`
tax_structure_and_sdg |>
  select(country, year, sdg4_lower_secondary) |>
  mutate(
    is_missing = if_else(is.na(sdg4_lower_secondary), "Missing", "Available")
  ) |>
  ggplot(aes(x = year, y = country, fill = is_missing)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Available" = "#56B4E9", "Missing" = "#E69F00"),
    name = "Data Status"
  ) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(
    title = "SDG4 Lower Secondary Completion: Data Availability by Country and Year",
    x = "Year",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

# next, let's look at sdg3

tax_structure_and_sdg |>
  select(
    country,
    year,
    contains("sdg3")
  ) |>
  summary()

# Correlation matrix - check if PCA is appropriate
cor_matrix <- tax_structure_and_sdg |>
  select(
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  ) |>
  cor(use = "complete.obs")

print(cor_matrix)

# Result: Very high correlations (0.74-0.97) - excellent for PCA

# Prepare data for PCA validation
sdg3_data <- tax_structure_and_sdg |>
  select(
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  ) |>
  na.omit()

cat("\nOriginal sample size:", nrow(tax_structure_and_sdg), "\n")
cat("Complete cases after removing NAs:", nrow(sdg3_data), "\n")
cat(
  "Retention rate:",
  round(100 * nrow(sdg3_data) / nrow(tax_structure_and_sdg), 1),
  "%\n"
)

# KMO Test (Kaiser-Meyer-Olkin) - measures sampling adequacy
# KMO > 0.9 = marvelous, > 0.8 = meritorious, > 0.7 = middling, > 0.6 = mediocre
kmo_result <- KMO(sdg3_data)
print(kmo_result)

# Bartlett's Test - tests if correlation matrix is significantly different from identity matrix
# p < 0.05 means correlations exist, PCA is appropriate
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(sdg3_data))
print(bartlett_result)

# Run PCA
pca_sdg3 <- prcomp(sdg3_data, scale. = TRUE)
summary(pca_sdg3)

# Check variance explained by PC1
# Should be > 60% for good composite index
cat(
  "\nPC1 explains",
  round(100 * summary(pca_sdg3)$importance[2, 1], 1),
  "% of variance\n"
)

# Visualize
screeplot(pca_sdg3, type = "lines", main = "Scree Plot: SDG3 Health Indicators")

# Check loadings - all should point in same direction
print("Factor loadings on PC1:")
print(pca_sdg3$rotation[, 1])

# Visualize missingness pattern for SDG3 mortality indicators
tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  ) |>
  mutate(
    # Check if ANY of the three variables is missing
    any_missing = is.na(sdg3_under_5_mortality_rate) |
      is.na(sdg3_neonatal_mortality_rate) |
      is.na(sdg3_maternal_mortality_ratio),
    status = if_else(any_missing, "Missing", "Complete")
  ) |>
  ggplot(aes(x = year, y = country, fill = status)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("Complete" = "#56B4E9", "Missing" = "#E69F00"),
    name = "Data Status"
  ) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(
    title = "SDG3 Health Index: Complete Cases Availability (All 3 Mortality Indicators)",
    subtitle = "Blue = all 3 indicators available, Orange = at least 1 missing",
    x = "Year",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  )

# Impute sdg4 lower secondary --------------------------------------------

library(mice)

# Option 2: Simpler approach - use regular mice but include country as factor
# This captures country-specific patterns without the complexity of 2l.pan

imputation_data_simple <- tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg4_lower_secondary,
    mod_gdp_per_capita,
    # Drop population to reduce collinearity
    tax_income_and_profits
  ) |>
  # Remove complete missingness rows to avoid singularity
  filter(!is.na(mod_gdp_per_capita) & !is.na(tax_income_and_profits))

# Initialize
init_simple <- mice(imputation_data_simple, maxit = 0)

# Set method
meth_simple <- init_simple$method
meth_simple["sdg4_lower_secondary"] <- "pmm" # PMM is robust to collinearity

# Set predictor matrix - use country as predictor
pred_simple <- init_simple$predictorMatrix
pred_simple[, "year"] <- 0 # Don't use year as linear predictor

# Run imputation
imp_simple <- mice(
  imputation_data_simple,
  method = meth_simple,
  predictorMatrix = pred_simple,
  m = 5,
  maxit = 20,
  seed = 123,
  printFlag = FALSE
)

# Check convergence
plot(imp_simple)

# Check imputed vs observed distributions
densityplot(imp_simple, ~sdg4_lower_secondary)

# Extract completed dataset
completed_data_simple <- complete(imp_simple, action = 1)

# Alternative: If that still fails, use even simpler approach with just GDP
# This is most robust but ignores some information
imputation_data_minimal <- tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg4_lower_secondary,
    mod_gdp_per_capita
  ) |>
  filter(!is.na(mod_gdp_per_capita))

# Run minimal imputation
imp_minimal <- mice(
  imputation_data_minimal,
  method = "pmm",
  m = 5,
  maxit = 20,
  seed = 123,
  printFlag = FALSE
)

# Diagnostics
plot(imp_minimal)
densityplot(imp_minimal, ~sdg4_lower_secondary)

completed_data_minimal <- complete(imp_minimal, action = 1)
