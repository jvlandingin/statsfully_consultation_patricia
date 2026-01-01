# Functions for Tax Structure and SDG Analysis Pipeline
# These functions are sourced by _targets.R

# List of all countries in the study
all_countries <- c(
  "Armenia",
  "Australia",

  "Azerbaijan",

  "Bangladesh",
  "Bhutan",
  "Cambodia",
  "China",
  "Cook Island",
  "Fiji",
  "Georgia",
  "Hong Kong",
  "Indonesia",
  "Japan",
  "Kazakhstan",
  "Kyrgyzstan",
  "Laos",
  "Malaysia",
  "Maldives",
  "Mongolia",
  "New Zealand",
  "Pakistan",
  "Papua New Guinea",
  "Philippines",
  "Samoa",
  "Singapore",
  "Solomon Island",
  "South Korea",
  "Thailand",
  "Vietnam"
)

#' Tidy googlesheet extract
#'
#' @description
#' Prepare raw googlesheets table extract to tidy format
#'
#' @param googlesheet_extract A data frame from googlesheets4::read_sheet()
#' @param field_name The name to give the value column
#' @return A tidy data frame with country, year, and the field value
tidy_googlesheet_extract <- function(googlesheet_extract, field_name) {
  # last_row <-
  #   googlesheet_extract |>
  #   dplyr::slice_tail(n = 1) |>
  #   dplyr::select(-`List of Countries (Asia-Pacific Region)`)

  year_row <-
    googlesheet_extract |>
    filter(`List of Countries (Asia-Pacific Region)` == "Years Covered") |>
    select(-`List of Countries (Asia-Pacific Region)`)

  initial <-
    googlesheet_extract |>
    dplyr::filter(`List of Countries (Asia-Pacific Region)` != "Years Covered")

  colnames(initial) <- c("country", as.character(year_row))

  tidy_format <-
    initial |>
    tidyr::pivot_longer(
      cols = -country,
      names_to = "year",
      values_to = field_name
    ) |>
    dplyr::mutate(year = as.numeric(year))

  # Correction for country name inconsistencies
  tidy_format <-
    tidy_format |>
    dplyr::mutate(
      country = dplyr::case_when(
        country == "Krygyzstan" ~ "Kyrgyzstan",
        country == "Solomon Islands" ~ "Solomon Island",
        .default = country
      )
    )

  # Data validation
  tidy_format |>
    pointblank::col_is_numeric(
      columns = c("year", dplyr::all_of(field_name))
    ) |>
    pointblank::col_is_character(columns = "country") |>
    pointblank::col_vals_not_null(columns = c("country", "year")) |>
    pointblank::col_vals_not_null(
      columns = dplyr::all_of(field_name),
      actions = pointblank::action_levels(warn_at = 0.80)
    ) |>
    pointblank::col_vals_make_set(
      columns = "country",
      set = all_countries,
      actions = pointblank::warn_on_fail(warn_at = 1)
    ) |>
    pointblank::col_vals_make_set(
      columns = "year",
      set = seq(2014, 2023),
      actions = pointblank::warn_on_fail(warn_at = 1)
    )

  return(tidy_format)
}

#' Extract data from Google Sheets
#'
#' @param url The Google Sheets URL
#' @param range The cell range to extract
#' @return Raw data frame from Google Sheets
extract_googlesheet_data <- function(url, range) {
  googlesheets4::read_sheet(
    ss = url,
    sheet = "DATA SHEET",
    range = range
  )
}

#' Full join with one-to-one relationship enforcement
#'
#' @param x First data frame
#' @param y Second data frame
#' @return Joined data frame
full_join_one_to_one_no_na <- function(x, y) {
  dplyr::full_join(
    x = x,
    y = y,
    by = dplyr::join_by(country, year),
    relationship = "one-to-one",
    na_matches = "never"
  )
}

#' Integrate all tidy datasets into one panel
#'
#' @param ... Tidy data frames to join (arbitrary number)
#' @return Integrated panel data frame
integrate_panel_data <- function(...) {
  datasets <- list(...)

  # Use Reduce to sequentially join all datasets
  Reduce(
    f = full_join_one_to_one_no_na,
    x = datasets
  )
}

#' Calculate missingness per year
#'
#' @param data The integrated panel data
#' @return Data frame with missingness proportions by year
calculate_missingness <- function(data) {
  data |>
    dplyr::summarize(
      .by = year,
      dplyr::across(
        .cols = !c(country),
        .fns = ~ sum(is.na(.)) / dplyr::n()
      )
    )
}

#' Fit PCA on imputed SDG3 health data
#'
#' Performs standardized PCA on 3 SDG3 mortality indicators from imputed dataset.
#'
#' @param sdg3_imputed_data Complete dataset from extract_sdg3_imputed_data()
#'
#' @return List with two components:
#'   - `pca_model`: prcomp object with PCA results
#'   - `data`: Data with country, year, and 3 SDG3 mortality indicators
#'
#' @details
#' ## Input Requirements
#'
#' Expects COMPLETE data (no missing values) from panel imputation. This function
#' no longer performs listwise deletion (`drop_na()`). The input must be the
#' output from `extract_sdg3_imputed_data()` which provides complete cases.
#'
#' ## PCA Specification
#'
#' Performs standardized PCA (`scale. = TRUE`) on 3 mortality indicators:
#' - `sdg3_under_5_mortality_rate`
#' - `sdg3_neonatal_mortality_rate`
#' - `sdg3_maternal_mortality_ratio`
#'
#' Standardization ensures each variable contributes equally to PC1 based on
#' correlations, not raw variances (variables have different scales).
#'
#' ## Previous Implementation
#'
#' Old approach used `drop_na()` which:
#' - Lost 16% of data (47/290 observations)
#' - Reduced to 243 complete cases
#' - Biased if missingness not completely at random
#'
#' New approach uses multilevel panel imputation which:
#' - Retains all 290 observations
#' - Respects country clustering
#' - More efficient and less biased
#'
#' @export
fit_sdg3_pca <- function(sdg3_imputed_data) {
  # Select SDG3 variables for PCA (NO drop_na - data already complete)
  sdg3_for_pca <- sdg3_imputed_data |>
    dplyr::select(
      country,
      year,
      sdg3_under_5_mortality_rate,
      sdg3_neonatal_mortality_rate,
      sdg3_maternal_mortality_ratio
    )

  # PCA with standardization
  pca_model <- stats::prcomp(
    sdg3_for_pca |>
      dplyr::select(-country, -year),
    scale. = TRUE
  )

  # Return model + data for index extraction
  list(
    pca_model = pca_model,
    data = sdg3_for_pca
  )
}

#' Extract SDG3 index from fitted PCA model
#'
#' @param pca_fit List from fit_sdg3_pca() containing pca_model and data
#' @return Tibble with country, year, and sdg3_index
extract_sdg3_index <- function(pca_fit) {
  # Extract PC1 scores
  sdg3_index_values <- pca_fit$pca_model$x[, 1]

  # Combine with identifiers
  pca_fit$data |>
    dplyr::mutate(sdg3_index = sdg3_index_values) |>
    dplyr::select(country, year, sdg3_index)
}

#' Validate PCA suitability using statistical tests
#'
#' @param data Matrix or data frame with numeric columns for PCA
#' @return List with correlation matrix, KMO result, Bartlett result, and variance explained
validate_pca_suitability <- function(data) {
  # Calculate correlation matrix
  cor_matrix <- stats::cor(data, use = "complete.obs")

  # Run KMO test (requires psych package)
  kmo_result <- psych::KMO(data)

  # Run Bartlett's test
  bartlett_result <- psych::cortest.bartlett(
    cor_matrix,
    n = nrow(tidyr::drop_na(data))
  )

  # Run PCA to get variance explained
  pca_result <- stats::prcomp(data, scale. = TRUE)
  variance_explained <- summary(pca_result)$importance[2, 1]

  # Return all validation metrics
  list(
    cor_matrix = cor_matrix,
    kmo_result = kmo_result,
    bartlett_result = bartlett_result,
    variance_explained = variance_explained
  )
}

#' Fit Multilevel Panel Imputation Model for SDG4 Education Indicators
#'
#' @description
#' Performs multilevel imputation on 4 highly correlated SDG4 education variables
#' using the 2l.pan method from the mice package. This approach respects the nested
#' panel structure (observations within countries) and leverages strong correlations
#' (r = 0.74-0.93) among education indicators.
#'
#' @details
#' ## Methodological Rationale
#'
#' **Variable Selection (4 SDG4 variables):**
#' Based on correlation analysis (see R/analyze_sdg4_correlations.R), only 4 SDG4
#' variables were selected for imputation:
#' - sdg4_lower_secondary (33% missing, r = 0.926 with primary_secondary)
#' - sdg4_prop_primary_secondary (r = 0.926 with lower_secondary)
#' - sdg4_adult_literacy_rate (r = 0.776 with lower_secondary)
#' - sdg4_upper_secondary (r = 0.744 with lower_secondary)
#'
#' Other SDG4 variables (e.g., teacher training indicators) were excluded due to:
#' 1. Weak correlations (|r| < 0.2) making them poor predictors
#' 2. High missingness (>50%) that would reduce sample size excessively
#' 3. Conceptual disconnect (inputs vs outcomes - see main.R lines 127-135)
#'
#' **Why NOT Include Moderators or Tax Variables:**
#' CRITICAL DECISION: Moderating variables (GDP per capita, population, debt ratio)
#' and tax variables are intentionally EXCLUDED from the imputation model to preserve
#' the relationships we intend to study in the regression analysis.
#'
#' Including moderators/tax variables in imputation would:
#' 1. Contaminate the very relationships we want to estimate (circularity)
#' 2. Build assumptions about tax→education relationships into the imputed data
#' 3. Make it impossible to distinguish between:
#'    - Real observed relationships in the data
#'    - Synthetic relationships created by the imputation model
#'
#' This is a fundamental principle: impute outcome variables using ONLY related
#' outcome variables, never using predictor variables that will later be used
#' to explain that outcome.
#'
#' **Panel Structure (2l.pan method):**
#' The 2l.pan method accounts for clustering within countries, recognizing that
#' observations from the same country are more similar than observations from
#' different countries. This prevents standard errors from being underestimated
#' and ensures imputed values respect country-specific patterns.
#'
#' @param tax_structure_and_sdg Full panel dataset (290 obs, 29 countries, 2014-2023)
#' @param m Number of multiply imputed datasets to create (default: 5)
#' @param maxit Maximum number of iterations for MICE algorithm (default: 20)
#' @param seed Random seed for reproducibility (default: 123)
#'
#' @return List containing:
#'   - imp_model: mice object with all m imputed datasets
#'   - panel_data: Original data subset with country_id added
#'   - predictor_matrix: Matrix showing which variables predict which
#'   - method: Named vector of imputation methods used
#'
#' @examples
#' \dontrun{
#' tar_load(tax_structure_and_sdg)
#' imputation_fit <- fit_sdg4_panel_imputation(tax_structure_and_sdg)
#'
#' # Check convergence
#' plot(imputation_fit$imp_model)
#'
#' # View predictor matrix
#' print(imputation_fit$predictor_matrix)
#' }
#'
#' @references
#' - Correlation analysis: R/analyze_sdg4_correlations.R
#' - Reference implementation: main.R lines 343-407
#' - Variable selection rationale: main.R lines 116-156
#'
#' @seealso
#' \code{\link{extract_sdg4_imputed_data}} for extracting completed datasets
#' \code{\link{generate_sdg4_imputation_diagnostics}} for diagnostic plots
#'
#' @export
fit_sdg4_panel_imputation <- function(
  tax_structure_and_sdg,
  m = 5,
  maxit = 20,
  seed = 123
) {
  # Prepare panel data with only SDG4 variables (NO moderators/tax)
  panel_data <- tax_structure_and_sdg |>
    dplyr::select(
      country,
      year,
      sdg4_lower_secondary,
      sdg4_prop_primary_secondary,
      sdg4_adult_literacy_rate,
      sdg4_upper_secondary
    ) |>
    dplyr::mutate(
      # Create numeric country ID for 2l.pan (cluster identifier)
      country_id = as.numeric(as.factor(country)),
      .after = country
    )

  # Initialize mice
  init_panel <- mice::mice(panel_data, maxit = 0, printFlag = FALSE)

  # Set imputation methods - use 2l.pan for all SDG4 variables
  # 2l.pan = two-level passive imputation accounting for country clustering
  meth_panel <- init_panel$method
  meth_panel["sdg4_lower_secondary"] <- "2l.pan"
  meth_panel["sdg4_prop_primary_secondary"] <- "2l.pan"
  meth_panel["sdg4_adult_literacy_rate"] <- "2l.pan"
  meth_panel["sdg4_upper_secondary"] <- "2l.pan"

  # Set predictor matrix
  # Type codes: 0 = don't use, 1 = fixed effect, -2 = class variable (cluster ID)
  pred_panel <- init_panel$predictorMatrix
  pred_panel[, "country"] <- 0 # Don't use country name (character)
  pred_panel[, "country_id"] <- -2 # Use as cluster identifier (MUST be numeric)
  pred_panel[, "year"] <- 1 # Use as fixed effect predictor

  # Run panel imputation
  set.seed(seed)
  imp_model <- mice::mice(
    panel_data,
    method = meth_panel,
    predictorMatrix = pred_panel,
    m = m,
    maxit = maxit,
    seed = seed,
    printFlag = FALSE
  )

  # Return complete imputation object and metadata
  list(
    imp_model = imp_model,
    panel_data = panel_data,
    predictor_matrix = pred_panel,
    method = meth_panel
  )
}

#' Extract Completed Datasets from SDG4 Imputation Model
#'
#' @description
#' Extracts imputed datasets from a fitted mice object in various formats to
#' support different analysis workflows: single imputation for PCA, all
#' imputations for sensitivity analysis, or long format for pooled inference.
#'
#' @details
#' ## Why Multiple Formats?
#'
#' **Single Imputation (action = 1):**
#' Used for the main analysis pipeline (PCA → regression). While multiple
#' imputation theory recommends pooling across all m datasets, we use the
#' first imputation for PCA to create a single composite index. This is
#' acceptable because:
#' 1. PCA is a data reduction technique, not inference
#' 2. Sensitivity analysis (using all 5) can validate robustness
#' 3. Simplifies interpretation and reporting
#'
#' **All Imputations (action = "all"):**
#' Preserves all m imputed datasets for:
#' 1. Sensitivity analysis: Does the SDG4 index vary substantially across imputations?
#' 2. Diagnostic checks: Are imputed values consistent?
#' 3. Methodological transparency in thesis defense
#'
#' **Long Format (action = "long"):**
#' Creates a stacked dataset with .imp and .id columns for advanced users who
#' want to perform pooled regression analysis using Rubin's rules (beyond scope
#' of current thesis, but preserves option for future work).
#'
#' @param imputation_fit List from fit_sdg4_panel_imputation() containing imp_model
#' @param action Extraction format:
#'   - Integer 1-m: Extract specific imputation number (default: 1)
#'   - "all": Return list of all m imputations
#'   - "long": Return stacked long format with .imp and .id columns
#'
#' @return Depends on action parameter:
#'   - action = 1: Tibble with country, year, and 4 imputed SDG4 variables
#'   - action = "all": List of m tibbles
#'   - action = "long": Tibble with .imp, .id, country, year, and 4 SDG4 variables
#'
#' @examples
#' \dontrun{
#' tar_load(sdg4_imputation_fit)
#'
#' # Extract first imputation for PCA
#' primary <- extract_sdg4_imputed_data(sdg4_imputation_fit, action = 1)
#'
#' # Extract all 5 for sensitivity analysis
#' all_imps <- extract_sdg4_imputed_data(sdg4_imputation_fit, action = "all")
#'
#' # Compare indices across imputations
#' indices <- lapply(all_imps, function(d) {
#'   pca <- prcomp(d[, 3:6], scale = TRUE)
#'   pca$x[, 1]
#' })
#' cor(do.call(cbind, indices))  # Should be very high (>0.95)
#' }
#'
#' @seealso
#' \code{\link{fit_sdg4_panel_imputation}} for creating the imputation model
#'
#' @export
extract_sdg4_imputed_data <- function(imputation_fit, action = 1) {
  if (action == "all") {
    # Return list of all imputations
    lapply(1:imputation_fit$imp_model$m, function(i) {
      mice::complete(imputation_fit$imp_model, action = i) |>
        dplyr::select(
          country,
          year,
          sdg4_lower_secondary,
          sdg4_prop_primary_secondary,
          sdg4_adult_literacy_rate,
          sdg4_upper_secondary
        )
    })
  } else if (action == "long") {
    # Return long format for pooled analysis
    mice::complete(imputation_fit$imp_model, action = "long") |>
      dplyr::select(
        .imp,
        .id,
        country,
        year,
        sdg4_lower_secondary,
        sdg4_prop_primary_secondary,
        sdg4_adult_literacy_rate,
        sdg4_upper_secondary
      )
  } else {
    # Return specific imputation
    mice::complete(imputation_fit$imp_model, action = action) |>
      dplyr::select(
        country,
        year,
        sdg4_lower_secondary,
        sdg4_prop_primary_secondary,
        sdg4_adult_literacy_rate,
        sdg4_upper_secondary
      )
  }
}

#' Generate Diagnostic Plots for SDG4 Imputation Quality Assessment
#'
#' @description
#' Creates convergence traces and density plots comparing observed vs imputed
#' distributions for all 4 SDG4 education variables.
#'
#' @details
#' Diagnostics help validate:
#' 1. MICE algorithm convergence (trace plots should look like "hairy caterpillars")
#' 2. Imputed values are plausible (densities should overlap with observed)
#'
#' Convergence issues appear as trends or patterns in trace plots.
#' Distribution mismatches suggest model misspecification.
#'
#' @param imputation_fit List from fit_sdg4_panel_imputation()
#'
#' @return List containing:
#'   - convergence: Trace plots for all variables
#'   - densities: List of 4 density plots (observed vs imputed for each SDG4 variable)
#'
#' @export
generate_sdg4_imputation_diagnostics <- function(imputation_fit) {
  imp_model <- imputation_fit$imp_model

  # Convergence plots (trace plots for all variables)
  convergence_plot <- plot(imp_model)

  # Density plots for each variable
  density_plots <- list(
    lower_secondary = mice::densityplot(imp_model, ~sdg4_lower_secondary),
    primary_secondary = mice::densityplot(
      imp_model,
      ~sdg4_prop_primary_secondary
    ),
    adult_literacy = mice::densityplot(imp_model, ~sdg4_adult_literacy_rate),
    upper_secondary = mice::densityplot(imp_model, ~sdg4_upper_secondary)
  )

  list(
    convergence = convergence_plot,
    densities = density_plots
  )
}

#' Validate PCA Appropriateness for Imputed SDG4 Data
#'
#' @description
#' Tests whether imputed SDG4 data meets PCA assumptions using KMO and Bartlett tests.
#'
#' @details
#' Expected results based on correlation analysis (r = 0.74-0.93):
#' - KMO > 0.7 (preferably > 0.8) indicates sampling adequacy
#' - Bartlett test p < 0.05 confirms correlations exist
#' - PC1 should explain >60% of variance
#'
#' @param imputed_data Tibble with imputed SDG4 variables (from extract_sdg4_imputed_data)
#'
#' @return List with cor_matrix, kmo, bartlett, variance_explained
#'
#' @export
validate_sdg4_pca_suitability <- function(imputed_data) {
  # Extract just the 4 SDG4 variables
  sdg4_vars <- imputed_data |>
    dplyr::select(
      sdg4_lower_secondary,
      sdg4_prop_primary_secondary,
      sdg4_adult_literacy_rate,
      sdg4_upper_secondary
    )

  # Reuse existing validation function
  validate_pca_suitability(sdg4_vars)
}

#' Fit PCA Model on Imputed SDG4 Education Indicators
#'
#' @description
#' Performs PCA on 4 imputed SDG4 variables to create composite education index.
#'
#' @details
#' Uses standardization (scale = TRUE) to give equal weight to all variables.
#' PC1 expected to explain >60% variance based on high correlations (r = 0.74-0.93).
#'
#' @param imputed_data Tibble with imputed SDG4 variables (from extract_sdg4_imputed_data)
#'
#' @return List containing:
#'   - pca_model: prcomp object with PCA results
#'   - data: Complete imputed data with identifiers
#'
#' @export
fit_sdg4_pca <- function(imputed_data) {
  # Extract just the education indicators for PCA
  education_vars <- imputed_data |>
    dplyr::select(
      sdg4_lower_secondary,
      sdg4_prop_primary_secondary,
      sdg4_adult_literacy_rate,
      sdg4_upper_secondary
    )

  # Run PCA with standardization
  pca_model <- stats::prcomp(education_vars, scale. = TRUE)

  # Return both the model and the data
  list(
    pca_model = pca_model,
    data = imputed_data
  )
}

#' Extract SDG4 Composite Index from PCA Model
#'
#' @description
#' Extracts PC1 scores as the final SDG4 education index for regression analysis.
#'
#' @details
#' PC1 direction is arbitrary - may need reversal if higher values indicate
#' worse education outcomes. Check loadings and correlations to determine direction.
#'
#' @param pca_fit List from fit_sdg4_pca() containing pca_model and data
#'
#' @return Tibble with country, year, sdg4_index (ready for join with analysis_ready_data)
#'
#' @export
extract_sdg4_index <- function(pca_fit) {
  # Extract PC1 scores
  sdg4_index_values <- pca_fit$pca_model$x[, 1]

  # Combine with identifiers
  pca_fit$data |>
    dplyr::mutate(sdg4_index = sdg4_index_values) |>
    dplyr::select(country, year, sdg4_index)
}

# ==============================================================================
# CORRELATION ANALYSIS FUNCTIONS
# ==============================================================================

#' Analyze correlations for SDG3 health variables
#'
#' Computes correlation matrix and summary statistics for SDG3 mortality
#' indicators to support variable selection for panel imputation.
#'
#' @param data Dataset containing SDG3 health variables
#'
#' @return List with four components:
#'   - `correlation_matrix`: Correlation matrix for 3 SDG3 mortality indicators
#'   - `key_correlations`: Named list of pairwise correlations
#'   - `summary`: Summary statistics (n_vars, mean/min/max correlations)
#'   - `variables`: Character vector of variable names analyzed
#'
#' @details
#' Analyzes correlations among 3 SDG3 mortality indicators:
#' - `sdg3_under_5_mortality_rate`
#' - `sdg3_neonatal_mortality_rate`
#' - `sdg3_maternal_mortality_ratio`
#'
#' These variables are used in multilevel panel imputation (2l.pan method).
#' High correlations (r > 0.7) justify joint imputation where variables
#' predict each other's missing values.
#'
#' Uses pairwise complete observations to handle missing data in correlation
#' calculation.
#'
#' @export
analyze_sdg3_correlations <- function(data) {
  sdg3_vars <- c(
    "sdg3_under_5_mortality_rate",
    "sdg3_neonatal_mortality_rate",
    "sdg3_maternal_mortality_ratio"
  )

  # Correlation matrix
  cor_matrix <- data |>
    dplyr::select(dplyr::all_of(sdg3_vars)) |>
    cor(use = "pairwise.complete.obs")

  # Key pairwise correlations
  key_cors <- list(
    under5_neonatal = cor_matrix[
      "sdg3_under_5_mortality_rate",
      "sdg3_neonatal_mortality_rate"
    ],
    under5_maternal = cor_matrix[
      "sdg3_under_5_mortality_rate",
      "sdg3_maternal_mortality_ratio"
    ],
    neonatal_maternal = cor_matrix[
      "sdg3_neonatal_mortality_rate",
      "sdg3_maternal_mortality_ratio"
    ]
  )

  # Summary statistics
  summary <- list(
    n_vars = length(sdg3_vars),
    mean_correlation = mean(cor_matrix[lower.tri(cor_matrix)]),
    min_correlation = min(cor_matrix[lower.tri(cor_matrix)]),
    max_correlation = max(cor_matrix[lower.tri(cor_matrix)])
  )

  list(
    correlation_matrix = cor_matrix,
    key_correlations = key_cors,
    summary = summary,
    variables = sdg3_vars
  )
}

#' Analyze correlations for SDG4 education variables
#'
#' Computes correlation matrix and summary statistics for SDG4 education
#' indicators to support variable selection for panel imputation.
#'
#' @param data Dataset containing SDG4 education variables
#'
#' @return List with four components:
#'   - `correlation_matrix`: Correlation matrix for 4 SDG4 education indicators
#'   - `key_correlations`: Named list of correlations with baseline (lower_secondary)
#'   - `summary`: Summary statistics (n_vars, mean/min/max correlations)
#'   - `variables`: Character vector of variable names analyzed
#'
#' @details
#' Analyzes correlations among 4 SDG4 education indicators:
#' - `sdg4_lower_secondary` (baseline for comparison)
#' - `sdg4_prop_primary_secondary`
#' - `sdg4_adult_literacy_rate`
#' - `sdg4_upper_secondary`
#'
#' These variables are used in multilevel panel imputation (2l.pan method).
#' High correlations (r > 0.7) justify joint imputation where variables
#' predict each other's missing values.
#'
#' Uses pairwise complete observations to handle missing data in correlation
#' calculation.
#'
#' @export
analyze_sdg4_correlations <- function(data) {
  sdg4_vars <- c(
    "sdg4_lower_secondary",
    "sdg4_prop_primary_secondary",
    "sdg4_adult_literacy_rate",
    "sdg4_upper_secondary"
  )

  # Correlation matrix
  cor_matrix <- data |>
    dplyr::select(dplyr::all_of(sdg4_vars)) |>
    cor(use = "pairwise.complete.obs")

  # Key correlations with baseline (lower_secondary)
  key_cors <- list(
    lower_vs_primary = cor_matrix[
      "sdg4_lower_secondary",
      "sdg4_prop_primary_secondary"
    ],
    lower_vs_literacy = cor_matrix[
      "sdg4_lower_secondary",
      "sdg4_adult_literacy_rate"
    ],
    lower_vs_upper = cor_matrix["sdg4_lower_secondary", "sdg4_upper_secondary"]
  )

  # Summary statistics
  summary <- list(
    n_vars = length(sdg4_vars),
    mean_correlation = mean(cor_matrix[lower.tri(cor_matrix)]),
    min_correlation = min(cor_matrix[lower.tri(cor_matrix)]),
    max_correlation = max(cor_matrix[lower.tri(cor_matrix)])
  )

  list(
    correlation_matrix = cor_matrix,
    key_correlations = key_cors,
    summary = summary,
    variables = sdg4_vars
  )
}

# ==============================================================================
# SDG3 HEALTH PANEL IMPUTATION FUNCTIONS
# ==============================================================================

#' Fit multilevel panel imputation for SDG3 health indicators
#'
#' Imputes missing values in 3 SDG3 mortality indicators using multilevel panel
#' imputation (2l.pan method) that respects country clustering and panel structure.
#'
#' @param tax_structure_and_sdg Full dataset containing SDG3 health variables
#' @param m Number of multiply imputed datasets to create (default: 5)
#' @param maxit Maximum iterations for MICE algorithm (default: 20)
#' @param seed Random seed for reproducibility (default: 123)
#'
#' @return List with four components:
#'   - `imp_model`: mice imputation object with m complete datasets
#'   - `panel_data`: Original panel data with country_id added
#'   - `predictor_matrix`: Matrix defining variable relationships in imputation
#'   - `method`: Named vector of imputation methods used
#'
#' @details
#' ## Variable Selection
#'
#' Imputes 3 SDG3 mortality indicators:
#' - `sdg3_under_5_mortality_rate` (15.9% missing)
#' - `sdg3_neonatal_mortality_rate` (6.9% missing)
#' - `sdg3_maternal_mortality_ratio` (6.9% missing)
#'
#' These variables are highly correlated (r = 0.74-0.97) and can reliably
#' predict each other's missing values.
#'
#' ## Excluded Variables
#'
#' **CRITICAL METHODOLOGICAL DECISION**: Moderating variables (GDP per capita,
#' population, debt-to-GDP ratio) and tax structure variables are intentionally
#' EXCLUDED from the imputation model to preserve the relationships we want to
#' study in regression analysis. Including these would contaminate the very
#' relationships we intend to estimate.
#'
#' Also excluded:
#' - `sdg3_prop_births_with_skilled_personnel` (59% missing)
#' - `sdg3_coverage_of_essential_health_services` (64% missing, biennial pattern)
#'
#' ## Imputation Method
#'
#' Uses 2l.pan (two-level panel) method which:
#' - Accounts for clustering of observations within countries (Level 2)
#' - Allows each country to have its own baseline level (random intercept)
#' - Uses year as fixed effect predictor (secular trends)
#' - Respects panel structure: observations from same country are correlated
#'
#' ## Predictor Matrix
#'
#' Type codes in predictor matrix:
#' - `0`: Don't use this variable
#' - `1`: Use as fixed effect predictor
#' - `-2`: Use as cluster identifier (class variable)
#'
#' Setup:
#' - `country` = 0 (character variable, not used)
#' - `country_id` = -2 (numeric cluster identifier for Level 2)
#' - `year` = 1 (fixed effect for temporal trends)
#' - Each SDG3 variable = 1 or 0 (mutual prediction, no self-prediction)
#'
#' @export
fit_sdg3_panel_imputation <- function(
  tax_structure_and_sdg,
  m = 5,
  maxit = 20,
  seed = 123
) {
  # Prepare panel data with only SDG3 variables (NO moderators/tax)
  panel_data <- tax_structure_and_sdg |>
    dplyr::select(
      country,
      year,
      sdg3_under_5_mortality_rate,
      sdg3_neonatal_mortality_rate,
      sdg3_maternal_mortality_ratio
    ) |>
    dplyr::mutate(
      # Create numeric country ID for 2l.pan (cluster identifier)
      country_id = as.numeric(as.factor(country)),
      .after = country
    )

  # Initialize mice
  init_panel <- mice::mice(panel_data, maxit = 0, printFlag = FALSE)

  # Set imputation methods - use 2l.pan for all 3 SDG3 variables
  meth_panel <- init_panel$method
  meth_panel["sdg3_under_5_mortality_rate"] <- "2l.pan"
  meth_panel["sdg3_neonatal_mortality_rate"] <- "2l.pan"
  meth_panel["sdg3_maternal_mortality_ratio"] <- "2l.pan"

  # Set predictor matrix
  # Type codes: 0 = don't use, 1 = fixed effect, -2 = class variable (cluster ID)
  pred_panel <- init_panel$predictorMatrix
  pred_panel[, "country"] <- 0 # Don't use country name (character)
  pred_panel[, "country_id"] <- -2 # Use as cluster identifier (MUST be numeric)
  pred_panel[, "year"] <- 1 # Use as fixed effect predictor

  # Run panel imputation
  set.seed(seed)
  imp_model <- mice::mice(
    panel_data,
    method = meth_panel,
    predictorMatrix = pred_panel,
    m = m,
    maxit = maxit,
    seed = seed,
    printFlag = FALSE
  )

  # Return complete imputation object and metadata
  list(
    imp_model = imp_model,
    panel_data = panel_data,
    predictor_matrix = pred_panel,
    method = meth_panel
  )
}

#' Extract imputed SDG3 data in various formats
#'
#' Extracts completed datasets from SDG3 panel imputation model in multiple
#' formats for different analysis purposes.
#'
#' @param sdg3_imputation_fit Output from fit_sdg3_panel_imputation()
#' @param action Extraction mode:
#'   - `1` (default): Extract first imputed dataset (for PCA)
#'   - `"all"`: Extract all m imputations as list (for sensitivity analysis)
#'   - `"long"`: Extract long format with .imp and .id columns (for pooled analysis)
#'
#' @return Imputed dataset(s):
#'   - If action = 1: Single tibble with completed data
#'   - If action = "all": List of m tibbles, one per imputation
#'   - If action = "long": Single long-format tibble with imputation indicators
#'
#' @details
#' ## Extraction Modes
#'
#' **action = 1 (Primary Dataset)**
#' - Extracts first of m imputations
#' - Used for PCA to create SDG3 index
#' - Default choice for point estimates
#'
#' **action = "all" (All Imputations)**
#' - Returns list of all m imputed datasets
#' - Used for sensitivity analysis
#' - Check robustness: Do results vary across imputations?
#' - Can compute SDG3 index for each and compare
#'
#' **action = "long" (Pooled Format)**
#' - Returns single dataset with all imputations stacked
#' - Adds `.imp` column (imputation number) and `.id` column (row ID)
#' - Used for pooled inference with Rubin's rules
#' - Compatible with mice::pool() for combining estimates
#'
#' @export
extract_sdg3_imputed_data <- function(sdg3_imputation_fit, action = 1) {
  imp_model <- sdg3_imputation_fit$imp_model

  if (action == "all") {
    # Return all m imputations as list
    lapply(1:imp_model$m, function(i) {
      mice::complete(imp_model, action = i)
    })
  } else if (action == "long") {
    # Return long format for pooled analysis
    mice::complete(imp_model, action = "long")
  } else {
    # Return single imputation (default: first)
    mice::complete(imp_model, action = action)
  }
}

#' Generate diagnostic plots for SDG3 imputation
#'
#' Creates convergence and density plots to validate SDG3 panel imputation quality.
#'
#' @param sdg3_imputation_fit Output from fit_sdg3_panel_imputation()
#'
#' @return List with four components:
#'   - `convergence_plot`: Trace plots showing mean and SD across iterations
#'   - `density_under5`: Density comparison for under-5 mortality
#'   - `density_neonatal`: Density comparison for neonatal mortality
#'   - `density_maternal`: Density comparison for maternal mortality
#'
#' @details
#' ## Convergence Plot
#'
#' Trace plots show mean and standard deviation of imputed values across
#' MICE iterations for all 3 SDG3 variables.
#'
#' **Good convergence**:
#' - Lines stabilize (no trends or oscillations) after ~5-10 iterations
#' - Multiple chains (different colors) converge to similar values
#'
#' **Bad convergence** (indicates model problems):
#' - Lines show trends (increasing/decreasing over iterations)
#' - Lines oscillate or cycle
#' - Chains diverge (different final values)
#'
#' ## Density Plots
#'
#' Compare distribution of observed values (blue) vs imputed values (red/pink).
#'
#' **Ideal result**:
#' - Imputed distribution overlaps well with observed distribution
#' - Similar shape, center, and spread
#' - Indicates imputation model produces plausible values
#'
#' **Red flag**:
#' - Imputed distribution substantially different from observed
#' - Bimodal when observed is unimodal
#' - Different mean or variance
#' - May indicate model misspecification
#'
#' @export
generate_sdg3_imputation_diagnostics <- function(sdg3_imputation_fit) {
  imp_model <- sdg3_imputation_fit$imp_model

  list(
    convergence_plot = plot(imp_model),
    density_under5 = lattice::densityplot(
      imp_model,
      ~sdg3_under_5_mortality_rate
    ),
    density_neonatal = lattice::densityplot(
      imp_model,
      ~sdg3_neonatal_mortality_rate
    ),
    density_maternal = lattice::densityplot(
      imp_model,
      ~sdg3_maternal_mortality_ratio
    )
  )
}
