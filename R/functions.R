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

#' Create SDG3 health index using PCA on mortality indicators
#'
#' @param tax_structure_and_sdg Full panel dataset
#' @return Tibble with country, year, and sdg3_index
create_sdg3_index <- function(tax_structure_and_sdg) {
  # Select mortality indicators and identifiers
  sdg3_data <- tax_structure_and_sdg |>
    dplyr::select(
      country,
      year,
      sdg3_under_5_mortality_rate,
      sdg3_neonatal_mortality_rate,
      sdg3_maternal_mortality_ratio
    ) |>
    tidyr::drop_na()

  # Extract just the mortality indicators for PCA
  mortality_vars <- sdg3_data |>
    dplyr::select(
      sdg3_under_5_mortality_rate,
      sdg3_neonatal_mortality_rate,
      sdg3_maternal_mortality_ratio
    )

  # Run PCA with standardization
  pca_result <- stats::prcomp(mortality_vars, scale. = TRUE)

  # Extract PC1 scores and combine with identifiers
  sdg3_data$sdg3_index <- pca_result$x[, 1]

  # Return only identifiers and index
  sdg3_data |>
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

#' Impute missing values in SDG4 education indicator
#'
#' @param tax_structure_and_sdg Full panel dataset
#' @param m Number of imputations (default 5)
#' @param maxit Number of iterations (default 20)
#' @param seed Random seed for reproducibility (default 123)
#' @return Tibble with country, year, and imputed sdg4_lower_secondary
impute_sdg4_education <- function(tax_structure_and_sdg,
                                    m = 5,
                                    maxit = 20,
                                    seed = 123) {
  # Select variables for imputation
  imputation_data <- tax_structure_and_sdg |>
    dplyr::select(
      country,
      year,
      sdg4_lower_secondary,
      mod_gdp_per_capita,
      tax_income_and_profits
    ) |>
    # Remove rows where predictors are completely missing
    dplyr::filter(!is.na(mod_gdp_per_capita) & !is.na(tax_income_and_profits))

  # Initialize mice
  init <- mice::mice(imputation_data, maxit = 0, printFlag = FALSE)

  # Set method to PMM for sdg4_lower_secondary
  meth <- init$method
  meth["sdg4_lower_secondary"] <- "pmm"

  # Set predictor matrix (don't use year as linear predictor)
  pred <- init$predictorMatrix
  pred[, "year"] <- 0

  # Run multiple imputation
  set.seed(seed)
  imp_result <- mice::mice(
    imputation_data,
    method = meth,
    predictorMatrix = pred,
    m = m,
    maxit = maxit,
    seed = seed,
    printFlag = FALSE
  )

  # Extract first completed dataset
  completed_data <- mice::complete(imp_result, action = 1)

  # Return only necessary columns
  completed_data |>
    dplyr::select(country, year, sdg4_lower_secondary)
}
