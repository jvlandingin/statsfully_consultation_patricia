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
#'
#' @export
tidy_googlesheet_extract <- function(googlesheet_extract, field_name) {
  # last_row <-
  #   googlesheet_extract |>
  #   dplyr::slice_tail(n = 1) |>
  #   dplyr::select(-`List of Countries (Asia-Pacific Region)`)

  year_row <-
    googlesheet_extract |>
    filter(`List of Countries (Asia-Pacific Region)` == "Years Covered") |>
    select(-`List of Countries (Asia-Pacific Region)`)

  # if no year_row extracted, create one. assume years 2014 to 2023
  if (nrow(year_row) == 0) {
    new_year_row <-
      tibble(
        year = 2014:2023
      ) |>
      t() |>
      as_tibble()
    colnames(new_year_row) <- colnames(year_row)

    year_row <-
      year_row |>
      bind_rows(
        new_year_row
      )
  }

  initial <-
    googlesheet_extract |>
    dplyr::filter(
      `List of Countries (Asia-Pacific Region)` != "Years Covered"
    )

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
      actions = pointblank::action_levels(warn_at = 0.90)
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

#' @export
filter_tax_structure_and_sdg <-
  function(tax_structure_and_sdg) {
    tax_structure_and_sdg_filtered <-
      tax_structure_and_sdg |>
      filter(
        !country %in% c("Cook Island", "Fiji")
      )
  }

#' Generate All Valid SDG Combinations
#'
#' @description
#' Generate All Valid SDG Combinations
#'
#' @details
#' Only 5 SDG3 and 5 SDG4 variables are retained.
#'
#' @export
generate_valid_sdg_combinations <-
  function(tax_structure_and_sdg_filtered) {
    # We will choose among these SDG variables which ones to keep
    all_vars <-
      colnames(tax_structure_and_sdg_filtered) |>
      stringr::str_subset(pattern = "^sdg")

    # Generate multiple samples of different variable combinations

    # A function for filtering the combinations to those that
    # meet the criteria namely: 5 sdg3 variables and 5
    # sdg4 variables
    valid_combo <- function(x) {
      sum(grepl("^sdg3_", x)) == 5 &&
        sum(grepl("^sdg4_", x)) == 5
    }

    # Generate all possible combinations
    valid_samples <-
      purrr::map(
        .x = 10, # Set number of variables to retains to test
        .f = function(n) {
          Filter(
            valid_combo,
            combn(all_vars, n, simplify = FALSE)
          )
        },
        .progress = TRUE
      ) |>
      unlist(recursive = FALSE)

    valid_samples
  }

#' Align Variable Direction
#'
#' @description
#' Ensure that the direction of variables within SDG3 and within SDG4 are
#' consistent i.e. when their values increase they all represent "better"
#' outcomes
#'
#' @details
#' Variables are multiplied by -1 if higher original values indicate worse
#' outcomes. After transformation, higher values consistently indicate
#' better outcomes for all SDG variables, which is required for meaningful
#' PCA index construction.
#'
#' Reversed variables:
#' - SDG3: mortality rates and disease death rates (higher = worse health)
#' - SDG4: out-of-school rate (higher = worse education access)
#'
#' @param tax_structure_and_sdg_filtered Data frame with SDG variables
#' @return Data frame with reversed variables (names updated, values negated)
#'
#' @export
align_variable_direction <-
  function(tax_structure_and_sdg_filtered) {
    tax_structure_and_sdg_filtered %>%
      mutate(
        across(
          .cols = c(
            # SDG3
            sdg3_under_5_mortality_rate,
            sdg3_neonatal_mortality_rate,
            sdg3_maternal_mortality_ratio,
            sdg3_death_rate_disease_30_to_70,

            # SDG4
            sdg4_out_of_school_rate
          ),
          .fns = ~ -1 * .,
          .names = "{.col}_inv"
        ),
        .keep = "unused"
      )
  }

#' Perform Interpolation
#'
#' @description
#' Estimate missing data points via interpolation
#'
#' @details
#' Variables SDG3, SDG4, Tax, and moderating variables are interpolated.
#'
#' @export
perform_interpolation <-
  function(tax_structure_and_sdg_filtered) {
    tax_structure_and_sdg_interpolated <-
      tax_structure_and_sdg_filtered |>
      split(tax_structure_and_sdg_filtered$country) |>
      map(
        .f = function(data) {
          data |>
            mutate(
              across(
                .cols = matches("sdg3_|sdg4_|tax_|mod_"),
                .fns = ~ zoo::na.approx(., na.rm = FALSE)
              )
            )
        }
      ) |>
      list_rbind()

    tax_structure_and_sdg_interpolated
  }

#' Calculate Remaining Rows per SDG Combinations
#'
#' @description
#' For each combination of SDG3 and SDG4 variables, calculate its remaining
#' rows after removing missing cases.
#'
#' @export
calculate_remaining_rows_per_sdg_combination <-
  function(tax_structure_and_sdg_interpolated, valid_combinations) {
    # Calculate number of remaining rows after removing missing cases for
    # each combination of SDG3 and SDG4
    remaining_rows_per_comb_interp <-
      map_dbl(
        .x = valid_combinations,
        .f = function(sample) {
          tax_structure_and_sdg_interpolated |>
            select(
              country,
              year,
              all_of(sample),
              contains("tax_"),
              contains("mod_")
            ) |>
            na.omit() |>
            nrow()
        },
        .progress = TRUE
      )

    # Get the
    nrow_per_sdg_combination <-
      tibble(
        combination = valid_combinations,
        nrow = remaining_rows_per_comb_interp,
        total_rows = nrow(tax_structure_and_sdg_interpolated)
      ) |>
      mutate(
        cols_retained = map_dbl(.x = combination, .f = length),
        rows_ratained_pct = nrow / total_rows
      )

    nrow_per_sdg_combination
  }

#' Calculate PCA Quality Metrics for SDG Index Construction
#'
#' @description
#' For each combination of SDG3 and SDG4 variables, calculate remaining rows
#' and metrics relevant to composite index construction via PCA. Metrics are
#' calculated separately for SDG3 and SDG4 variable groups.
#'
#' @details
#' This function evaluates whether a set of variables can be meaningfully
#' combined into a single composite index using Principal Component Analysis.
#' The metrics are tailored for index construction (dimension reduction for
#' use as a dependent variable in regression), not for factor analysis or
#' scale validation.
#'
#' Metrics are computed separately for SDG3 and SDG4 variables within each
#' combination, since these represent distinct constructs (health vs education)
#' that will form separate indices. The correlation matrices and PCA outputs
#' are stored as list columns for further inspection if needed.
#'
#' ## Metrics Used
#'
#' **Variance Explained by First Principal Component (PC1)**
#'
#' The most critical metric for index construction. If PC1 explains less than
#' 50% of total variance, the single index score poorly represents the
#' constituent variables — information is lost and the index may not capture
#' the intended construct. Higher values indicate the variables share
#' substantial common variance suitable for reduction to one score.
#'
#' - PC1 >= 70%: Excellent — variables are highly coherent
#' - PC1 >= 50%: Acceptable — majority of variance captured
#' - PC1 < 50%: Problematic — consider using fewer variables or separate indices
#'
#' **Minimum Absolute Loading on PC1**
#'
#' Each variable should contribute meaningfully to the index. The minimum
#' absolute loading indicates whether any variable loads weakly on PC1.
#' Variables with |loading| < 0.4 contribute little to the composite and
#' may be measuring a different construct.
#'
#' - min |loading| >= 0.6: All variables contribute strongly
#' - min |loading| >= 0.4: Acceptable contribution from all variables
#' - min |loading| < 0.4: At least one variable barely contributes — consider removal
#'
#' **Average Inter-Item Correlation**
#'
#' The mean absolute correlation among all variable pairs. Higher values
#' indicate variables share more variance. For index construction,
#' average correlations should typically exceed 0.3.
#'
#' **All Correlations Positive**
#'
#' A logical check verifying that all pairwise correlations are positive.
#' After applying direction alignment (e.g., via `align_variable_direction`),
#' all variables should correlate positively — higher values should
#' consistently indicate "better" outcomes. Negative correlations after
#' alignment suggest a variable was not properly reversed or does not
#' belong with the other variables in the index.
#'
#' ## Metrics Not Used (and Why)
#'
#' **Bartlett's Test of Sphericity**
#'
#' Bartlett's test checks whether the correlation matrix differs from an
#' identity matrix (i.e., whether correlations exist). While this question
#' applies to PCA, the test was developed for factor analysis (Bartlett, 1950)
#' and is redundant when already examining PC1 variance explained. If variables
#' are uncorrelated, PC1 will explain approximately 1/k of variance (where k is
#' the number of variables) — the same information Bartlett's test provides,
#' but more directly interpretable. Additionally, Bartlett's test is sensitive
#' to sample size and will almost always be significant with large samples.
#'
#' Reference: Bartlett, M. S. (1950). Tests of significance in factor analysis.
#' British Journal of Statistical Psychology, 3(2), 77-85.
#'
#' **Kaiser-Meyer-Olkin (KMO) Measure**
#'
#' KMO was developed by Kaiser (1970, 1974) specifically for factor analysis,
#' not PCA. It tests whether partial correlations are small relative to
#' zero-order correlations — a condition relevant to the common factor model's
#' assumption that latent factors cause observed correlations. Since PCA makes
#' no such latent variable assumptions and we are constructing a composite
#' index (not validating a measurement model), KMO is not theoretically
#' appropriate here. Its widespread use before PCA in software like SPSS is
#' a convention, not a requirement.
#'
#' References:
#' - Kaiser, H. F. (1970). A second generation little jiffy. Psychometrika, 35(4), 401-415.
#' - Kaiser, H. F., & Rice, J. (1974). Little Jiffy, Mark IV. Educational and
#'   Psychological Measurement, 34(1), 111-117.
#'
#' **Determinant of the Correlation Matrix**
#'
#' The determinant checks for multicollinearity (values near zero indicate
#' variables are too highly correlated). This is a concern for factor analysis
#' estimation but not for PCA index construction — in fact, high correlations
#' are desirable when creating a composite, as they indicate variables measure
#' related aspects of the same construct. PCA handles collinearity without
#' computational issues.
#'
#' @param tax_structure_and_sdg_interpolated A data frame containing
#'   country, year, SDG variables, tax variables, and moderator variables.
#' @param valid_combinations A list of character vectors, where each vector
#'   contains variable names representing one SDG combination to evaluate.
#'
#' @return A tibble with one row per combination containing:
#' \describe{
#'   \item{combination}{The variable names in this combination}
#'   \item{nrow}{Number of complete cases}
#'   \item{sdg3_cor_matrix}{List column: correlation matrix for SDG3 variables}
#'   \item{sdg3_pca_result}{List column: prcomp output for SDG3 variables}
#'   \item{sdg3_pc1_var_explained}{Proportion of variance explained by PC1 for SDG3}
#'   \item{sdg3_min_abs_loading}{Minimum absolute loading on PC1 for SDG3}
#'   \item{sdg3_avg_correlation}{Mean absolute inter-item correlation for SDG3}
#'   \item{sdg3_all_cors_positive}{Logical: TRUE if all SDG3 correlations are positive}
#'   \item{sdg4_cor_matrix}{List column: correlation matrix for SDG4 variables}
#'   \item{sdg4_pca_result}{List column: prcomp output for SDG4 variables}
#'   \item{sdg4_pc1_var_explained}{Proportion of variance explained by PC1 for SDG4}
#'   \item{sdg4_min_abs_loading}{Minimum absolute loading on PC1 for SDG4}
#'   \item{sdg4_avg_correlation}{Mean absolute inter-item correlation for SDG4}
#'   \item{sdg4_all_cors_positive}{Logical: TRUE if all SDG4 correlations are positive}
#'   \item{total_rows}{Total rows in input data}
#'   \item{cols_retained}{Number of variables in this combination}
#'   \item{rows_retained_pct}{Proportion of rows retained after listwise deletion}
#'   \item{sdg3_pca_suitable}{Logical: TRUE if SDG3 meets all quality criteria}
#'   \item{sdg4_pca_suitable}{Logical: TRUE if SDG4 meets all quality criteria}
#'   \item{pca_suitable}{Logical: TRUE if both SDG3 and SDG4 are suitable}
#' }
#'
#' @references
#' Jolliffe, I. T. (2002). Principal Component Analysis (2nd ed.). Springer.
#'
#' @export
calculate_pca_quality_per_sdg_combination <-
  function(tax_structure_and_sdg_interpolated, valid_combinations) {
    # Helper function to calculate PCA quality metrics for a single SDG group
    calculate_sdg_metrics <- function(sdg_data) {
      if (ncol(sdg_data) < 2) {
        return(list(
          cor_matrix = NULL,
          pca_result = NULL,
          pc1_var_explained = NA_real_,
          min_abs_loading = NA_real_,
          avg_correlation = NA_real_,
          all_correlations_positive = NA
        ))
      }

      # Scale the data
      sdg_data_scaled <- sdg_data |>
        mutate(across(everything(), scale))

      # Correlation matrix
      cor_matrix <- cor(sdg_data_scaled, use = "pairwise.complete.obs")

      # Check if all correlations are positive
      off_diagonal <- cor_matrix[lower.tri(cor_matrix)]
      all_positive <- all(off_diagonal > 0)

      # Average correlation (not absolute, since we expect positive after alignment)
      avg_cor <- mean(abs(off_diagonal))

      # PCA
      pca_result <- tryCatch(
        prcomp(sdg_data_scaled, scale. = FALSE),
        error = function(e) NULL
      )

      if (is.null(pca_result)) {
        return(list(
          cor_matrix = cor_matrix,
          pca_result = NULL,
          pc1_var_explained = NA_real_,
          min_abs_loading = NA_real_,
          avg_correlation = avg_cor,
          all_correlations_positive = all_positive
        ))
      }

      pc1_var <- summary(pca_result)$importance["Proportion of Variance", 1]
      min_loading <- min(abs(pca_result$rotation[, 1]))

      list(
        cor_matrix = cor_matrix,
        pca_result = pca_result,
        pc1_var_explained = pc1_var,
        min_abs_loading = min_loading,
        avg_correlation = avg_cor,
        all_correlations_positive = all_positive
      )
    }

    results <- map(
      .x = valid_combinations,
      .f = function(sample) {
        # Get complete cases for this combination
        data_subset <- tax_structure_and_sdg_interpolated |>
          select(
            country,
            year,
            all_of(sample),
            contains("tax_"),
            contains("mod_")
          ) |>
          na.omit()

        n_rows <- nrow(data_subset)

        # Need minimum observations for meaningful PCA
        if (n_rows < 30) {
          return(tibble(
            nrow = n_rows,
            # SDG3 metrics
            sdg3_cor_matrix = list(NULL),
            sdg3_pca_result = list(NULL),
            sdg3_pc1_var_explained = NA_real_,
            sdg3_min_abs_loading = NA_real_,
            sdg3_avg_correlation = NA_real_,
            sdg3_all_cors_positive = NA,
            # SDG4 metrics
            sdg4_cor_matrix = list(NULL),
            sdg4_pca_result = list(NULL),
            sdg4_pc1_var_explained = NA_real_,
            sdg4_min_abs_loading = NA_real_,
            sdg4_avg_correlation = NA_real_,
            sdg4_all_cors_positive = NA
          ))
        }

        # Split variables by SDG group
        sdg3_vars <- sample[grepl("^sdg3_", sample)]
        sdg4_vars <- sample[grepl("^sdg4_", sample)]

        # Extract data for each SDG group
        sdg3_data <- data_subset |> select(all_of(sdg3_vars))
        sdg4_data <- data_subset |> select(all_of(sdg4_vars))

        # Calculate metrics for each group
        sdg3_metrics <- calculate_sdg_metrics(sdg3_data)
        sdg4_metrics <- calculate_sdg_metrics(sdg4_data)

        tibble(
          nrow = n_rows,
          # SDG3 metrics
          sdg3_cor_matrix = list(sdg3_metrics$cor_matrix),
          sdg3_pca_result = list(sdg3_metrics$pca_result),
          sdg3_pc1_var_explained = sdg3_metrics$pc1_var_explained,
          sdg3_min_abs_loading = sdg3_metrics$min_abs_loading,
          sdg3_avg_correlation = sdg3_metrics$avg_correlation,
          sdg3_all_cors_positive = sdg3_metrics$all_correlations_positive,
          # SDG4 metrics
          sdg4_cor_matrix = list(sdg4_metrics$cor_matrix),
          sdg4_pca_result = list(sdg4_metrics$pca_result),
          sdg4_pc1_var_explained = sdg4_metrics$pc1_var_explained,
          sdg4_min_abs_loading = sdg4_metrics$min_abs_loading,
          sdg4_avg_correlation = sdg4_metrics$avg_correlation,
          sdg4_all_cors_positive = sdg4_metrics$all_correlations_positive
        )
      },
      .progress = TRUE
    )

    # Combine results
    bind_cols(
      tibble(combination = valid_combinations),
      bind_rows(results)
    ) |>
      mutate(
        total_rows = nrow(tax_structure_and_sdg_interpolated),
        cols_retained = map_dbl(combination, length),
        rows_retained_pct = nrow / total_rows,
        mean_pc1_var_explained = (sdg3_pc1_var_explained +
          sdg4_pc1_var_explained) /
          2,
        # Index construction quality flags (separate for each SDG)
        sdg3_pca_suitable = sdg3_pc1_var_explained > 0.5 &
          sdg3_all_cors_positive,
        sdg4_pca_suitable = sdg4_pc1_var_explained > 0.5 &
          sdg4_all_cors_positive,
        # Overall suitability
        pca_suitable = sdg3_pca_suitable & sdg4_pca_suitable
      )
  }

#' Select best SDG combination
#'
#' @export
select_best_sdg_combination <-
  function(pca_quality_per_sdg_combination) {
    best_sdg_combination <-
      pca_quality_per_sdg_combination %>%
      filter(sdg3_all_cors_positive == TRUE & sdg4_all_cors_positive == TRUE) |>
      slice_max(order_by = mean_pc1_var_explained) %>%
      slice_max(order_by = nrow) %>%
      pointblank::row_count_match(count = 1)

    log <-
      function() {
        cli::cli_inform(
          "The best {.emph SDG combination} is the following:"
        )
        cli::cli_li(best_sdg_combination$combination[[1]])
        cli::cli_inform(
          "We retain {best_sdg_combination$nrow} out of {best_sdg_combination$total_rows} rows"
        )
        cli::cli_inform(
          "This corresponds to {scales::percent(best_sdg_combination$rows_retained_pct)} of items"
        )
      }
    log()

    list(
      combination = best_sdg_combination$combination[[1]],
      sdg3_pca_result = best_sdg_combination$sdg3_pca_result,
      sdg4_pca_result = best_sdg_combination$sdg4_pca_result
    )
  }

#' Perform Variable Selection and Keep Complete Rows
#'
#' @export
perform_variable_selection_and_keep_complete <-
  function(tax_structure_and_sdg_interpolated, best_sdg_combination) {
    tax_structure_and_sdg_interpolated %>%
      select(
        country,
        year,
        contains("tax_"),
        contains("mod_"),
        all_of(best_sdg_combination)
      ) |>
      na.omit()
  }

#' Create SDG Indices
#'
#' @description
#' Create the SDG3 and SDG4 indices based on the PCA outputs
#'
#' @details
#' Create new fields sdg3_index and sdg4_index. Then remove all SDG variables.
#'
#' @export
create_sdg_indices <-
  function(
    tax_structure_and_sdg_complete,
    best_sdg_combination
  ) {
    sdg3_idx_df <-
      best_sdg_combination$sdg3_pca_result[[1]] |>
      _$x |>
      as_tibble() |>
      rename(sdg3_index = PC1) |>
      select(sdg3_index)
    sdg4_idx_df <-
      best_sdg_combination$sdg4_pca_result[[1]] |>
      _$x |>
      as_tibble() |>
      rename(sdg4_index = PC1) |>
      select(sdg4_index)

    tax_structure_and_sdg_complete |>
      select(-matches("sdg3_|sdg4_")) |>
      bind_cols(
        sdg3_idx_df,
        sdg4_idx_df
      )
  }

#' Classify Country-Years by Tax Predominance
#'
#' @description
#' Classifies each country-year observation by tax predominance based on the
#' difference between income and consumption tax shares.
#'
#' @details
#' Tax variables used (OECD Revenue Statistics classification):
#' - **1000**: Taxes on income, profits and capital gains (PIT + CIT)
#' - **5110**: General taxes on goods and services (VAT/GST)
#'
#' Classification rule (10 percentage point threshold):
#' - **Consumption-predominant**: 5110 exceeds 1000 by >= 10pp
#' - **Income-predominant**: 1000 exceeds 5110 by >= 10pp
#' - **Balanced**: Absolute difference < 10pp
#'
#' @section Source:
#' OECD (2025). *Revenue Statistics - OECD Classification of Taxes and
#' Interpretative Guide*.
#' \url{https://www.oecd.org/tax/tax-policy/oecd-classification-taxes-interpretative-guide.pdf}
#'
#' @param tax_structure_and_sdg_pca A data frame containing `tax_general_consumption`
#'   (5110) and `tax_income_and_profits` (1000) columns as percentages of total
#'   taxation.
#' @return A data frame with a new `predominant_tax_composition` column and all
#'   original `tax_*` columns removed.
#'
#' @export
classify_tax_predominance <- function(tax_structure_and_sdg_pca) {
  tax_structure_and_sdg_pca |>
    mutate(
      tax_composition = case_when(
        tax_general_consumption - tax_income_and_profits >= 10 ~
          "predominant_consumption_tax",
        tax_income_and_profits - tax_general_consumption >= 10 ~
          "predominant_income_tax",
        .default = "balanced_tax_mix"
      )
    ) |>
    select(
      -tax_general_consumption,
      -tax_income_and_profits,
      -tax_goods_and_services
    )
}

#' Scale Moderating Variables
#'
#' @export
scale_mod_variables <-
  function(tax_composition_and_sdg) {
    tax_composition_and_sdg |>
      mutate(
        across(
          .cols = matches("^mod_"),
          .fns = scale
        )
      )
  }
