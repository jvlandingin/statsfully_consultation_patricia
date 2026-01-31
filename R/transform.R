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

#' Perform Variable Selection
#'
#' @description
#' Perform Variable Selection on SDG data.
#'
#' @details
#' Only 5 SDG3 and 5 SDG4 variables are retained.
#' The purpose of this step is to retain as much records as possible after
#' deleting incomplete cases.
#'
#' @export
perform_variable_selection <-
  function(tax_structure_and_sdg_filtered, valid_samples) {}
