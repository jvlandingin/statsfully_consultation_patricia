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
