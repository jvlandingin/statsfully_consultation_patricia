# Initialization ---------------------------------------------------------

box::use(
  googlesheets4[...],
  dplyr[...],
  tidyr[...]
)

googlesheets4::gs4_auth(
  email = "johnvincentland@gmail.com"
)

analysis_data_url <-
  "https://docs.google.com/spreadsheets/d/1tCuj6KMPIyLbE1QQGtY08Rgj0SF3Wrr2yi83UXF8qB4/edit?pli=1&gid=808174644#gid=808174644"

# Extract Data -----------------------------------------------------------

tax_good_and_services_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B3:L33"
  )
tax_general_consumption_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B35:L65"
  )
tax_income_and_profits_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B67:L97"
  )
under_5_mortality_rate_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B99:L129"
  )

neonatal_mortality_rate_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B131:L161"
  )

maternal_mortality_ratio_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B163:L193"
  )

prop_births_with_skilled_personnel_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B195:L225"
  )

coverage_of_essential_health_services_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B227:L257"
  )

lower_secondary_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B291:L321"
  )

adult_literacy_rate_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B323:L353"
  )

macroeconomic_population_raw <-
  googlesheets4::read_sheet(
    ss = analysis_data_url,
    sheet = "DATA SHEET",
    range = "B451:L481"
  )

# To be used for checking
# list all the countries used
all_countries <-
  tax_good_and_services_raw |>
  slice_head(n = 29) |>
  pull(`List of Countries (Asia-Pacific Region)`)

# Tidy Data --------------------------------------------------------------

#' Tidy googlesheet extract
#'
#' @description
#' Prepare raw googlesheets table extract to tidy format
#'
tidy_googlesheet_extract <-
  function(
    googlesheet_extract,
    field_name
  ) {
    last_row <-
      googlesheet_extract |>
      slice_tail(n = 1) |>
      select(-`List of Countries (Asia-Pacific Region)`)

    initial <-
      googlesheet_extract |>
      filter(`List of Countries (Asia-Pacific Region)` != "Years Covered")

    colnames(initial) <-
      c("country", as.character(last_row))

    tidy_format <-
      initial |>
      pivot_longer(
        cols = -country,
        names_to = "year",
        values_to = field_name
      ) |>
      mutate(year = as.numeric(year))

    # Correction
    tidy_format <-
      tidy_format |>
      mutate(
        country = case_when(
          country == "Kyrgyzstan" ~ "Krygyzstan",
          country == "Solomon Islands" ~ "Solomon Island",
          .default = country
        )
      )

    # Data validation
    tidy_format |>
      pointblank::col_is_numeric(columns = c("year", all_of(field_name))) |>
      pointblank::col_is_character(columns = "country") |>
      pointblank::col_vals_not_null(
        columns = c("country", "year")
      ) |>
      pointblank::col_vals_not_null(
        columns = all_of(field_name),
        actions = pointblank::action_levels(warn_at = 0.80)
      ) |>
      # Check that all countries exist
      pointblank::col_vals_in_set(
        columns = "country",
        set = all_countries,
        actions = pointblank::warn_on_fail(warn_at = 1)
      ) |>
      pointblank::col_vals_in_set(
        columns = "year",
        set = seq(2014, 2023),
        actions = pointblank::warn_on_fail(warn_at = 1)
      )

    return(tidy_format)
  }

tax_good_and_services_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = tax_good_and_services_raw,
    field_name = "tax_goods_and_services"
  )


tax_general_consumption_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = tax_general_consumption_raw,
    field_name = "tax_general_consumption"
  )


tax_income_and_profits_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = tax_income_and_profits_raw,
    field_name = "tax_income_and_profits"
  )


under_5_mortality_rate_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = under_5_mortality_rate_raw,
    field_name = "under_5_mortality_rate"
  )


neonatal_mortality_rate_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = neonatal_mortality_rate_raw,
    field_name = "neonatal_mortality_rate"
  )

maternal_mortality_ratio_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = maternal_mortality_ratio_raw,
    field_name = "maternal_mortality_ratio"
  )

prop_births_with_skilled_personnel_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = prop_births_with_skilled_personnel_raw,
    field_name = "prop_births_with_skilled_personnel"
  )

coverage_of_essential_health_services_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = coverage_of_essential_health_services_raw,
    field_name = "coverage_of_essential_health_services"
  )

lower_secondary_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = lower_secondary_raw,
    field_name = "lower_secondary"
  )

adult_literacy_rate_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = adult_literacy_rate_raw,
    field_name = "adult_literacy_rate"
  )

macroeconomic_population_tidy <-
  tidy_googlesheet_extract(
    googlesheet_extract = macroeconomic_population_raw,
    field_name = "macroeconomic_population"
  )

# Integrate data ---------------------------------------------------------

full_join_one_to_one_no_na <-
  function(
    x,
    y
  ) {
    full_join(
      x = x,
      y = y,
      by = join_by(country, year),
      relationship = "one-to-one",
      na_matches = "never"
    )
  }

tax_structure_and_sdg <-
  tax_good_and_services_tidy |>
  full_join_one_to_one_no_na(
    tax_general_consumption_tidy
  ) |>
  full_join_one_to_one_no_na(
    tax_income_and_profits_tidy
  ) |>
  full_join_one_to_one_no_na(
    under_5_mortality_rate_tidy
  ) |>
  full_join_one_to_one_no_na(
    neonatal_mortality_rate_tidy
  ) |>
  full_join_one_to_one_no_na(
    maternal_mortality_ratio_tidy
  ) |>
  full_join_one_to_one_no_na(
    prop_births_with_skilled_personnel_tidy
  ) |>
  full_join_one_to_one_no_na(
    coverage_of_essential_health_services_tidy
  ) |>
  full_join_one_to_one_no_na(
    lower_secondary_tidy
  ) |>
  full_join_one_to_one_no_na(
    adult_literacy_rate_tidy
  ) |>
  full_join_one_to_one_no_na(
    macroeconomic_population_tidy
  )

tax_structure_and_sdg
