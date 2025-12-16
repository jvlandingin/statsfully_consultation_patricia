# _targets.R
# Tax Structure and SDG Outcomes Analysis Pipeline
# See https://books.ropensci.org/targets/ for documentation

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)

# Source functions from R/ directory
tar_source(
  "R/functions.R"
)

# Set target options (packages used by targets)
tar_option_set(
  packages = c(
    "googlesheets4",
    "dplyr",
    "tidyr",
    "pointblank"
  )
)

googlesheets4::gs4_auth(
  email = "johnvincentland@gmail.com",
  cache = ".secrets/gs4_cache"
)

# Define the pipeline
list(
  # ============================================================================
  # CONFIGURATION TARGETS
  # ============================================================================

  # Google Sheets URL for the data
  tar_target(
    analysis_data_url,
    "https://docs.google.com/spreadsheets/d/1tCuj6KMPIyLbE1QQGtY08Rgj0SF3Wrr2yi83UXF8qB4/edit?pli=1&gid=808174644#gid=808174644"
  ),

  # Health variables for PCA (SDG 3)
  tar_target(
    sdg_3_health,
    c(
      "sdg3_under_5_mortality_rate",
      "sdg3_neonatal_mortality_rate",
      "sdg3_maternal_mortality_ratio",
      "sdg3_prop_births_with_skilled_personnel",
      "sdg3_coverage_of_essential_health_services"
    )
  ),

  # Education variables for PCA (SDG 4)
  tar_target(
    sdg_4_education,
    c(
      "sdg4_adult_literacy_rate",
      "sdg4_lower_secondary"
    )
  ),

  # ============================================================================
  # EXTRACT RAW DATA FROM GOOGLE SHEETS
  # ============================================================================

  tar_target(
    tax_goods_and_services_raw,
    extract_googlesheet_data(analysis_data_url, "B3:L33")
  ),

  tar_target(
    tax_general_consumption_raw,
    extract_googlesheet_data(analysis_data_url, "B35:L65")
  ),

  tar_target(
    tax_income_and_profits_raw,
    extract_googlesheet_data(analysis_data_url, "B67:L97")
  ),

  tar_target(
    under_5_mortality_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B99:L129")
  ),

  tar_target(
    neonatal_mortality_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B131:L161")
  ),

  tar_target(
    maternal_mortality_ratio_raw,
    extract_googlesheet_data(analysis_data_url, "B163:L193")
  ),

  tar_target(
    prop_births_with_skilled_personnel_raw,
    extract_googlesheet_data(analysis_data_url, "B195:L225")
  ),

  tar_target(
    coverage_of_essential_health_services_raw,
    extract_googlesheet_data(analysis_data_url, "B227:L257")
  ),

  tar_target(
    lower_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B291:L321")
  ),

  tar_target(
    adult_literacy_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B323:L353")
  ),

  tar_target(
    macroeconomic_population_raw,
    extract_googlesheet_data(analysis_data_url, "B451:L481")
  ),

  tar_target(
    mod_gdp_per_capita_raw,
    extract_googlesheet_data(analysis_data_url, "B419:L449")
  ),

  tar_target(
    mod_debt_to_gpt_ratio_raw,
    extract_googlesheet_data(analysis_data_url, "B483:L513")
  ),

  # ============================================================================
  # TIDY RAW DATA
  # ============================================================================

  tar_target(
    tax_goods_and_services_tidy,
    tidy_googlesheet_extract(
      tax_goods_and_services_raw,
      "tax_goods_and_services"
    )
  ),

  tar_target(
    tax_general_consumption_tidy,
    tidy_googlesheet_extract(
      tax_general_consumption_raw,
      "tax_general_consumption"
    )
  ),

  tar_target(
    tax_income_and_profits_tidy,
    tidy_googlesheet_extract(
      tax_income_and_profits_raw,
      "tax_income_and_profits"
    )
  ),

  tar_target(
    sdg3_under_5_mortality_rate_tidy,
    tidy_googlesheet_extract(
      under_5_mortality_rate_raw,
      "sdg3_under_5_mortality_rate"
    )
  ),

  tar_target(
    sdg3_neonatal_mortality_rate_tidy,
    tidy_googlesheet_extract(
      neonatal_mortality_rate_raw,
      "sdg3_neonatal_mortality_rate"
    )
  ),

  tar_target(
    sdg3_maternal_mortality_ratio_tidy,
    tidy_googlesheet_extract(
      maternal_mortality_ratio_raw,
      "sdg3_maternal_mortality_ratio"
    )
  ),

  tar_target(
    sdg3_prop_births_with_skilled_personnel_tidy,
    tidy_googlesheet_extract(
      prop_births_with_skilled_personnel_raw,
      "sdg3_prop_births_with_skilled_personnel"
    )
  ),

  tar_target(
    sdg3_coverage_of_essential_health_services_tidy,
    tidy_googlesheet_extract(
      coverage_of_essential_health_services_raw,
      "sdg3_coverage_of_essential_health_services"
    )
  ),

  tar_target(
    sdg4_lower_secondary_tidy,
    tidy_googlesheet_extract(lower_secondary_raw, "sdg4_lower_secondary")
  ),

  tar_target(
    sdg4_adult_literacy_rate_tidy,
    tidy_googlesheet_extract(
      adult_literacy_rate_raw,
      "sdg4_adult_literacy_rate"
    )
  ),

  tar_target(
    mod_macroeconomic_population_tidy,
    tidy_googlesheet_extract(
      macroeconomic_population_raw,
      "mod_macroeconomic_population"
    )
  ),

  tar_target(
    mod_gdp_per_capita_tidy,
    tidy_googlesheet_extract(
      mod_gdp_per_capita_raw,
      "mod_gdp_per_capita"
    )
  ),

  tar_target(
    mod_debt_to_gpt_ratio_tidy,
    tidy_googlesheet_extract(
      mod_debt_to_gpt_ratio_raw,
      "mod_debt_to_gpt_ratio"
    )
  ),

  # ============================================================================
  # INTEGRATE DATA
  # ============================================================================

  tar_target(
    tax_structure_and_sdg,
    integrate_panel_data(
      tax_goods_and_services_tidy,
      tax_general_consumption_tidy,
      tax_income_and_profits_tidy,
      sdg3_under_5_mortality_rate_tidy,
      sdg3_neonatal_mortality_rate_tidy,
      sdg3_maternal_mortality_ratio_tidy,
      sdg3_prop_births_with_skilled_personnel_tidy,
      sdg3_coverage_of_essential_health_services_tidy,
      sdg4_lower_secondary_tidy,
      sdg4_adult_literacy_rate_tidy,
      mod_macroeconomic_population_tidy,
      mod_gdp_per_capita_tidy,
      mod_debt_to_gpt_ratio_tidy
    )
  ),

  # ============================================================================
  # MISSINGNESS ANALYSIS
  # ============================================================================

  tar_target(
    missing_per_year,
    calculate_missingness(tax_structure_and_sdg)
  )

  # ============================================================================
  # PCA FOR SDG3 INDEX
  # ============================================================================
)
