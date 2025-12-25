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
      "sdg4_prop_students_proficient_math",
      "sdg4_prop_students_proficient_reading",
      "sdg4_prop_primary_secondary",
      "sdg4_lower_secondary",
      "sdg4_upper_secondary",
      "sdg4_adult_literacy_rate",
      "sdg4_prop_trained_teachers_pre_primary",
      "sdg4_prop_trained_teachers_primary",
      "sdg4_prop_trained_teachers_lower_secondary",
      "sdg4_prop_trained_teachers_upper_secondary",
      "sdg4_prop_trained_teachers_secondary",
      "sdg4_gender_gap_in_completion"
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
    sdg3_under_5_mortality_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B101:L131")
  ),

  tar_target(
    sdg3_neonatal_mortality_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B133:L163")
  ),

  tar_target(
    sdg3_maternal_mortality_ratio_raw,
    extract_googlesheet_data(analysis_data_url, "B165:L195")
  ),

  tar_target(
    sdg3_prop_births_with_skilled_personnel_raw,
    extract_googlesheet_data(analysis_data_url, "B197:L227")
  ),

  tar_target(
    sdg3_coverage_of_essential_health_services_raw,
    extract_googlesheet_data(analysis_data_url, "B229:L259")
  ),

  tar_target(
    sdg4_prop_students_proficient_math_raw,
    extract_googlesheet_data(analysis_data_url, "B263:L293")
  ),

  tar_target(
    sdg4_prop_students_proficient_reading_raw,
    extract_googlesheet_data(analysis_data_url, "B295:L325")
  ),

  tar_target(
    sdg4_prop_primary_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B327:L357")
  ),

  tar_target(
    sdg4_lower_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B359:L389")
  ),

  tar_target(
    sdg4_upper_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B391:L421")
  ),

  tar_target(
    sdg4_adult_literacy_rate_raw,
    extract_googlesheet_data(analysis_data_url, "B424:L454")
  ),

  tar_target(
    sdg4_prop_trained_teachers_pre_primary_raw,
    extract_googlesheet_data(analysis_data_url, "B456:L486")
  ),

  tar_target(
    sdg4_prop_trained_teachers_primary_raw,
    extract_googlesheet_data(analysis_data_url, "B489:L519")
  ),

  tar_target(
    sdg4_prop_trained_teachers_lower_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B520:L550")
  ),

  tar_target(
    sdg4_prop_trained_teachers_upper_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B552:L582")
  ),

  tar_target(
    sdg4_prop_trained_teachers_secondary_raw,
    extract_googlesheet_data(analysis_data_url, "B584:L614")
  ),

  tar_target(
    sdg4_gender_gap_in_completion_raw,
    extract_googlesheet_data(analysis_data_url, "B616:L646")
  ),

  tar_target(
    mod_macroeconomic_population_raw,
    extract_googlesheet_data(analysis_data_url, "B683:L713")
  ),

  tar_target(
    mod_gdp_per_capita_raw,
    extract_googlesheet_data(analysis_data_url, "B651:L681")
  ),

  tar_target(
    mod_debt_to_gpt_ratio_raw,
    extract_googlesheet_data(analysis_data_url, "B715:L745")
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
      sdg3_under_5_mortality_rate_raw,
      "sdg3_under_5_mortality_rate"
    )
  ),

  tar_target(
    sdg3_neonatal_mortality_rate_tidy,
    tidy_googlesheet_extract(
      sdg3_neonatal_mortality_rate_raw,
      "sdg3_neonatal_mortality_rate"
    )
  ),

  tar_target(
    sdg3_maternal_mortality_ratio_tidy,
    tidy_googlesheet_extract(
      sdg3_maternal_mortality_ratio_raw,
      "sdg3_maternal_mortality_ratio"
    )
  ),

  tar_target(
    sdg3_prop_births_with_skilled_personnel_tidy,
    tidy_googlesheet_extract(
      sdg3_prop_births_with_skilled_personnel_raw,
      "sdg3_prop_births_with_skilled_personnel"
    )
  ),

  tar_target(
    sdg3_coverage_of_essential_health_services_tidy,
    tidy_googlesheet_extract(
      sdg3_coverage_of_essential_health_services_raw,
      "sdg3_coverage_of_essential_health_services"
    )
  ),

  tar_target(
    sdg4_lower_secondary_tidy,
    tidy_googlesheet_extract(sdg4_lower_secondary_raw, "sdg4_lower_secondary")
  ),

  tar_target(
    sdg4_adult_literacy_rate_tidy,
    tidy_googlesheet_extract(
      sdg4_adult_literacy_rate_raw,
      "sdg4_adult_literacy_rate"
    )
  ),

  tar_target(
    mod_macroeconomic_population_tidy,
    tidy_googlesheet_extract(
      mod_macroeconomic_population_raw,
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
