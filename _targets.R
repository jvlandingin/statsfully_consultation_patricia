# _targets.R
# Tax Structure and SDG Outcomes Analysis Pipeline
# See https://books.ropensci.org/targets/ for documentation

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tibble)

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

# Health variables for PCA (SDG 3)
sdg_3_health <-
  c(
    "sdg3_under_5_mortality_rate",
    "sdg3_neonatal_mortality_rate",
    "sdg3_maternal_mortality_ratio",
    "sdg3_prop_births_with_skilled_personnel",
    "sdg3_coverage_of_essential_health_services"
  )

# Education variables for PCA (SDG 4)
sdg_4_education <-
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

# Moderating Variables
mod_variables <-
  c(
    "mod_macroeconomic_population",
    "mod_gdp_per_capita",
    "mod_debt_to_gpt_ratio"
  )

# Tax Variables
tax_variables <-
  c(
    "tax_goods_and_services",
    "tax_general_consumption",
    "tax_income_and_profits"
  )

all_variables <-
  c(
    sdg_3_health,
    sdg_4_education,
    mod_variables,
    tax_variables
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

  # ============================================================================
  # EXTRACT RAW DATA FROM GOOGLE SHEETS
  # ============================================================================

  tar_map(
    values = tribble(
      ~var_name                                    , ~gsheet_range ,
      # Tax variables
      "tax_goods_and_services"                     , "B3:L33"      ,
      "tax_general_consumption"                    , "B35:L65"     ,
      "tax_income_and_profits"                     , "B67:L97"     ,
      # SDG 3 Health variables
      "sdg3_under_5_mortality_rate"                , "B101:L131"   ,
      "sdg3_neonatal_mortality_rate"               , "B133:L163"   ,
      "sdg3_maternal_mortality_ratio"              , "B165:L195"   ,
      "sdg3_prop_births_with_skilled_personnel"    , "B197:L227"   ,
      "sdg3_coverage_of_essential_health_services" , "B229:L259"   ,
      # SDG 4 Education variables
      "sdg4_prop_students_proficient_math"         , "B263:L293"   ,
      "sdg4_prop_students_proficient_reading"      , "B295:L325"   ,
      "sdg4_prop_primary_secondary"                , "B327:L357"   ,
      "sdg4_lower_secondary"                       , "B359:L389"   ,
      "sdg4_upper_secondary"                       , "B391:L421"   ,
      "sdg4_adult_literacy_rate"                   , "B424:L454"   ,
      "sdg4_prop_trained_teachers_pre_primary"     , "B456:L486"   ,
      "sdg4_prop_trained_teachers_primary"         , "B488:L518"   ,
      "sdg4_prop_trained_teachers_lower_secondary" , "B520:L550"   ,
      "sdg4_prop_trained_teachers_upper_secondary" , "B552:L582"   ,
      "sdg4_prop_trained_teachers_secondary"       , "B584:L614"   ,
      "sdg4_gender_gap_in_completion"              , "B616:L646"   ,
      # Moderating variables
      "mod_gdp_per_capita"                         , "B651:L681"   ,
      "mod_macroeconomic_population"               , "B683:L713"   ,
      "mod_debt_to_gpt_ratio"                      , "B715:L745"
    ),
    names = var_name,
    tar_target(
      raw,
      extract_googlesheet_data(analysis_data_url, gsheet_range)
    )
  ),

  # ============================================================================
  # TIDY RAW DATA
  # ============================================================================

  tar_map(
    values = tibble(
      var_name = c(sdg_3_health, sdg_4_education, mod_variables, tax_variables), # Just the base names: "sdg3_under_5_mortality_rate"
      var_symbol = rlang::syms(paste0(
        "raw_",
        c(sdg_3_health, sdg_4_education, mod_variables, tax_variables)
      )) # Convert to symbols
    ),
    names = var_name, # Use var_name for naming the output targets
    tar_target(
      tidy,
      tidy_googlesheet_extract(
        var_symbol, # This gets substituted with the actual target symbol
        var_name # This gets substituted with the string name
      )
    )
  ),

  # ============================================================================
  # INTEGRATE DATA
  # ============================================================================

  tar_target(
    tax_structure_and_sdg,
    integrate_panel_data(
      !!!rlang::syms(paste0("tidy_", all_variables))
    )
  )

  # ============================================================================
  # MISSINGNESS ANALYSIS
  # ============================================================================

  # tar_target(
  #   missing_per_year,
  #   calculate_missingness(tax_structure_and_sdg)
  # )

  # ============================================================================
  # PCA FOR SDG3 INDEX
  # ============================================================================
)
