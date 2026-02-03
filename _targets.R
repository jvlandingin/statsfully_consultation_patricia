# _targets.R
# Tax Structure and SDG Outcomes Analysis Pipeline
# See https://books.ropensci.org/targets/ for documentation

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tibble)

# Source functions from R/ directory
tar_source(
  c("R/transform.R", "R/integration.R", "R/settings.R")
)

# Set target options (packages used by targets)
tar_option_set(
  packages = c(
    "googlesheets4",
    "purrr",
    "dplyr",
    "tidyr",
    "pointblank",
    "mice", # For multiple imputation
    "psych", # For PCA validation tests (KMO, Bartlett)
    "lattice" # For mice density plots (diagnostics)
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

  # ============================================================================
  # EXTRACT RAW DATA FROM GOOGLE SHEETS
  # ============================================================================

  tar_map(
    values = tribble(
      ~var_name                              , ~gsheet_range , ~gsheet_sheet  ,
      # Tax variables
      "tax_goods_and_services"               , "B3:L32"      , "CLEANUP DATA" ,
      "tax_general_consumption"              , "B34:L63"     , "CLEANUP DATA" ,
      "tax_income_and_profits"               , "B65:L94"     , "CLEANUP DATA" ,

      # SDG 3 Health variables
      "sdg3_under_5_mortality_rate"          , "B96:L125"    , "CLEANUP DATA" ,
      "sdg3_neonatal_mortality_rate"         , "B127:L156"   , "CLEANUP DATA" ,
      "sdg3_maternal_mortality_ratio"        , "B158:L187"   , "CLEANUP DATA" ,
      "sdg3_prop_births_with_skilled_person" , "B197:L227"   , "DATA"         ,
      "sdg3_coverage_essential_health"       , "B229:L259"   , "DATA"         ,
      "sdg3_death_rate_disease_30_to_70"     , "B189:L218"   , "CLEANUP DATA" ,
      "sdg3_surviving_infants_vaccinated"    , "B220:L249"   , "CLEANUP DATA" ,

      # SDG 4 Education variables
      "sdg4_primary_proficiency_math"        , "B263:293"    , "DATA"         ,
      "sdg4_primary_proficiency_reading"     , "B295:325"    , "DATA"         ,
      "sdg4_primary_secondary_percent"       , "B327:357"    , "DATA"         ,
      "sdg4_upper_secondary_completion_rate" , "B391:L421"   , "DATA"         ,
      "sdg4_prop_teachers_pre_primary"       , "B456:L486"   , "DATA"         ,
      "sdg4_prop_teachers_primary"           , "B488:L518"   , "DATA"         ,
      "sdg4_prop_teachers_lower_secondary"   , "B520:L550"   , "DATA"         ,
      "sdg4_prop_teachers_upper_secondary"   , "B552:L582"   , "DATA"         ,
      "sdg4_prop_teachers_secondary"         , "B584:L614"   , "DATA"         ,
      "sdg4_gender_gap_completion"           , "B616:L646"   , "DATA"         ,
      "sdg4_lower_secondary_completion_rate" , "B251:L280"   , "CLEANUP DATA" ,
      "sdg4_adjusted_net_enrollment_rate"    , "B282:L311"   , "CLEANUP DATA" ,
      "sdg4_out_of_school_rate"              , "B313:L342"   , "CLEANUP DATA" ,
      "sdg4_adult_literacy_rate"             , "B344:L373"   , "CLEANUP DATA" ,
      "sdg4_n_years_free_education"          , "B375:L404"   , "CLEANUP DATA" ,

      # Moderating variables
      "mod_gdp_per_capita"                   , "B406:L435"   , "CLEANUP DATA" ,
      "mod_macroeconomic_population"         , "B437:L466"   , "CLEANUP DATA" ,
      "mod_debt_to_gdp_ratio"                , "B468:L497"   , "CLEANUP DATA"
    ),
    names = var_name,
    tar_target(
      raw,
      googlesheets4::read_sheet(
        ss = analysis_data_url,
        sheet = gsheet_sheet,
        range = gsheet_range
      )
    )
  ),

  # ============================================================================
  # TIDY RAW DATA
  # ============================================================================

  tar_map(
    values = tibble(
      # Just the base names: "sdg3_under_5_mortality_rate"
      var_name = c(sdg_3_health, sdg_4_education, mod_variables, tax_variables),
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
  ),

  # ============================================================================
  # DATA CLEANING
  # ============================================================================

  tar_target(
    tax_structure_and_sdg_filtered,
    filter_tax_structure_and_sdg(tax_structure_and_sdg)
  ),
  tar_target(
    tax_structure_and_sdg_aligned,
    command = align_variable_direction(tax_structure_and_sdg_filtered)
  ),

  # ====================
  # VARIABLE SELECTION
  # ====================

  tar_target(
    tax_structure_and_sdg_interpolated,
    perform_interpolation(tax_structure_and_sdg_aligned)
  ),
  tar_target(
    valid_combinations,
    generate_valid_sdg_combinations(tax_structure_and_sdg_interpolated)
  ),
  tar_target(
    remaining_rows_per_sdg_combination,
    calculate_remaining_rows_per_sdg_combination(
      tax_structure_and_sdg_interpolated = tax_structure_and_sdg_interpolated,
      valid_combinations = valid_combinations
    )
  ),
  tar_target(
    name = pca_quality_per_sdg_combination,
    command = calculate_pca_quality_per_sdg_combination(
      tax_structure_and_sdg_interpolated = tax_structure_and_sdg_interpolated,
      # Only calculate PCA quality for combinations that kept at least 70 rows
      valid_combinations = remaining_rows_per_sdg_combination %>%
        filter(nrow >= 70) %>%
        pull(combination)
    )
  ),
  tar_target(
    name = best_sdg_combination,
    command = select_best_sdg_combination(pca_quality_per_sdg_combination)
  ),
  tar_target(
    name = tax_structure_and_sdg_complete,
    command = perform_variable_selection_and_keep_complete(
      tax_structure_and_sdg_interpolated = tax_structure_and_sdg_interpolated,
      best_sdg_combination = best_sdg_combination$combination
    )
  ),

  # ====================
  # PCA
  # ====================
  tar_target(
    tax_structure_and_sdg_pca,
    command = create_sdg_indices(
      tax_structure_and_sdg_complete = tax_structure_and_sdg_complete,
      best_sdg_combination = best_sdg_combination
    )
  ),

  # ====================
  # Other Pre-processing
  # ====================
  tar_target(
    tax_composition_and_sdg,
    command = classify_tax_predominance(tax_structure_and_sdg_pca)
  ),
  tar_target(
    tax_composition_sdg_idx_scaled_mod,
    command = scale_mod_variables(tax_composition_and_sdg)
  )
)
