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

# Health variables for PCA (SDG 3)
sdg_3_health <-
  c(
    "sdg3_under_5_mortality_rate",
    "sdg3_neonatal_mortality_rate",
    "sdg3_maternal_mortality_ratio",
    "sdg3_death_rate_disease_30_to_70",
    "sdg3_surviving_infants_vaccinated"
  )

# Education variables for PCA (SDG 4)
sdg_4_education <-
  c(
    "sdg4_lower_secondary_completion_rate",
    "sdg4_adjusted_net_enrollment_rate",
    "sdg4_out_of_school_rate",
    "sdg4_adult_literacy_rate",
    "sdg4_n_years_free_education"
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
      ~var_name                              , ~gsheet_range ,
      # Tax variables
      "tax_goods_and_services"               , "B3:L32"      ,
      "tax_general_consumption"              , "B34:L63"     ,
      "tax_income_and_profits"               , "B65:L94"     ,

      # SDG 3 Health variables
      "sdg3_under_5_mortality_rate"          , "B96:L125"    ,
      "sdg3_neonatal_mortality_rate"         , "B127:L156"   ,
      "sdg3_maternal_mortality_ratio"        , "B158:L187"   ,
      "sdg3_death_rate_disease_30_to_70"     , "B189:L218"   ,
      "sdg3_surviving_infants_vaccinated"    , "B220:L249"   ,

      # SDG 4 Education variables
      "sdg4_lower_secondary_completion_rate" , "B251:L280"   ,
      "sdg4_adjusted_net_enrollment_rate"    , "B282:L311"   ,
      "sdg4_out_of_school_rate"              , "B313:L342"   ,
      "sdg4_adult_literacy_rate"             , "B344:L373"   ,
      "sdg4_n_years_free_education"          , "B375:L404"   ,

      # Moderating variables
      "mod_gdp_per_capita"                   , "B406:L435"   ,
      "mod_macroeconomic_population"         , "B437:L466"   ,
      "mod_debt_to_gpt_ratio"                , "B468:L497"
    ),
    names = var_name,
    tar_target(
      raw,
      googlesheets4::read_sheet(
        ss = analysis_data_url,
        sheet = "CLEANUP DATA",
        range = gsheet_range
      )
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
  ),

  # ============================================================================
  # MISSINGNESS ANALYSIS
  # ============================================================================

  # tar_target(
  #   missing_per_year,
  #   calculate_missingness(tax_structure_and_sdg)
  # )

  # ============================================================================
  # CORRELATION ANALYSIS FOR IMPUTATION VARIABLE SELECTION
  # ============================================================================

  # Analyze SDG3 health variable correlations
  tar_target(
    sdg3_correlation_analysis,
    analyze_sdg3_correlations(tax_structure_and_sdg)
  ),

  # Analyze SDG4 education variable correlations
  tar_target(
    sdg4_correlation_analysis,
    analyze_sdg4_correlations(tax_structure_and_sdg)
  ),

  # ============================================================================
  # PANEL IMPUTATION AND PCA FOR SDG3 HEALTH
  # ============================================================================

  # Fit multilevel panel imputation model
  tar_target(
    sdg3_imputation_fit,
    fit_sdg3_panel_imputation(
      tax_structure_and_sdg,
      m = 5,
      maxit = 20,
      seed = 123
    )
  ),

  # Extract all 5 imputed datasets for sensitivity analysis
  tar_target(
    sdg3_imputed_datasets,
    extract_sdg3_imputed_data(sdg3_imputation_fit, action = "all")
  ),

  # Extract first imputation as primary dataset for PCA
  tar_target(
    sdg3_imputed_primary,
    extract_sdg3_imputed_data(sdg3_imputation_fit, action = 1)
  ),

  # Extract long format for pooled analysis (optional)
  tar_target(
    sdg3_imputed_long,
    extract_sdg3_imputed_data(sdg3_imputation_fit, action = "long")
  ),

  # Generate imputation diagnostic plots
  tar_target(
    sdg3_imputation_diagnostics,
    generate_sdg3_imputation_diagnostics(sdg3_imputation_fit)
  ),

  # Validate PCA suitability on imputed data
  tar_target(
    sdg3_pca_validation,
    {
      # Select only SDG3 variables for validation
      sdg3_imputed_primary |>
        dplyr::select(
          sdg3_under_5_mortality_rate,
          sdg3_neonatal_mortality_rate,
          sdg3_maternal_mortality_ratio
        ) |>
        validate_pca_suitability()
    }
  ),

  # Fit PCA model on imputed SDG3 data
  tar_target(
    sdg3_pca_fit,
    fit_sdg3_pca(sdg3_imputed_primary)
  ),

  # Extract SDG3 index from PCA
  tar_target(
    sdg3_index,
    extract_sdg3_index(sdg3_pca_fit)
  ),

  # ============================================================================
  # PANEL IMPUTATION AND PCA FOR SDG4 EDUCATION
  # ============================================================================

  # Fit multilevel panel imputation model
  tar_target(
    sdg4_imputation_fit,
    fit_sdg4_panel_imputation(
      tax_structure_and_sdg,
      m = 5,
      maxit = 20,
      seed = 123
    )
  ),

  # Extract all 5 imputed datasets for sensitivity analysis
  tar_target(
    sdg4_imputed_datasets,
    extract_sdg4_imputed_data(sdg4_imputation_fit, action = "all")
  ),

  # Extract first imputation as primary dataset for PCA
  tar_target(
    sdg4_imputed_primary,
    extract_sdg4_imputed_data(sdg4_imputation_fit, action = 1)
  ),

  # Extract long format for pooled analysis (optional)
  tar_target(
    sdg4_imputed_long,
    extract_sdg4_imputed_data(sdg4_imputation_fit, action = "long")
  ),

  # Generate imputation diagnostic plots
  tar_target(
    sdg4_imputation_diagnostics,
    generate_sdg4_imputation_diagnostics(sdg4_imputation_fit)
  ),

  # Validate PCA suitability on imputed data
  tar_target(
    sdg4_pca_validation,
    validate_sdg4_pca_suitability(sdg4_imputed_primary)
  ),

  # Fit PCA model on imputed SDG4 data
  tar_target(
    sdg4_pca_fit,
    fit_sdg4_pca(sdg4_imputed_primary)
  ),

  # Extract SDG4 index from PCA
  tar_target(
    sdg4_index,
    extract_sdg4_index(sdg4_pca_fit)
  ),

  # ============================================================================
  # ANALYSIS-READY DATASET
  # ============================================================================

  # Combine SDG3 index, SDG4 index, and full dataset
  tar_target(
    analysis_ready_data,
    {
      tax_structure_and_sdg |>
        dplyr::left_join(sdg3_index, by = c("country", "year")) |>
        dplyr::left_join(sdg4_index, by = c("country", "year"))
    }
  ),

  # ============================================================================
  # QUARTO REPORT
  # ============================================================================

  # Render data preparation report
  tar_quarto(
    data_preparation_report,
    path = "data_preparation_report.qmd"
  )
)
