# Health variables for PCA (SDG 3)
sdg_3_health <-
  c(
    "sdg3_under_5_mortality_rate",
    "sdg3_neonatal_mortality_rate",
    "sdg3_maternal_mortality_ratio",
    "sdg3_prop_births_with_skilled_person",
    "sdg3_coverage_essential_health",
    "sdg3_death_rate_disease_30_to_70",
    "sdg3_surviving_infants_vaccinated"
  )

# Education variables for PCA (SDG 4)
sdg_4_education <-
  c(
    "sdg4_primary_proficiency_math",
    "sdg4_primary_proficiency_reading",
    "sdg4_primary_secondary_percent",
    "sdg4_upper_secondary_completion_rate",
    "sdg4_prop_teachers_pre_primary",
    "sdg4_prop_teachers_primary",
    "sdg4_prop_teachers_lower_secondary",
    "sdg4_prop_teachers_upper_secondary",
    "sdg4_prop_teachers_secondary",
    "sdg4_gender_gap_completion",
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
    "mod_debt_to_gdp_ratio"
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
