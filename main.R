box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  targets[tar_load]
)

tar_load(tax_structure_and_sdg)
tar_load(sdg_3_health)
tar_load(sdg_4_education)

tar_load(missing_per_year)

# Data Exploration -------------------------------------------------------

# For 3 reliable mortality indicators + tax vars (2017-2022)
analysis_ready <- tax_structure_and_sdg |>
  filter(year >= 2017 & year <= 2022) |>
  filter(complete.cases(pick(
    under_5_mortality_rate,
    neonatal_mortality_rate,
    maternal_mortality_ratio,
    tax_goods_and_services,
    tax_income_and_profits
  )))

vis_dat(analysis_ready)

categ_variables_tidy <-
  tax_structure_and_sdg |>
  pivot_longer(
    cols = -c(country, year),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    group = case_when(
      variable %in% sdg_3_health ~ "sdg_3_health",
      variable %in% sdg_4_education ~ "sdg_4_education",
      str_detect(variable, "tax_") ~ "tax_structure",
      .default = "covariate"
    ),
    .after = variable
  )

categ_variables_tidy |>
  distinct(
    variable,
    group
  )

visualize_missing_count_per_year <-
  function(data) {
    data |>
      summarize(
        .by = c(year, variable),
        missing_count = sum(is.na(value))
      ) |>
      ggplot(
        aes(year, missing_count, color = variable)
      ) +
      geom_line(alpha = 0.5, linewidth = 2)
  }

categ_variables_tidy |>
  filter(group == "sdg_3_health") |>
  visualize_missing_count_per_year()

categ_variables_tidy |>
  filter(group == "sdg_4_education") |>
  visualize_missing_count_per_year()

categ_variables_tidy |>
  filter(group == "tax_structure") |>
  visualize_missing_count_per_year()

coverage_pattern <- tax_structure_and_sdg |>
  group_by(year) |>
  summarise(
    pct_missing = round(
      mean(is.na(coverage_of_essential_health_services)) * 100,
      1
    )
  ) |>
  mutate(
    year_type = if_else(year %% 2 == 0, "Even", "Odd")
  )

# PCA --------------------------------------------------------------------
