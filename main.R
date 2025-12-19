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


# Analyze missingness ----------------------------------------------------

tax_structure_and_sdg |>
  select(country, year, sdg4_lower_secondary) |>
  mutate(is_missing = is.na(sdg4_lower_secondary)) |>
  ggplot(aes(x = year, y = country, fill = is_missing)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("FALSE" = "#91cf60", "TRUE" = "#d73027"),
    labels = c("Present", "Missing"),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(
    title = "Missingness Pattern: sdg4_lower_secondary",
    x = "Year",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

# Research solution to missingness problem -------------------------------

# PCA --------------------------------------------------------------------

sdg3_fields <- tax_structure_and_sdg |>
  # select(
  #   # contains("sdg3_"),
  #   # sdg3_under_5_mortality_rate,
  #   # sdg3_neonatal_mortality_rate,
  #   # sdg3_maternal_mortality_ratio
  # ) |>
  na.omit()

pca_sdg3 <- prcomp(sdg3_fields, scale. = TRUE)

summary(pca_sdg3)

sdg3_complete$sdg3_index <- pca_result$x[, "PC1"]

# Run PCA on mortality indicators
sdg3_mortality <- tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  )

# Get complete cases with identifiers
sdg3_complete <- sdg3_mortality |> na.omit()

# PCA on just the numeric columns
pca_result <- prcomp(
  sdg3_complete |> select(-country, -year),
  scale. = TRUE
)

# Extract PC1 scores and attach back to country-year
sdg3_complete$sdg3_index <- pca_result$x[, "PC1"]


tax_structure_and_sdg |>
  select(
    matches("^sdg")
  ) |>
  select(
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio,
    sdg4_lower_secondary
  ) |>
  na.omit()

tax_structure_and_sdg |>
  select(
    matches("^sdg3_"),
    matches("^sdg4_")
  ) |>
  na.omit()

# install.packages("openxlsx2")
openxlsx2::write_xlsx(
  tax_structure_and_sdg,
  file = "data/tax_structure_and_sdg.xlsx"
)
