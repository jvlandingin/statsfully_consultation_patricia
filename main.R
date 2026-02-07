tar_load(tax_composition_sdg_idx_scaled_mod)

tax_composition_sdg_idx_scaled_mod
box::use(
  dplyr[...],
  plm[...],
  broom[...],
  car[...]
)

panel_data <-
  tax_composition_sdg_idx_scaled_mod

# Descriptive
panel_data |>
  count(country) |>
  rename(count_of_years = n)

# Define formula ONCE for consistency
sdg3_formula <- sdg3_index ~
  tax_composition +
  mod_macroeconomic_population +
  mod_macroeconomic_population:tax_composition +
  mod_gdp_per_capita +
  mod_gdp_per_capita:tax_composition +
  mod_debt_to_gdp_ratio +
  mod_debt_to_gdp_ratio:tax_composition

ols_sdg3 <-
  lm(
    sdg3_formula,
    data = panel_data
  )
summary(ols_sdg3)


fixed_sdg3 <-
  plm(
    sdg3_formula,
    data = panel_data,
    index = c("country", "year"),
    model = "within"
  )
summary(fixed_sdg3)

# random_sdg3 <-
#   plm(
#     sdg3_formula,
#     data = panel_data,
#     index = c("country", "year"),
#     model = "random"
#   )
# summary(random_sdg3)

random_sdg3 <- plm(
  sdg3_formula,
  data = panel_data,
  index = c("country", "year"),
  model = "random",
  random.method = "walhus" # or "amemiya", "nerlove"
)
summary(random_sdg3)


# Hausman Test
hausman_test_p <-
  phtest(fixed_sdg3, random_sdg3) |> tidy()
if (hausman_test_p$p.value < 0.05) {
  chosen_sdg3_mod <- fixed_sdg3
} else {
  chosen_sdg3_mod <- random_sdg3
}

# Wooldridge Test
pbgtest(chosen_sdg3_mod)

# Breusch-Pagan test
plmtest(chosen_sdg3_mod, c("time"), type = ("bp"))

# Pesaran CD test for cross-sectional dependence in panels
pcdtest(chosen_sdg3_mod, test = c("cd"))

# For those with more than one predictors
car::vif(chosen_sdg3_mod)

# Chow test or F-test
pFtest(fixed_sdg3, ols_sdg3)


# check data combinatinos ------------------------------------------------

tar_load(pca_quality_per_sdg_combination)

glimpse(pca_quality_per_sdg_combination)

pca_quality_per_sdg_combination |>
  arrange(desc(n_countries))

pca_quality_per_sdg_combination |>
  arrange(desc(n_years))

pca_quality_per_sdg_combination_simple <-
  pca_quality_per_sdg_combination |>
  filter(
    sdg3_all_cors_positive,
    sdg4_all_cors_positive
  ) |>
  select(
    nrow,
    n_countries,
    n_years,
    min_year,
    max_year,
    sdg3_pc1_var_explained,
    sdg4_pc1_var_explained
  )

pca_quality_per_sdg_combination_simple |>
  arrange(desc(n_years))
