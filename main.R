box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  targets[...]
)

tar_load(tax_structure_and_sdg)

# Research solution to missingness problem -------------------------------

glimpse(tax_structure_and_sdg)

# PCA --------------------------------------------------------------------

sdg3_fields <- tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg3_under_5_mortality_rate,
    sdg3_neonatal_mortality_rate,
    sdg3_maternal_mortality_ratio
  ) |>
  na.omit()

pca_sdg3 <- prcomp(sdg3_fields, scale. = TRUE)

summary(pca_sdg3)

sdg3_fields$sdg3_index <- pca_sdg3$x[, "PC1"]

# install.packages("openxlsx2")
# openxlsx2::write_xlsx(
#   tax_structure_and_sdg,
#   file = "data/tax_structure_and_sdg.xlsx"
# )

sdg4_fields <-
  tax_structure_and_sdg |>
  select(
    country,
    year,
    sdg4_lower_secondary,
    sdg4_prop_trained_teachers_primary,
    sdg4_prop_trained_teachers_pre_primary
  ) |>
  na.omit()

sdg3_and_sdg4 <-
  sdg4_fields |>
  inner_join(
    sdg3_fields,
    by = join_by(country, year)
  )

pca_sdg4 <-
  sdg3_and_sdg4 |>
  select(
    contains("sdg4_")
  ) |>
  prcomp(scale. = TRUE)

summary(pca_sdg4)

sdg_indices <-
  sdg3_and_sdg4 |>
  mutate(
    sdg4_index = pca_sdg4$x[, "PC1"]
  ) |>
  select(
    country,
    year,
    sdg3_index,
    sdg4_index
  )

# Get tax and moderating variables
sdg_idx_mod_and_tax <-
  sdg_indices |>
  inner_join(
    tax_structure_and_sdg |>
      select(
        country,
        year,
        contains("mod_"),
        contains("tax_")
      ),
    by = join_by(country, year)
  )

sdg_idx_mod_and_tax |>
  vis_dat()
