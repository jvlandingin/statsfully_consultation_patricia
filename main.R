box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  targets[...],
  constructive[...],
  psych[KMO, cortest.bartlett]
)

tar_load(tax_structure_and_sdg)
tar_load(sdg3_index)
tar_load(sdg4_index)

analysis_data <-
  tax_structure_and_sdg |>
  left_join(
    sdg3_index,
    by = join_by(country, year)
  ) |>
  left_join(
    sdg4_index,
    by = join_by(country, year)
  ) |>
  select(
    country,
    year,
    sdg3_index,
    sdg4_index,
    contains("mod_"),
    contains("tax_")
  )

#' Note on missingness in tax variables and moderating variables.
#' I noticed that for tax variables, most of the missingness is in the year
#' 2014. While for moderating variables, all years are present for all
#' countries. However, some countries do not have data at all.

#' We need to understand how many data points we have if we omit rows with NAs

analysis_data |>
  na.omit()

#' We only get 92 rows out of 290 which is a lot

analysis_data |>
  vis_dat()
