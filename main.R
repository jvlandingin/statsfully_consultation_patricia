box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  purrr[...],
  targets[...],
  constructive[...]
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


# Filter table -----------------------------------------------------------

tax_structure_and_sdg_filtered <-
  tax_structure_and_sdg |>
  filter(
    !country %in% c("Cook Island", "Fiji")
  )

# Study new data ---------------------------------------------------------

tar_load(tax_structure_and_sdg)

# We will choose among these SDG variables which ones to keep
all_vars <-
  colnames(tax_structure_and_sdg) |>
  stringr::str_subset(pattern = "^sdg")

# Generate multiple samples of different variables selections ------------

valid_combo <- function(x) {
  sum(grepl("^sdg3_", x)) >= 5 &&
    sum(grepl("^sdg4_", x)) >= 5
}
valid_samples <- unlist(
  lapply(10, function(n) {
    Filter(
      valid_combo,
      combn(all_vars, n, simplify = FALSE)
    )
  }),
  recursive = FALSE
)

valid_samples <-
  purrr::map(
    .x = 10, # Set number of variables to retains to test
    .f = function(n) {
      Filter(
        valid_combo,
        combn(all_vars, n, simplify = FALSE)
      )
    },
    .progress = TRUE
  ) |>
  unlist(recursive = FALSE)

# distribution of subset sizes
table(lengths(valid_samples))

# verify constraint
all(sapply(valid_samples, valid_combo))

# Check the number of columns retained depending on which variables we keep

remaining_rows_per_comb <-
  map_dbl(
    .x = valid_samples,
    .f = function(sample) {
      tax_structure_and_sdg |>
        select(
          country,
          year,
          all_of(sample),
          contains("tax_"),
          contains("mod_")
        ) |>
        na.omit() |>
        nrow()
    },
    .progress = TRUE
  )

# Analyze
nrow_per_sample <-
  tibble(
    sample = valid_samples,
    nrow = remaining_rows_per_comb,
    total_rows = 290
  ) |>
  mutate(
    cols_retained = map_dbl(.x = sample, .f = length),
    rows_ratained_pct = nrow / total_rows
  )

nrow_per_sample |>
  arrange(desc(nrow))

nrow_per_sample |>
  filter(nrow >= 143) |>
  pull(sample)

nrow_per_sample |>
  filter(cols_retained == 16)

nrow_per_sample |>
  ggplot(aes(
    x = factor(cols_retained, levels = rev(levels(factor(cols_retained)))),
    y = rows_ratained_pct,
    group = factor(cols_retained),
    fill = factor(cols_retained)
  )) +
  scale_y_continuous(labels = scales::percent) +
  geom_boxplot() +
  stat_summary(
    fun = max,
    geom = "text",
    aes(label = scales::percent(after_stat(y), accuracy = 0.1)),
    vjust = -0.5,
    size = 3
  ) +
  xlab("Number of fields to retain") +
  ylab("% of records retained") +
  guides(fill = "none") +
  theme_minimal()


# Attempt interpolation --------------------------------------------------

tax_structure_and_sdg_interpolated |>
  select(country, year, matches("^tax_"), matches("^mod_")) |>
  na.omit() |>
  count(country, sort = TRUE)

tax_structure_and_sdg_interpolated <-
  tax_structure_and_sdg_filtered |>
  split(tax_structure_and_sdg_filtered$country) |>
  map(
    .f = function(data) {
      data |>
        mutate(
          across(
            .cols = matches("sdg3_|sdg4_|tax_|mod_"),
            .fns = ~ zoo::na.approx(., na.rm = FALSE)
          )
        )
    }
  ) |>
  list_rbind()

remaining_rows_per_comb_interp <-
  map_dbl(
    .x = valid_samples,
    .f = function(sample) {
      tax_structure_and_sdg_interpolated |>
        select(
          country,
          year,
          all_of(sample),
          contains("tax_"),
          contains("mod_")
        ) |>
        na.omit() |>
        nrow()
    },
    .progress = TRUE
  )

# Analyze
nrow_per_interp_sample <-
  tibble(
    sample = valid_samples,
    nrow = remaining_rows_per_comb_interp,
    total_rows = 290
  ) |>
  mutate(
    cols_retained = map_dbl(.x = sample, .f = length),
    rows_ratained_pct = nrow / total_rows
  )


best_sample <-
  nrow_per_interp_sample |>
  slice_max(n = 1, order_by = nrow)

best_sample

best_sample |>
  pull(sample) |>
  _[[1]] |>
  unlist()

tax_structure_and_sdg_interpolated |>
  select(country, year, all_of(best_sample$sample[[1]])) |>
  na.omit() |>
  count(year)

tax_structure_and_sdg_interpolated |>
  select(country, year, all_of(best_sample$sample[[1]])) |>
  naniar::miss_var_summary()

tax_structure_and_sdg_interpolated |>
  select(country, year, all_of(best_sample$sample[[1]])) |>
  naniar::miss_summary()

?naniar::miss_var_prop
?naniar::miss_case_prop


# What if we extrapolate only the tax variables --------------------------

tax_structure_and_sdg_interpolated_extrapolated <-
  tax_structure_and_sdg_filtered |>
  split(tax_structure_and_sdg_filtered$country) |>
  map(
    .f = function(data) {
      data |>
        mutate(
          across(
            .cols = matches("sdg3_|sdg4_|tax_|mod_"),
            .fns = ~ zoo::na.approx(., na.rm = FALSE)
          ),
          across(
            .cols = matches("tax_"),
            .fns = ~ zoo::na.approx(., na.rm = FALSE, rule = 2)
          )
        )
    }
  ) |>
  list_rbind()

remaining_rows_per_comb_extrap <-
  map_dbl(
    .x = valid_samples,
    .f = function(sample) {
      tax_structure_and_sdg_interpolated_extrapolated |>
        select(
          country,
          year,
          all_of(sample),
          contains("tax_"),
          contains("mod_")
        ) |>
        na.omit() |>
        nrow()
    },
    .progress = TRUE
  )

# Analyze
nrow_per_extrap_sample <-
  tibble(
    sample = valid_samples,
    nrow = remaining_rows_per_comb_extrap,
    total_rows = 290
  ) |>
  mutate(
    cols_retained = map_dbl(.x = sample, .f = length),
    rows_ratained_pct = nrow / total_rows
  )


best_sample_extrap <-
  nrow_per_extrap_sample |>
  slice_max(n = 1, order_by = nrow)

best_sample_extrap$sample[[1]]

tax_structure_and_sdg_complete <-
  tax_structure_and_sdg_interpolated_extrapolated |>
  select(country, year, all_of(best_sample_extrap$sample[[1]])) |>
  na.omit()

tax_structure_and_sdg_complete |>
  count(year)

tax_structure_and_sdg_complete |>
  count(country)
