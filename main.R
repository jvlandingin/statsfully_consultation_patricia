box::use(
  dplyr[...],
  tidyr[...],
  naniar[...],
  visdat[...],
  ggplot2[...],
  stringr[...],
  purrr[...],
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


# Study new data ---------------------------------------------------------

tar_load(tax_structure_and_sdg)
tax_structure_and_sdg |>
  na.omit()
tax_structure_and_sdg |>
  visdat::vis_dat()

# tax_structure_and_sdg |>
#   write.csv("tax_structure_and_sdg.csv")

all_vars <-
  colnames(tax_structure_and_sdg) |>
  setdiff(c("country", "year"))


# Generate multiple samples of different variables selections ------------

valid_combo <- function(x) {
  sum(grepl("^sdg3_", x)) >= 2 &&
    sum(grepl("^sdg4_", x)) >= 2 &&
    sum(grepl("^mod_", x)) >= 2 &&
    sum(grepl("^tax_", x)) >= 2
}
valid_samples <- unlist(
  lapply(8:length(all_vars), function(n) {
    Filter(
      valid_combo,
      combn(all_vars, n, simplify = FALSE)
    )
  }),
  recursive = FALSE
)

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
        select(all_of(sample)) |>
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
  filter(nrow >= 143, cols_retained == 9) |>
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

tax_structure_and_sdg


one_country <-
  tax_structure_and_sdg |>
  select(country, year, tax_income_and_profits) |>
  filter(country == "Azerbaijan")

tax_structure_and_sdg_interpolated <-
  tax_structure_and_sdg |>
  split(tax_structure_and_sdg$country) |>
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
        select(all_of(sample)) |>
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


nrow_per_interp_sample |>
  arrange(desc(nrow))

nrow_per_interp_sample |>
  filter(nrow >= 143, cols_retained == 9) |>
  pull(sample)

nrow_per_interp_sample |>
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

nrow_per_interp_sample |>
  filter(cols_retained == 16)

# Improvement in missingness count

old_missing_n <-
  sum(is.na(tax_structure_and_sdg))
total_n <-
  sum(!is.na(tax_structure_and_sdg)) + old_missing_n

new_missing_n <-
  sum(is.na(tax_structure_and_sdg_interpolated))
new_missing_n / old_missing_n - 1
old_missing_n + (-0.08561644 * old_missing_n)

old_missing_n / total_n

new_missing_n / total_n
