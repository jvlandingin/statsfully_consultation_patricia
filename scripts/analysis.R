# Overview ---------------------------------------------------------------

#' Our goal here is to determine if there really is no relationship between
#' tax and SDG3

# Libraries --------------------------------------------------------------

library(targets)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Fetch Data -------------------------------------------------------------
tar_load(tax_structure_and_sdg_complete)
tar_load(tax_composition_and_sdg)

# Analysis ---------------------------------------------------------------

tax_structure_and_sdg3 <-
  tax_structure_and_sdg_complete |>
  select(
    country,
    year,
    contains("sdg3_")
  ) |>
  left_join(
    tax_composition_and_sdg |> select(country, year, tax_composition),
    by = join_by(country, year),
    na_matches = "never",
    relationship = "one-to-one"
  ) |>
  mutate(
    tax_composition = as.factor(tax_composition)
  )


tax_structure_and_sdg3 |> head() |> as.data.frame()

tax_structure_and_sdg3 |>
  select(country, year, tax_composition, sdg3_coverage_essential_health) |>
  ggplot(aes(year, y = sdg3_coverage_essential_health, color = country)) +
  geom_line() +
  facet_wrap(. ~ tax_composition, scales = "fixed")

tax_structure_and_sdg3 |>
  ggplot(aes(
    x = tax_composition,
    y = sdg3_coverage_essential_health,
    fill = tax_composition
  )) +
  geom_boxplot() +
  labs(x = "Tax Composition", y = "Coverage of Essential Health Services")

tax_structure_and_sdg3 |>
  summarise(
    mean_val = mean(sdg3_coverage_essential_health, na.rm = TRUE),
    .by = c(year, tax_composition)
  ) |>
  ggplot(aes(x = year, y = mean_val, color = tax_composition)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    y = "Mean Coverage of Essential Health Services",
    color = "Tax Composition"
  )

tax_structure_and_sdg3 |>
  ggplot(aes(x = year, y = sdg3_coverage_essential_health)) +
  geom_line(aes(group = country), alpha = 0.15) +
  stat_summary(
    aes(color = tax_composition),
    fun = mean,
    geom = "line",
    linewidth = 1.2
  ) +
  facet_wrap(~tax_composition)


tax_structure_and_sdg3 |>
  pivot_longer(
    cols = starts_with("sdg3_"),
    names_to = "indicator",
    values_to = "value"
  ) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    .by = c(year, tax_composition, indicator)
  ) |>
  ggplot(aes(x = year, y = mean_val, color = tax_composition)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~indicator, scales = "free_y") +
  labs(y = "Group Mean", color = "Tax Composition")


### SDG4 ###

tax_structure_and_sdg4 <-
  tax_structure_and_sdg_complete |>
  select(
    country,
    year,
    contains("sdg4_")
  ) |>
  left_join(
    tax_composition_and_sdg |> select(country, year, tax_composition),
    by = join_by(country, year),
    na_matches = "never",
    relationship = "one-to-one"
  ) |>
  mutate(
    tax_composition = as.factor(tax_composition)
  )

tax_structure_and_sdg4 |>
  pivot_longer(
    cols = starts_with("sdg4_"),
    names_to = "indicator",
    values_to = "value"
  ) |>
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    .by = c(year, tax_composition, indicator)
  ) |>
  ggplot(aes(x = year, y = mean_val, color = tax_composition)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1) +
  facet_wrap(~indicator, scales = "free_y") +
  labs(y = "Group Mean", color = "Tax Composition")


tax_composition_and_sdg |>
  pivot_longer(
    cols = ends_with("_index"),
    names_to = "sdg_index",
    values_to = "value"
  ) |>
  summarize(
    .by = c(year, tax_composition, sdg_index),
    value = mean(value)
  ) |>
  ggplot(
    aes(year, value, color = tax_composition)
  ) +
  geom_line() +
  facet_wrap(~sdg_index)
