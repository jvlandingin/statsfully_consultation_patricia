tar_load(tax_structure_and_sdg_pca)

tax_structure_and_sdg_pca
box::use(
  dplyr[...],
  plm[...],
  broom[...],
  car[...]
)

#' Predominant Income Tax Share
#' - higher share of income taxes in total tax revenue
#'
#' Predominant Consumption Tax Share
#' - higher share of consumption taxes in total tax revenue
#'
#' Balanced Mix of Income and Consumption Tax Share
#' - balanced mix of income and consumption taxes
#'
#' Direct Tax or Income Tax Share
#' - Income and Profits
#'
#' Indirect Tax or Consumption Tax
#' - Goods and Services

ols_sdg3 <-
  lm(
    sdg3_index ~
      tax_goods_and_services + tax_general_consumption + tax_income_and_profits,
    data = tax_structure_and_sdg_pca
  )
summary(ols_sdg3)

fixed_sdg3 <-
  plm(
    sdg3_index ~
      tax_goods_and_services + tax_general_consumption + tax_income_and_profits,
    data = tax_structure_and_sdg_pca,
    index = c("country", "year"),
    model = "within"
  )
summary(fixed_sdg3)

random_sdg3 <-
  plm(
    sdg3_index ~
      tax_goods_and_services + tax_general_consumption + tax_income_and_profits,
    data = tax_structure_and_sdg_pca,
    index = c("country", "year"),
    model = "random"
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


# Categorize tax composition ---------------------------------------------

tar_load(tax_structure_and_sdg_complete)
tax_structure_and_sdg_complete

tax_composition_and_sdg <-
  tax_structure_and_sdg_pca |>
  mutate(
    predominant_tax_composition = case_when(
      tax_general_consumption - tax_income_and_profits >= 10 ~
        "predominant_consumption_tax",
      tax_income_and_profits - tax_general_consumption >= 10 ~
        "predominant_income_tax",
      .default = "balanced_tax_mix"
    )
  ) |>
  select(-matches("^tax_"))
