#' Full join with one-to-one relationship enforcement
#'
#' @param x First data frame
#' @param y Second data frame
#' @return Joined data frame
full_join_one_to_one_no_na <- function(x, y) {
  dplyr::full_join(
    x = x,
    y = y,
    by = dplyr::join_by(country, year),
    relationship = "one-to-one",
    na_matches = "never"
  )
}

#' Integrate all tidy datasets into one panel
#'
#' @param ... Tidy data frames to join (arbitrary number)
#' @return Integrated panel data frame
integrate_panel_data <- function(...) {
  datasets <- list(...)

  # Use Reduce to sequentially join all datasets
  Reduce(
    f = full_join_one_to_one_no_na,
    x = datasets
  )
}
