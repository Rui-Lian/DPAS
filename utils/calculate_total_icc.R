library(tidyverse)

calculate_total_icc <- function(df) {
  
  stopifnot(is.data.frame(df))
  
  # Convert from long format:
  # one row = one patient-rater combination
  icc_data <- df |>
    select(Patient_id, Rater, Total) |>
    pivot_wider(
      names_from = Rater,
      values_from = Total
    ) |>
    ungroup() |>
    select(-Patient_id)
  
  # Calculate ICC
  psych::ICC(icc_data)
}

