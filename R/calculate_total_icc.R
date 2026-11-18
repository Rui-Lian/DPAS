calculate_total_icc <- function(data) {
  
  # Convert from long format:
  # one row = one patient-rater combination
  icc_data <- data |>
    select(Patient_id, Rater, Total) |>
    pivot_wider(
      names_from = Rater,
      values_from = Total
    ) |>
    select(-Patient_id)
  
  # Calculate ICC
  ICC(icc_data)
}
