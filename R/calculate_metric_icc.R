calculate_metric_icc <- function(data) {
  
  metrics <- sort(unique(data$Metric))
  
  icc_results <- purrr::map_dfr(metrics, function(m) {
    
    # Patient × Rater data for this metric
    icc_data <- data |>
      filter(Metric == m) |>
      select(Patient_id, Rater, Total) |>
      pivot_wider(
        names_from = Rater,
        values_from = Total
      ) |>
      select(-Patient_id)
    
    # Calculate ICC
    icc <- psych::ICC(icc_data)$results[
      "Single_random_raters",
    ]
    
    # Extract numeric values
    tibble(
      Metric = m,
      ICC2_1 = icc[["ICC"]],
      F = icc[["F"]],
      df1 = icc[["df1"]],
      df2 = icc[["df2"]],
      P = icc[["p"]],
      Lower = icc[["lower bound"]],
      Upper = icc[["upper bound"]]
    )
  })
  
  icc_results
}