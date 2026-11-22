
calculate_metric_icc <- function(df) {
  
  metrics <- sort(unique(df$Metric))
  
  icc_results <- purrr::map_dfr(metrics, function(m) {
    
    # Patient × Rater data for this metric
    icc_data <- df |>
      filter(Metric == m) |>
      select(Patient_id, Rater, Total) |>
      pivot_wider(
        names_from = Rater,
        values_from = Total,
        names_prefix = "Rater_"
      ) |>
      arrange(Patient_id) |>
      select(-Patient_id) |>
      as.data.frame()
    
    # Ensure numeric data
    icc_data[] <- lapply(icc_data, as.numeric)
    
    # Check completeness
    if (anyNA(icc_data)) {
      stop(paste("Missing scores for metric:", m))
    }
    
    # Calculate ICC
    icc <- psych::ICC(icc_data)$results
    
    icc <- icc["Single_random_raters", ]
    
    # Extract results
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

