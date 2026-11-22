calculate_lesion_icc <- function(df) {
  
  lesion <- sort(unique(df$Lesion))
  
  icc_results <- purrr::map_dfr(lesion, function(m) {
    
    # Patient × Rater data for this lesion
    icc_data <- df |>
      filter(Lesion == m) |>
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
      stop(paste("Missing scores for lesion:", m))
    }
    
    # Calculate ICC
    icc <- psych::ICC(icc_data)$results
    
    icc <- icc["Single_random_raters", ]
    
    tibble(
      Lesion = m,
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
