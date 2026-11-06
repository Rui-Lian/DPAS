# Function: 
# Plot_score_sex_posion_lesion
# Lesion score from majority vote: score1, score2, score3 as argument
# the plot title should paste the colume name score3, score2, score1 with the rest characters.
plot_score_sex_position_lesion <- function(data, score_col) {
  
  # ensym() captures the argument as a symbol without evaluating it.
  # In R, a symbol is an object that represents a name 
  # (such as a variable or column name), 
  # rather than the value stored under that name.
  # as_name() converts that symbol into a character string.
  score_name <- rlang::as_name(rlang::ensym(score_col))
  
  
  score_sex_position_lesion <- data |>
    group_by(Sex, Position, Lesion) |>
    summarise(
      n = sum({{ score_col }}, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(pct = n / 24) |>
    filter(Lesion != "皮肤萎缩")
  
  g <- ggplot(
    score_sex_position_lesion,
    aes(x = forcats::fct_rev(Lesion), y = pct, fill = Sex)
  ) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = scales::percent(pct, accuracy = 1)),
      position = position_dodge(width = 0.9),
      hjust = -0.1,
      size = 2.5
    ) +
    scale_fill_discrete_qualitative(palette = "Harmonic") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.4),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_cartesian(ylim = c(0, 5)) + 
    labs(
      title = paste0(
        "不同光老化皮损类型在男女四个部位的发生率 (",
        score_name, ")"
      ),
      x = "光老化类型",
      y = "发生率"
    ) +
    coord_flip() +
    facet_grid(. ~ Position) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_blank()
    )
  
  return(g)
}

plot_score_sex_position_lesion(data = dat_final, score_col = score1)
