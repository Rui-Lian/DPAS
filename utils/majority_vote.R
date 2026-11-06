# Background
# Raters may have different opinion on a lesion on a facial site of a patient
# and gives different diagnosis to that lesion-site of a patient
# A majority voting scheme to consolidate the different diagnosis opinions
# if 3 of 4 raters give positive diagnosis, the patients score of that lesion
# on that site is positive. 
# To explore the sensitivity of this majority voting approach
# full set of voting results are created
# score1: assign positive 1 with only 1 positive rater
# score2: assign positive 1 when 2 raters give positive
# So on and so forth to score 4. 
# original column score of each patient, stored as a column list in column score

# BTW: shape of the table
# each patient with four rater of each lesion on each site: 8448 rows
# each patient has ONE majority voting, 2112 rows
majority_vote <- function(data) {
  
  data %>%
    group_by(Patient_id, Position, Lesion) %>%
    summarise(
      Raw_Score = list(Score),
      n_positive = sum(unlist(Score) == 1),
      score1 = as.integer(n_positive >= 1),
      score2 = as.integer(n_positive >= 2),
      score3 = as.integer(n_positive >= 3),
      score4 = as.integer(n_positive == 4),
      .groups = "drop"
    )
}