# Visualizing Distributions for each team
library(patchwork)

# Histogram for Team A (condition of less than 15 goals since otherwise extremely rare)
team_a_hist <- ggplot(pens[pens$Team.A.Goals < 15, ], aes(x = Team.A.Goals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Distribution of Goals for Team A (Argentina)") +
  xlab("Goals Scored") +
  ylab("Frequency")

# Histogram for Team B
team_b_hist <- ggplot(pens[pens$Team.B.Goals < 15, ], aes(x = Team.B.Goals)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  ggtitle("Distribution of Goals for Team B (France)") +
  xlab("Goals Scored") +
  ylab("Frequency")


goal_distributions <- team_a_hist / team_b_hist

ggsave("goal_distributions.png", goal_distributions)


# Statistical Tests to assess likelihood of observed goal difference occurring by chance
# Compare mean of both teams
#   Checking assumptions
#       Normality
shapiro.test(pens$Team.A.Goals) 
shapiro.test(pens$Team.B.Goals) 
# neither is normally distributed, opt for a non-parametric test

# Mann-Whitney U Test
mw_test_result <- wilcox.test(pens$Team.A.Goals, pens$Team.B.Goals)
mw_test_result
# small p-value suggests there is a difference in the median values between the two means




