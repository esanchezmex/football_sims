# Application
#     Create ideal lineup for each team that maximizes chances of winning

# Wrap code in function for ease of use
sim_pens <- function(team_a_players, team_b_players, df, nExperiments = 5000){
  nExperiments = nExperiments
  pens = data.frame(t(replicate(nExperiments, simulate_penalty_shootout(team_a_players, team_b_players, df))))
  
  # Choose team A stats
  c("Goals" = mean(pens$Team.A.Goals), "Shots" = mean(pens$Team.A.Shots))
}