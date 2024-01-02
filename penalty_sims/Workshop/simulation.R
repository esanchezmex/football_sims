# SIMULATION

# Simulating one shot
penalty_shot <- function(success_probability) {
  return(runif(1) < success_probability)
}

# Simulating a whole shootout
#   RULES: Best of 5, if one team has an insurmountable lead, the shootout ends
#           If teams are tied after 5 rounds, they go into sudden death
simulate_penalty_shootout <- function() {
  team_a_goals <- 0
  team_b_goals <- 0
  rounds_played <- 0
  total_shots <- 0
  
  while (((rounds_played < 5) | 
          (team_a_goals == team_b_goals)) &
         (abs(team_a_goals - team_b_goals) <= (5 - rounds_played))
  ) {
    rounds_played <- rounds_played + 1
    
    team_a_scored <- penalty_shot(0.75)
    total_shots <- total_shots + 1
    team_b_scored <- penalty_shot(0.75)
    total_shots <- total_shots + 1
    
    if (team_a_scored) {
      team_a_goals <- team_a_goals + 1
    } else if (team_b_scored) {
      team_b_goals <- team_b_goals + 1
    }
  }
  
  c('Team A Goals' = team_a_goals, 'Team B Goals' = team_b_goals, 'Rounds Played' = rounds_played,  "Total Shots" = total_shots)
}

simulate_penalty_shootout()

nExperiments = 1000
pens = data.frame(t(replicate(nExperiments, simulate_penalty_shootout())))
mean(pens$Team.A.Goals)
mean(pens$Team.B.Goals)