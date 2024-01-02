# SIMULATION

# Simulating one shot (using a fixed probability for now)
penalty_shot <- function() {
  return(runif(1) < 0.75)  # Fixed probability for now
}

# Simulating a whole shootout 
simulate_penalty_shootout <- function() {
  team_a_goals <- 0
  team_a_shots <- 0
  team_b_goals <- 0
  team_b_shots <- 0
  rounds_played <- 0
  total_shots <- 0
  
  while ((rounds_played < 5) | (team_a_goals == team_b_goals)) {
    rounds_played <- rounds_played + 1
    
    team_a_goals <- team_a_goals + ifelse(penalty_shot(), 1, 0)
    team_a_shots <- team_a_shots + 1
    total_shots <- total_shots + 1
    
    team_b_goals <- team_b_goals + ifelse(penalty_shot(), 1, 0)
    team_b_shots <- team_b_shots + 1
    total_shots <- total_shots + 1
    
    # Sudden death if scores are tied after 5 rounds
    if (rounds_played == 5 & team_a_goals == team_b_goals) {
      while (team_a_goals == team_b_goals) {
        rounds_played <- rounds_played + 1
        
        team_a_goals <- team_a_goals + ifelse(penalty_shot(), 1, 0)
        team_a_shots <- team_a_shots + 1
        total_shots <- total_shots + 1
        
        team_b_goals <- team_b_goals + ifelse(penalty_shot(), 1, 0)
        team_b_shots <- team_b_shots + 1
        total_shots <- total_shots + 1
        
        if (team_a_goals != team_b_goals) {  # Break if a winner is determined
          break
        }
      }
    }
  }
  
  c('Team A Shots' = team_a_shots, 'Team A Goals' = team_a_goals, 'Team B Shots' = team_b_shots, 'Team B Goals' = team_b_goals, 'Rounds Played' = rounds_played, "Total Shots" = total_shots)
}

simulate_penalty_shootout()

nExperiments = 10000
pens = data.frame(t(replicate(nExperiments, simulate_penalty_shootout())))
mean(pens$Team.A.Goals)
mean(pens$Team.A.Shots)
mean(pens$Team.B.Goals)
mean(pens$Team.B.Shots)

