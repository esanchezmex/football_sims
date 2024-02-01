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
  
  c('Team A Shots' = team_a_shots,
    'Team A Goals' = team_a_goals,
    'Team B Shots' = team_b_shots,
    'Team B Goals' = team_b_goals,
    'Rounds Played' = rounds_played,
    "Total Shots" = total_shots)
}

simulate_penalty_shootout()

nExperiments = 10000
pens = data.frame(t(replicate(nExperiments, simulate_penalty_shootout())))
mean(pens$Team.A.Goals)
mean(pens$Team.A.Shots)
mean(pens$Team.B.Goals)
mean(pens$Team.B.Shots)
mean(pens$Total.Shots)



################################################################
    #     IMPLEMENTATION WITH REAL LIFE VALUES      #
################################################################

penalty_shot <- function(player_name, players_df) {
  success_probability <- players_df$pct[players_df$player_name == player_name]
  return(runif(1) < success_probability)
}

# Simulate a penalty shootout
simulate_penalty_shootout <- function(team_a_players, team_b_players, players_df) {
  team_a_goals <- 0
  team_b_goals <- 0
  team_a_shots <- 0
  team_b_shots <- 0
  rounds_played <- 0
  
  while (rounds_played < 5 || team_a_goals == team_b_goals) {
    rounds_played <- rounds_played + 1
    player_index_a <- ((rounds_played - 1) %% length(team_a_players)) + 1
    player_index_b <- ((rounds_played - 1) %% length(team_b_players)) + 1
    
    if (player_index_a <= length(team_a_players)) {
      team_a_goals <- team_a_goals + ifelse(penalty_shot(team_a_players[player_index_a], players_df), 1, 0)
      team_a_shots <- team_a_shots + 1
      
      if (rounds_played >= 5 && team_a_goals - team_b_goals > 5 - rounds_played) {
        break
      }
    }
    
    if (player_index_b <= length(team_b_players)) {
      team_b_goals <- team_b_goals + ifelse(penalty_shot(team_b_players[player_index_b], players_df), 1, 0)
      team_b_shots <- team_b_shots + 1
      
      if (rounds_played >= 5 && team_b_goals - team_a_goals > 5 - rounds_played) {
        break
      }
    }
  }
  
  total_shots <- team_a_shots + team_b_shots
  
  c(
    'Team A Goals' = team_a_goals,
    'Team B Goals' = team_b_goals,
    'Team A Shots' = team_a_shots,
    'Team B Shots' = team_b_shots,
    'Total Shots' = total_shots,
    'Rounds Played' = rounds_played
  )
}



simulate_penalty_shootout(team_a_players, team_b_players, arg_v_fra)

nExperiments = 10000
pens = data.frame(t(replicate(nExperiments, simulate_penalty_shootout(team_a_players, team_b_players, arg_v_fra))))
mean(pens$Team.A.Goals)
mean(pens$Team.A.Shots)
mean(pens$Team.B.Goals)
mean(pens$Team.B.Shots)
mean(pens$Total.Shots)


