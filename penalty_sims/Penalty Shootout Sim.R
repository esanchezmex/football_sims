# Function: Simulate one penalty
one_pen <- function(){
  prob <- runif(n = 1)
  return(prob)
}

# Function: Turn probability into result
goal_or_no <- function(prob){
  if (prob <= 0.725){
    result = 1
  } else {
    result = 0
  }
  return(result)
}


# Function: Check if a team won
check_winner <- function(team_a_count, team_b_count, total_pens){
  if (team_a_count > team_b_count){
    winner = 1
    return(cat("Team A wins \n") +  cat(c("Team A" = team_a_count, "Team B" = team_b_count, "Total Pens" = total_pens, "\n"))+ winner)
  } else if (team_b_count > team_a_count){
    winner = 1
    return(cat("Team B wins \n") + cat(c("Team A" = team_a_count, "Team B" = team_b_count, "Total Pens" = total_pens, '\n')) + winner)
  } else {
    winner = 0
    return(winner)
  }
}


# Function: In case of tie, sudden death
sudden_death <- function(team_a_count, team_b_count, total_pens){
  while (team_a_count == team_b_count | team_b_count == team_a_count){
    team_a_shot <- goal_or_no(one_pen())
    team_a_count = team_a_count + team_a_shot
    
    team_b_shot <- goal_or_no(one_pen())
    team_b_count = team_b_count + team_b_shot
    
    total_pens = total_pens + 2
  }
  check_winner(team_a_count, team_b_count, total_pens)
}

# Function: Translate results into penalty shootout rules
shootout <- function(){
  team_a_count = 0
  team_b_count = 0
  total_pens = 0
  for (pen_number in 1:5){
    team_a_shot <- goal_or_no(one_pen())
    team_a_count = team_a_count + team_a_shot
    
    team_b_shot <- goal_or_no(one_pen())
    team_b_count = team_b_count + team_b_shot
    
    total_pens = total_pens + 2
  }
  winner = check_winner(team_a_count, team_b_count, total_pens)[2]
  if ( winner == 1){
    cat("Winner")
  } else {
    sudden_death(team_a_count, team_b_count, total_pens)
  }
}

shootout()
check_winner(2, 3, 4)



###############
# DEV
test_func <- function(){
  team_a_goals = 0
  team_b_goals = 0
  rounds_played = 0
  total_shots = 0
  
  while((rounds_played < 5) || (team_a_goals == team_b_goals)){
    rounds_played <- rounds_played + 1
    
    team_a_scored <- penalty_shot(0.75)
    total_shots = total_shots + 1
    
    team_b_scored <- penalty_shot(0.75)
    total_shots = total_shots + 1
    
    if (team_a_scored) {
      team_a_goals <- team_a_goals + 1
    }
    
    if (team_b_scored) {
      team_b_goals <- team_b_goals + 1
    }
    
    if (abs(team_a_goals - team_b_goals) > (5 - rounds_played)) {
      break
    }
  }
  
  return(c("Team A Goals" = team_a_goals,
           "Team B Goals" = team_b_goals,
           "Total Shots" = total_shots))
}

test_func()





penalty_shot <- function(success_probability) {
  return(runif(1) < success_probability)
}

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
  
  return(c(team_a_goals, team_b_goals, rounds_played, total_shots))
}

simulate_penalty_shootout()



##### Way to have a better estimate of percentages for each player
kane <- fb_player_match_logs("https://fbref.com/en/players/21a66f6a/Harry-Kane", season_end_year = "2023", 
                             stat_type = "summary")



pens <- kane %>% filter(Comp == "Premier League") %>% select(PK_Performance, PKatt_Performance)
# PERCENTAGE OF PENS SCORED
sum(pens$PK_Performance) / sum(pens$PKatt_Performance)
# use this number prob in the penalthy shot function

fn = ecdf(gls_vec$Gls_Performance)
fn(gls_vec$Gls_Performance)