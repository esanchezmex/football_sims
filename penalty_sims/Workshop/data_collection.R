# Penalty SIM Data Collection

# Arg vs Fra WCF

library(worldfootballR)
library(dplyr)
library(gt)

# Argentina Team 
arg_urls <- fb_player_urls("https://fbref.com/en/squads/f9fddd6e/Argentina-Men-Stats")

arg <- fb_player_season_stats(arg_urls, stat_type = "standard")

# FINDING AVERAGE TO MAKE DEFAULT VAL
arg_avg <- arg %>% 
  group_by(player_name) %>% 
  summarise(pens_made = sum(PK, na.rm = T),
            pens_att = sum(PKatt, na.rm = T)) %>% 
  filter(pens_att != 0 | pens_made != 0) %>% 
  mutate(pct = pens_made / pens_att) %>% 
  pull(pct) %>% 
  mean()

arg_pens <- arg %>% 
  group_by(player_name) %>% 
  summarise(pens_made = sum(PK, na.rm = T),
            pens_att = sum(PKatt, na.rm = T)) %>%
  filter(pens_att != 0 | pens_made != 0) %>% 
  mutate(pct = pens_made / pens_att) %>% 
  # ASK FOR DIFFERENT WAYS TO HANDLE SMALL SAMPLE SIZES, CONTINUE WITH ONE AND SAY WELCOME TO TRY OUT YOURSELF
  mutate(pct = ifelse(pct == 1, arg_avg, pct)) %>% #method to handle small sample sizes
  select(player_name, pct) %>% 
  arrange(desc(pct))



# France Team
fra_urls <- fb_player_urls("https://fbref.com/en/squads/b1b36dcd/France-Men-Stats")

fra <- fb_player_season_stats(fra_urls, stat_type = "standard")

# FINDING AVERAGE TO MAKE DEFAULT VAL
fra_avg <- fra %>% 
  group_by(player_name) %>% 
  summarise(pens_made = sum(PK, na.rm = T),
            pens_att = sum(PKatt, na.rm = T)) %>% 
  filter(pens_att != 0 | pens_made != 0) %>% 
  mutate(pct = pens_made / pens_att) %>% 
  pull(pct) %>% 
  mean()

fra_pens <- fra %>% 
  group_by(player_name) %>% 
  summarise(pens_made = sum(PK, na.rm = T),
            pens_att = sum(PKatt, na.rm = T)) %>%
  filter(pens_att != 0 | pens_made != 0) %>% 
  mutate(pct = pens_made / pens_att) %>% 
  # ASK FOR DIFFERENT WAYS TO HANDLE SMALL SAMPLE SIZES, CONTINUE WITH ONE AND SAY WELCOME TO TRY OUT YOURSELF
  mutate(pct = ifelse(pct == 1, fra_avg, pct)) %>% #method to handle small sample sizes
  select(player_name, pct)



# check lineups for players to create order for penalties
wc_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")
lineups <- fb_match_lineups(match_url = wc_urls[length(wc_urls)])
lineups <- as_tibble(lineups)


# QUESTION: What is the ideal lineup for each team based on their percentages







