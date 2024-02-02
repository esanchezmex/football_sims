# Penalty SIM Data Collection

# Arg vs Fra WCF

library(worldfootballR)
library(tidyverse)

# Argentina Team 
arg_urls <- fb_player_urls("https://fbref.com/en/squads/f9fddd6e/Argentina-Men-Stats")

arg <- fb_player_season_stats(arg_urls, stat_type = "standard")

# AVERAGE PENALTY xG (Industry Wide)
pen_avg = 0.79

arg_pens <- arg %>%
  group_by(player_name) %>%
  summarise(pens_made = sum(PK, na.rm = TRUE),
            pens_att = sum(PKatt, na.rm = TRUE)) %>%
  mutate(pct = ifelse(pens_att == 0, pen_avg, pens_made / pens_att)) %>%
  mutate(pct = ifelse(pct == 1, pen_avg, pct)) %>%
  mutate(pct = ifelse(pct == 0, pen_avg, pct)) %>% 
  arrange(desc(pct))



# France Team
fra_urls <- fb_player_urls("https://fbref.com/en/squads/b1b36dcd/France-Men-Stats")

fra <- fb_player_season_stats(fra_urls, stat_type = "standard")


# AVERAGE PENALTY xG (Industry Wide)
pen_avg = 0.79

fra_pens <- fra %>%
  group_by(player_name) %>%
  summarise(pens_made = sum(PK, na.rm = TRUE),
            pens_att = sum(PKatt, na.rm = TRUE)) %>%
  mutate(pct = ifelse(pens_att == 0, pen_avg, pens_made / pens_att)) %>%
  mutate(pct = ifelse(pct == 1, pen_avg, pct)) %>%
  mutate(pct = ifelse(pct == 0, pen_avg, pct)) %>% 
  arrange(desc(pct))


# Create data strucutre needed for functions
arg_v_fra <- rbind(arg_pens, fra_pens)


