# STATISTICAL TESTS FOR FOOTBALL - ONE SAMPLE - SMALL SAMPLE

rodri <- fb_player_match_logs("https://fbref.com/en/players/6434f10d/Rodri", stat_type = "summary", season_end_year = 2024)

rodri %>% 
  filter(Comp == "Premier League") %>%
  select(Cmp_Passes) %>% 
  t.test(conf.level = 0.99, alternative = 'less', mu = 110)


n = rodri %>% filter(Comp == "Premier League") %>% select(Cmp_Passes) %>% count()
n = n[1,1]

s <- rodri %>% filter(Comp == "Premier League") %>% select(Cmp_Passes)
s <- sd(s$Cmp_Passes)

sample_mean <- rodri %>% filter(Comp == "Premier League") %>% select(Cmp_Passes)
sample_mean <-mean(sample_mean$Cmp_Passes)

critical_value <- qt(p = 0.80, df = n - 1)

standard_error <- s/sqrt(n)

lower_bound = sample_mean - critical_value*standard_error
upper_bound = sample_mean + critical_value*standard_error

CI = c('lower_bound' = lower_bound, 'upper_bound' = upper_bound)
CI
