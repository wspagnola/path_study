#Sheet 8
#Quit Attempts & Quit Rates 6.4.18


adult_panel %>% 
  filter(est_smoker_w1==1) %>%
  count
#16,327 established wave 1 smokers (includes former and current)

adult_panel %>% 
  filter(est_smoker_w1==1) %>%
  group_by(attempt_quit_completely) %>% 
  count
#Way off (1459 vs. 2051)

adult_panel %>% 
  filter(est_smoker_w1==1) %>%
  group_by(attempt_quit_reduce) %>% 
  count
#Off by one: (2766 vs. 2767)

adult_panel %>% 
  filter(est_smoker_w1==1) %>%
  group_by(attempt_reduce) %>% 
  count
#Off by 13 (1698 vs. 1711 )

adult_panel %>% 
  filter(est_smoker_w1==1) %>%
  group_by(attempt_none) %>% 
  count


adult_panel$cigarette_current_use_w2 %>%  table

adult_panel$cigarette_use_ever_w1 %>%  table
adult_panel$smoke %>%  table

adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  nrow

  count
  unique(adult_panel$R02_AN0120)