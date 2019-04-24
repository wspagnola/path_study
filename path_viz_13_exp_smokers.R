source('source.R')


options(row.names = F)
adult_panel$cig_num_life_w1
#Explore Experiment Smokers



#### Cross Sectional ####
adult_panel %>% 
    filter(smoking_status_full_w1 ==  "current_exp_smoker") %>% 
    count(age_w1) 

adult_panel %>% 
  filter(smoking_status_full_w2 ==  "current_exp_smoker") %>% 
  count(age_w2)

adult_panel %>% 
  filter(smoking_status_full_w3 ==  "current_exp_smoker") %>% 
  count(age_w3)





#### Longitudinal Current Exp Smokers ####

cur_exp_w1_w2 <-adult_panel %>% 
                  filter(current_exp_smoker_w1 == 1,
                         current_exp_smoker_w2 == 1,
                         current_exp_smoker_w3 == 1) %>%  
                  count(age_w1, age_w2) %>% 
                  rename(before = age_w1,
                         after = age_w2 ,
                         flow = n) %>%
                  mutate(step_from = 'wave 1',
                         step_to = 'wave 2',
                         before =  paste('w1', before, sep = '_'),
                         after = paste('w2', after, sep = '_'))  


cur_exp_w2_w3 <- adult_panel %>% 
                      filter(current_exp_smoker_w1 == 1,
                             current_exp_smoker_w2 == 1,
                             current_exp_smoker_w3 == 1) %>%  
                      count(age_w2, age_w3) %>% 
                      rename(before = age_w2,
                             after =  age_w3,
                             flow = n) %>% 
                      mutate(step_from = 'wave 2',
                             step_to = 'wave 3',
                             before = paste('w2', before, sep = '_'),
                             after = paste('w3', after, sep = '_'))
sankey_df <- rbind(cur_exp_w1_w2, cur_exp_w2_w3 )  
sankey_df <- sankey_df %>% 
  mutate(before = str_replace_all(before, 'NA', 'missing'),
         after = str_replace_all(after, 'NA', 'missing')) 
write.csv(sankey_df, 'Output/sankey_cur_exp_smokers_age_df.csv', row.names = F)


#### Cigarette Lifetime Use (Longitudinal) ###


adult_panel %>% 
    mutate(non_response_w1 = if_else(is.na(cig_num_life_w1), 1, 0),
         non_response_w2 = if_else(is.na(cig_num_life_w2) & 
                                     as.numeric(adult_panel$cig_num_life_w1 != 6), 1, 0),
         non_response_w3 = if_else(is.na(cig_num_life_w3) & 
                                      as.numeric(adult_panel$cig_num_life_w2) != 6 &
                                      as.numeric(adult_panel$cig_num_life_w3) != 6 , 1, 0)) %>% 
    select(cig_num_life_w1, cig_num_life_w2, cig_num)
                                  
sankey_cig_use_life <- plot_sankey(df =adult_panel, 
                                w1 = cig_num_life_w1, 
                                w2 = cig_num_life_w2, 
                                w3 = cig_num_life_w3,
                                sep = ' ',
                                useNA = F)

View(sankey_cig_use_life)
write.csv(sankey_cig_use_life, 
          file = 'Output/sankey_cur_exp_smokers_cig_use_life_df.csv',
          row.names = F)
