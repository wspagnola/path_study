#13 Smoking Status W1-W3

#Smoking Trajectories Waves 1-3 among ALL (20,208 Established & NonEstablished Smokers at Wave 1)


#NOTE: Current Established & Non-established does not include every one who stated that they smoked
#Some people indicated that they smoked but because they did not answer one or more of the
#lifetime cigarette number or current use questions they are labelled as NA because it can't
#be determined which group they belong to


##### SUBSET SMOKING GROUPS ####
w1_smokers <- adult_panel %>% filter(current_est_smoker_w1 ==1 | current_non_est_smoker_w1 ==1) 
w1_cur_est_smokers <- adult_panel %>%  filter(current_est_smoker_w1 ==1)
w1_cur_non_est_smokers <- adult_panel %>%  filter(current_non_est_smoker_w1 ==1)
w2_cur_est_smokers <- adult_panel %>%  filter(current_est_smoker_w2 ==1)

#### WAVE 1: Smokers ####

##Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_smokers %>% select(smoking_status_w2) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_smokers %>%  select(cigarette_current_freq_w2)  %>% table

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w1_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w1_smokers %>%  select(cigarette_current_freq_w3) %>% table


#### WAVE 2: Smokers ####
w2_smokers <- adult_panel %>% 
  filter(current_est_smoker_w2 ==1 | current_non_est_smoker_w2 ==1) 

##Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_smokers %>% select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_smokers %>%  select(cigarette_current_freq_w3)  %>% table

rm(w2_smokers)  


#### WAVE 1: CURRENT ESTABLISHED SMOKERS ####

##Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_cur_est_smokers %>% select(smoking_status_w2) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_cur_est_smokers %>%  select(cigarette_current_freq_w2)  %>% table

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w1_cur_est_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w1_cur_est_smokers %>%  select(cigarette_current_freq_w3) %>% table

#### WAVE 2: CURRENT ESTABLISHED SMOKERS ####
w2_cur_est_smokers <- adult_panel %>%  filter(current_est_smoker_w2 ==1)

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w2_cur_est_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_cur_est_smokers %>%  select(cigarette_current_freq_w3) %>% table



#### WAVE 1: CURRENT NON-ESTABLISHED SMOKERS ####

##Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_cur_non_est_smokers %>% select(smoking_status_w2) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_cur_non_est_smokers %>%  select(cigarette_current_freq_w2)  %>% table

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w1_cur_non_est_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w1_cur_non_est_smokers %>%  select(cigarette_current_freq_w3) %>% table

#### WAVE 2: CURRENT NON-ESTABLISHED SMOKERS ####
w2_cur_non_est_smokers <- adult_panel %>%  filter(current_non_est_smoker_w2 ==1)

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w2_cur_non_est_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_cur_non_est_smokers %>%  select(cigarette_current_freq_w3) %>% table


#### WAVE 1 SMOKERS : QUIT RATE ####

#Quit Rate at Wave 2
w1_cur_est_smokers %>% 
  group_by(abs_w2) %>% 
  count()

#Quit Rate at Wave 3
w1_cur_est_smokers %>% 
  group_by(abs_w3) %>% 
  count()
#### WAVE 2 CURRENT ESTABLISHED SMOKERS: Quit Rate ####

#Quit Rate at Wave 3
w2_cur_est_smokers %>% 
  group_by(abs_w3) %>% 
  count()


#### WAVE 1 ESTABLISHED SMOKERS: QUIT AT WAVE 2 ####
w1_cur_est_smokers %>%  
  group_by(quit_w1_w2) %>% 
  count

#### WAVE 1 ESTABLISHED SMOKERS: QUIT AT WAVE 3 ####
w1_cur_est_smokers %>%  
  group_by(quit_w1_w3) %>% 
  count


#### WAVE 2 ESTABLISHED SMOKERS: QUIT AT WAVE 3 ####
w2_cur_est_smokers %>%  
  group_by(quit_w2_w3) %>% 
  count

#### WAVE 1 ESTABLISHED SMOKERS: QUIT AT WAVE 2 (PAST 30-Days) ####
w1_cur_est_smokers %>% 
  mutate(quit_p30d_w1_w2 = case_when(
                      smoked_past30D_w2 == 0 & cigarette_current_use_w2==0 ~ 1,
                      cigarette_current_use_w2==1 | is.na(cigarette_current_use_w2) |
                                smoked_past30D_w2 == 1 |  is.na(smoked_past30D_w2)  ~ 0)) %>%  
  group_by(quit_p30d_w1_w2) %>% 
  count

#### WAVE 1 CUR. EST. SMOKERS: QUIT AT WAVE 3 (PAST 30-Days) ####
w1_cur_est_smokers %>% 
  mutate(quit_p30d_w1_w3 = case_when(
                              smoked_past30D_w3 == 0   & cigarette_current_use_w3==0 ~ 1,
                              cigarette_current_use_w3==1 | is.na(cigarette_current_use_w3) |
                                        smoked_past30D_w3 == 1 |  is.na(smoked_past30D_w3)  ~ 0)) %>%  
  group_by(quit_p30d_w1_w3) %>% 
  count


#### WAVE 2 CUR. EST. SMOKERS: QUIT AT WAVE 3 (PAST 30-Days) ####
w2_cur_est_smokers %>% 
  mutate(quit_p30d_w1_w3 = case_when(
                                smoked_past30D_w3 == 0   & cigarette_current_use_w3==0 ~ 1,
                                cigarette_current_use_w3==1 | is.na(cigarette_current_use_w3) |
                                          smoked_past30D_w3 == 1 |  is.na(smoked_past30D_w3)  ~ 0)) %>%  
  group_by(quit_p30d_w1_w3) %>% 
  count


#### NOTE: Discrepancy ####

adult_panel %>%  
  group_by(smoked_past30D_w2, cigarette_current_use_w2) %>%  
  count

##NOTE: Why do some people who stated they did not smoke in the past 30 days,
#respond that they have currently smoked some days or everyday ?



#### REMOVE OBJECTS ####

rm(w1_smokers)  
rm(w1_cur_est_smokers)
rm(w1_cur_non_est_smokers)