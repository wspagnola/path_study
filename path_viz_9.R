#13 Smoking Status W1-W3

#Smoking Trajectories Waves 1-3 among ALL (20,208 Established & NonEstablished Smokers at Wave 1)


#NOTE: Current Established & Non-established does not include every one who stated that they smoked
#Some people indicated that they smoked but because they did not answer one or more of the
#lifetime cigarette number or current use questions they are labelled as NA because it can't
#be determined which group they belong to

#### WAVE 1: Smokers ####
w1_smokers <- adult_panel %>% 
                  filter(current_est_smoker_w1 ==1 | current_non_est_smoker_w1 ==1) 
 
##Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_smokers %>% select(smoking_status_w2) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 2
w1_smokers %>%  select(cigarette_current_freq_w2)  %>% table

##Current Smoking Status by Lifetime Cigarette Use at Wave 3
w1_smokers %>%  select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w1_smokers %>%  select(cigarette_current_freq_w3) %>% table

rm(w1_smokers)  

#### WAVE 2: Smokers ####
w2_smokers <- adult_panel %>% 
  filter(current_est_smoker_w2 ==1 | current_non_est_smoker_w2 ==1) 

##Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_smokers %>% select(smoking_status_w3) %>% table

#Current Smoking Frequency by Lifetime Cigarette Use at Wave 3
w2_smokers %>%  select(cigarette_current_freq_w3)  %>% table

rm(w2_smokers)  


#### WAVE 1: CURRENT ESTABLISHED SMOKERS ####
w1_cur_est_smokers <- adult_panel %>%  filter(current_est_smoker_w1 ==1)

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
w1_cur_non_est_smokers <- adult_panel %>%  filter(current_non_est_smoker_w1 ==1)

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

