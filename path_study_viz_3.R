#Transitions

library(vcd)


#STATUS at WAVE2 for WAVE1 CURRENT Established Cigarette Smokers
adult_panel %>%
    mutate(current_est_smoker_w1 = 
                    if_else(smoking_status_w1== 'est_smoker', 'Yes', 'No'),
           current_est_smoker_w1 = factor(current_est_smoker_w1, 
                                            levels = c('Yes', 'No'))) %>% 
    select(current_est_smoker_w1, smoking_status_w2) %>% 
    table

#Mosaic Plot
adult_panel %>%
  mutate(current_est_smoker_w1 = 
           if_else(smoking_status_w1== 'est_smoker', 'Yes', 'No'),
         current_est_smoker_w1 = factor(current_est_smoker_w1, 
                                        levels = c('Yes', 'No'))) %>% 
  select(current_est_smoker_w1, smoking_status_w2) %>% 
  table  %>%  
  mosaic()


    

#STATUS at WAVE2 for WAVE1 FORMER Established Cigarette Smokers
adult_panel %>%
  mutate(former_est_smoker_w1 = 
           if_else(smoking_status_w1== 'former_est_smoker', 'Yes', 'No'),
         former_est_smoker_w1 = factor(former_est_smoker_w1, 
                                       levels = c('Yes', 'No'))) %>% 
  select(former_est_smoker_w1, smoking_status_w2) %>% 
  table


#STATUS at Wave 2 for  Wave 1 NEVER Smokers
adult_panel %>%
  mutate(never_smoker_w1 = 
              if_else(smoking_status_w1== 'never_smoker', 'Yes', 'No'),
         never_smoker_w1 = factor(never_smoker_w1, 
                                       levels = c('Yes', 'No'))) %>% 
  select(never_smoker_w1, smoking_status_w2) %>% 
  table

#Mosaic Plot
adult_panel %>%
  mutate(never_smoker_w1 = 
           if_else(smoking_status_w1== 'never_smoker', 'Yes', 'No'),
         never_smoker_w1 = factor(never_smoker_w1, 
                                  levels = c('Yes', 'No'))) %>% 
  select(never_smoker_w1, smoking_status_w2) %>% 
  mosaicplot



#### Graphics ####


#Barplot Transitions: Wave 1 Established Smokers
transition_current_est_smokers  <- adult_panel %>%
                    mutate(current_est_smoker_w1 = 
                             if_else(smoking_status_w1== 'est_smoker', 'Yes', 'No'),
                           current_est_smoker_w1 = factor(current_est_smoker_w1, 
                                                          levels = c('Yes', 'No'))) %>% 
                    group_by(current_est_smoker_w1, smoking_status_w2) %>% 
                    count %>% 
                    drop_na %>% 
                    as.data.frame %>% 
                    rbind(data.frame(current_est_smoker_w1 = 'Yes',  
                                     smoking_status_w2 = 'never_smoker', 
                                     n = 0)) 
                    transition_current_est_smokers %>% 
                            ggplot(aes(x= current_est_smoker_w1, y = n, fill = smoking_status_w2)) +
                            geom_col(position = position_dodge())

#Barplot Transitions: Wave 1 Former Established Smokers
transition_former_est_smokers <- adult_panel %>%
              mutate(former_est_smoker_w1 = 
                       if_else(smoking_status_w1== 'former_est_smoker', 'Yes', 'No'),
                     former_est_smoker_w1 = factor(former_est_smoker_w1, 
                                                   levels = c('Yes', 'No'))) %>% 
              group_by(former_est_smoker_w1, smoking_status_w2) %>% 
              count %>% 
              drop_na %>% 
              as.data.frame %>% 
              rbind(data.frame(former_est_smoker_w1 = 'Yes',  
                               smoking_status_w2 = 'never_smoker', 
                               n = 0)) 

transition_former_est_smokers %>% 
      ggplot(aes(x= former_est_smoker_w1, y = n, fill = smoking_status_w2)) +
      geom_col(position = position_dodge())


#Barplot Transitions: Wave 1 NEVER Smokers
transition_never_smokers <- adult_panel %>%
                    mutate(never_smoker_w1 = 
                             if_else(smoking_status_w1== 'never_smoker', 'Yes', 'No'),
                           never_smoker_w1 = factor(never_smoker_w1, 
                                                    levels = c('Yes', 'No'))) %>% 
                    group_by(never_smoker_w1, smoking_status_w2) %>% 
                    count %>% 
                    drop_na %>% 
                    as.data.frame %>% 
                    rbind(data.frame(never_smoker_w1= 'No',  
                                   smoking_status_w2 = 'never_smoker', 
                                   n = 0)) 

transition_never_smokers %>% 
  ggplot(aes(x= never_smoker_w1, y = n, fill = smoking_status_w2)) +
  geom_col(position = position_dodge())
  
#### Plot by Transition Category ####
adult_panel %>% 
group_by(smoking_status_w1, smoking_status_w2) %>% 
  count %>% 
  drop_na %>% 
  as.data.frame %>% 
  rbind(data.frame( smoking_status_w1  = 'est_smoker',  
                   smoking_status_w2 = 'never_smoker', 
                   n = 0))  %>% 
  rbind(data.frame( smoking_status_w1 = 'former_est_smoker',  
             smoking_status_w2 = 'never_smoker', 
             n = 0)) %>% 
  mutate(smoking_status_w1 = factor(smoking_status_w1, 
                                    levels = c('never_smoker', 
                                               'former_est_smoker',
                                               'est_smoker')),
         smoking_status_w2 = factor(smoking_status_w2, 
                              levels = c('never_smoker', 
                                         'former_est_smoker',
                                         'est_smoker'))) %>% 
  arrange(smoking_status_w1, smoking_status_w2) %>% 
  ggplot(aes(x=smoking_status_w1, y = n, fill = smoking_status_w2)) +
  geom_col(position = position_fill())
    
