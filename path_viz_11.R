#Mosaic Plot Without Dropout

#INITIATION: How many never smokers smoked?
never_smokers_w1_status_w3 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, cig_use_ever_w1 ==0) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2, smoked_past12M_w3, smoked_past30D_w3,
         cig_num_life_w2, cig_num_life_w3, cig_use_now_w2, cig_use_now_w3, wave_2, wave_3 )%>% 
  mutate(initiation_w2 = if_else(smoked_past12M_w2==1 | smoked_past30D_w2==1, 1, 0),
         initiation_w3 = if_else(smoked_past12M_w3==1 | smoked_past30D_w3==1, 1, 0),
         est_smoker_w2 = if_else(as.numeric(cig_num_life_w2) == 6, 1, 0),
         est_smoker_w3 = if_else(as.numeric(cig_num_life_w3) == 6, 1, 0)) %>% 
  group_by(initiation_w2, initiation_w3, wave_2, wave_3) %>% 
  count 

never_smokers_w1_status_w2 <- never_smokers_w1_status_w3 %>% 
                                  group_by(initiation_w2) %>% 
                                  summarize(n = sum(n)) %>% 
                                  drop_na(initiation_w2) %>% 
                                  mutate(wave = 2,
                                         status = if_else(initiation_w2==1,
                                                          'initiation',
                                                          'abstinence') ) %>% 
                                  select(-initiation_w2)
                                         
ns_status_w3 <- never_smokers_w1_status_w3 %>% 
  drop_na( initiation_w2 ,  initiation_w3) %>% 
  ungroup %>% 
  select(-wave_2, -wave_3) %>% 
  mutate(status = case_when(initiation_w2==0 & initiation_w3==0 ~'abstinence',
                            initiation_w2==0 & initiation_w3==1 ~'initiation',
                            initiation_w2 ==1 &initiation_w3 == 0 ~'cessation',
                            initiation_w2 ==1 &initiation_w3 == 1 ~'still smoking'),
         wave = 3
         ) %>% 
  select(-initiation_w2, -initiation_w3 )

never_smokers_mosaic_tab <- never_smokers_w1_status_w2 %>% 
                                rbind(ns_status_w3) %>% 
                                select(wave, status, n) %>% 
                                mutate(status = fct_relevel(status,
                                                            'still smoking', 
                                                            'cessation',
                                                            'initiation', 
                                                            'abstinence'),
                                  wave = as.factor(wave),
                                  group = 'Wave 1 Never Smokers') 

never_smokers_mosaic_tab  %>% 
  ggplot(aes(x = wave, y = n, fill = status)) +
  geom_col(position = 'fill')

#### Current Smokers ####

current_smokers_w1 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1,
         cig_use_now_w1 == 1,
         as.numeric(cig_num_life_w1) == 6) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2, smoked_past12M_w3, smoked_past30D_w3,
         wave_2, wave_3 ) %>%
  group_by(smoked_past30D_w2, smoked_past30D_w3, wave_2, wave_3) %>% 
  count

w1_current_smokers_at_w2 <- current_smokers_w1 %>% 
                                    ungroup %>% 
                                    select(-wave_2, -wave_3) %>% 
                                    drop_na(smoked_past30D_w2, smoked_past30D_w3) %>% 
                                    group_by(smoked_past30D_w2) %>% 
                                    summarize(n = sum(n)) %>% 
                                    mutate(wave = 2,
                                           status = if_else(smoked_past30D_w2==1,
                                                            'still smoking',
                                                            'new cessation')) %>% 
                                    select(-smoked_past30D_w2)

w1_current_smokers_at_w3 <- current_smokers_w1 %>% 
                                  ungroup %>% 
                                  select(-wave_2, -wave_3) %>% 
                                  drop_na(smoked_past30D_w2, smoked_past30D_w3) %>% 
                                  mutate(wave = 3,
                                         status = case_when(
                                    smoked_past30D_w2==1 & smoked_past30D_w3==1 ~ 'still smoking',
                                    smoked_past30D_w2==1 & smoked_past30D_w3==0 ~ 'new cessation',                   
                                    smoked_past30D_w2==0 & smoked_past30D_w3==0 ~ 'maintain cessation',               
                                    smoked_past30D_w2==0 & smoked_past30D_w3==1 ~ 'relapse')) %>% 
                                  select(-smoked_past30D_w2, - smoked_past30D_w3)

cur_smoker_mosaic_tab <-w1_current_smokers_at_w2 %>% 
                            rbind(w1_current_smokers_at_w3 ) %>% 
                            mutate(wave = as.factor(wave),
                                   group = "Wave 1 Cur. Est. Smokers") %>% 
                            select(wave, status, n) 

cur_smoker_mosaic_tab %>% 
        mutate(status = fct_relevel(status,
                       'relapse',
                       'maintain cessation',
                       'new cessation', 
                       'still smoking') )%>% 
        ggplot(aes(x = wave, y = n, fill = status)) +
        geom_col(position = 'fill') +
        ggtitle("Wave 1 Current Established Smokers Trajectory") +
        scale_y_continuous(breaks = seq(0, 1, .1))

 
#### Former Smokers ####

  