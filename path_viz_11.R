#Mosaic Plot Without Dropout

#INITIATION: How many never smokers started Smoking?
never_smokers_w1_status_w3 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, as.numeric(R03_ADULTTYPE)==1, cig_use_ever_w1 ==0) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2, smoked_past12M_w3, smoked_past30D_w3,
         cig_num_life_w2, cig_num_life_w3, cig_use_now_w2, cig_use_now_w3, wave_2, wave_3 )%>% 
  mutate(initiation_w2 = if_else(smoked_past12M_w2==1 | smoked_past30D_w2==1, 1, 0),
         initiation_w3 = if_else(smoked_past12M_w3==1 | smoked_past30D_w3==1, 1, 0)) %>% 
  group_by(initiation_w2, initiation_w3, wave_2, wave_3) %>% 
  count 


ns_status_w1 <- adult_panel %>% 
                                   filter(as.numeric(R02_CONTINUING_ADULT_LD)==1,
                                          as.numeric(R03_ADULTTYPE)==1,
                                          cig_use_ever_w1 == 0) %>% 
                                    select(cig_use_ever_w1) %>% 
                                    mutate(status = if_else(cig_use_ever_w1==1,
                                                   'initiated',
                                                   'abstinence'),
                                           wave = 1) %>% 
                                    count(status, wave)
                                  

ns_status_w2 <- never_smokers_w1_status_w3 %>% 
                                  group_by(initiation_w2) %>% 
                                  summarize(n = sum(n)) %>% 
                                  drop_na(initiation_w2) %>% 
                                  mutate(wave = 2,
                                         status = if_else(initiation_w2==1,
                                                          'initiated',
                                                          'abstinence') ) %>% 
                                  select(-initiation_w2)

#Generate Status                              
ns_status_w3 <- never_smokers_w1_status_w3 %>% 
  drop_na( initiation_w2 ,  initiation_w3) %>% 
  ungroup %>% 
  select(-wave_2, -wave_3) %>% 

  mutate(status = case_when(initiation_w2==0 & initiation_w3==0 ~'abstinence',
                            initiation_w2==1 | initiation_w3==1 ~'initiated'),
         wave = 3) %>% 
  select(-initiation_w2, -initiation_w3 ) %>% 
  group_by(status, wave) %>% 
  summarize(n = sum(n)) %>% 
  ungroup
 


#Table of W1 Never Smoker Transitions at W2 & W3
never_smokers_mosaic_tab <- ns_status_w1 %>% 
                                rbind(ns_status_w2) %>% 
                                rbind(ns_status_w3) %>% 
                                select(wave, status, n) %>% 
                                mutate(status = fct_relevel(status,
                                                            'initiated', 
                                                            'abstinence'),
                                  wave = as.factor(wave),
                                  group = 'Wave 1 Never Smokers') 

never_smokers_n_1 <-  ns_status_w1$n
subtitle <- paste0('N= ', never_smokers_n_1, '** at Wave 1')
#Plot W1 Never Smoker Transitions at W2 & W3
w1_never_smoker_initiation <- never_smokers_mosaic_tab  %>% 
                              ggplot(aes(x = wave, y = n, fill = status)) +
                              geom_col(position = 'fill') +
                              labs(title = 'Transition of Wave 1 Never Smokers* Across Waves',
                                   subtitle = subtitle,
                                      caption = '*For W1 Adult Never Smokers only. Does not account for attrition or non-response. \n **Due to Item Non-Response, N = 5218 at W2 and N = 5215 at W3.') +
                              xlab('Waves') +
                              ylab('Proportion (Unweighted)') +
                              scale_y_continuous(expand = c(0, 0))+
                              labs() +
                              theme_wave_mosaic 


ggsave('Figures/W1_Never_Smoker_Initiation.png', width = 6)


#### Current Smokers ####

current_smokers_w1 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, as.numeric(R03_ADULTTYPE)==1,
         cig_use_now_w1 == 1,
         as.numeric(cig_num_life_w1) == 6) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2, smoked_past12M_w3, smoked_past30D_w3,
         wave_2, wave_3 ) %>%
  group_by(smoked_past30D_w2, smoked_past30D_w3, wave_2, wave_3) %>% 
  count



w1_current_smokers_at_w1 <- adult_panel %>% 
                              filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
                                     as.numeric(R03_ADULTTYPE)==1,
                                     cig_use_now_w1 == 1,
                                     as.numeric(cig_num_life_w1) == 6) %>% 
                              select(cig_use_now_w1, cig_num_life_w1) %>% 
                              mutate(wave = 1,
                                     status = if_else(cig_use_now_w1 == 1 &
                                                      as.numeric(cig_num_life_w1) == 6, 
                                                      'still smoking', 'new cessation')) %>% 
                              count(status, wave)



w1_current_smokers_at_w2 <- current_smokers_w1 %>% 
                                    ungroup %>% 
                                    select(-wave_2, -wave_3) %>% 
                                    drop_na(smoked_past30D_w2) %>% 
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

#Create Table
cur_smoker_mosaic_tab <- w1_current_smokers_at_w1 %>% 
                            rbind(w1_current_smokers_at_w2) %>% 
                            rbind(w1_current_smokers_at_w3 ) %>% 
                            mutate(wave = as.factor(wave)) %>% 
                            select(wave, status, n) 

#Calculate N at each wave (May differ due to Item Non-Response)
cur_smoker_mosaic_tab %>%  group_by(wave) %>%  summarize(n = sum(n))

#Plot 'Mosaic/Filled Barplot' 
cur_smoker_mosaic_tab %>% 
        mutate(status = fct_relevel(status,
                       'relapse',
                       'maintain cessation',
                       'new cessation', 
                       'still smoking') )%>% 
        ggplot(aes(x = wave, y = n, fill = status)) +
        geom_col(position = 'fill', color = 'black') +
        labs(title = "Wave 1 Current Established Smokers* Trajectory",
             subtitle ='N = 8218** at Wave 1',
             caption = '*Only includes current established smokers who participated in all three waves \n **N=8217 at Wave 2 and N=8216 at Wave 3 due to item non-response \n +Cessation in Wave 1 is defined by everyday and some day smoking status. \n ++Cessation in Wave 2 and 3 is defined by not smoking in past 30 days') +
       # scale_y_continuous(breaks = seq(0, 1, .1)) +
        xlab('Waves') +
        ylab('Proportion (Unweighted)') +
        scale_fill_manual(values = c(  'pink',  'lightgreen', 'darkgreen', 'red'))+
        scale_x_discrete(labels = c('1+', '2++', '3++')) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_wave_mosaic

ggsave('Figures/W1_Cur_Est_Smokers_Transitions.png', width = 6)

 
#### Former Est. Smokers ####


fmr_est_smokers_w1 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, as.numeric(R03_ADULTTYPE)==1,
         cig_use_now_w1 == 0,
         as.numeric(cig_num_life_w1) == 6) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2, smoked_past12M_w3, smoked_past30D_w3,
         wave_2, wave_3 ) %>%
  group_by(smoked_past30D_w2, smoked_past30D_w3, wave_2, wave_3) %>% 
  count


w1_fmr_est_smokers_at_w1 <- adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
         as.numeric(R03_ADULTTYPE)==1,
         cig_use_now_w1 == 0,
         as.numeric(cig_num_life_w1) == 6) %>% 
         select(cig_use_now_w1, cig_num_life_w1) %>% 
         mutate(wave = 1,
                status = if_else(cig_use_now_w1 == 0 &
                          as.numeric(cig_num_life_w1) == 6, 
                        'maintain cessation', 'still_smoking')) %>% 
         count(status, wave)

w1_fmr_est_smokers_at_w2 <- fmr_est_smokers_w1%>% 
                              ungroup %>% 
                              select(-wave_2, -wave_3, -smoked_past30D_w3) %>% 
                              group_by(smoked_past30D_w2) %>% 
                              summarize(n = sum(n)) %>% 
                              mutate(wave = 2,
                                     status = if_else(smoked_past30D_w2==1,
                                              'relapse',
                                              'maintain cessation')) %>% 
                              select(-smoked_past30D_w2)

w1_fmr_est_smokers_at_w3 <-  fmr_est_smokers_w1%>% 
                          ungroup %>% 
                          select(-wave_2, -wave_3) %>% 
                          mutate(wave = 3,
                          status = case_when(
                                smoked_past30D_w2==1 & smoked_past30D_w3==1 ~ 'still smoking',
                                smoked_past30D_w2==1 & smoked_past30D_w3==0 ~ 'new cessation',                   
                                smoked_past30D_w2==0 & smoked_past30D_w3==0 ~ 'maintain cessation',               
                                smoked_past30D_w2==0 & smoked_past30D_w3==1 ~ 'relapse')) %>% 
                            select(-smoked_past30D_w2, - smoked_past30D_w3)


fmr_est_smoker_mosaic_tab <- w1_fmr_est_smokers_at_w1 %>% 
                                rbind(w1_fmr_est_smokers_at_w2) %>% 
                                rbind(w1_fmr_est_smokers_at_w3 ) %>% 
                                mutate(wave = as.factor(wave)) %>% 
                                select(wave, status, n) 


#Find N
fmr_est_smoker_mosaic_tab %>%  group_by(wave) %>%  summarize(n = sum(n))

#Plot 'Mosaic/Filled Barplot' 
fmr_est_smoker_mosaic_tab %>% 
  mutate(status = fct_relevel(status,
                              'new cessation', 
                              'still smoking',
                              'relapse',
                              'maintain cessation') )%>% 
  ggplot(aes(x = wave, y = n, fill = status)) +
  geom_col(position = 'fill', color = 'black') +
  labs(title = "Wave 1 Former Established Smokers* Trajectory",
       subtitle ='N = 8218** at Wave 1',
       caption = '*Only includes current established smokers who participated in all three waves \n **Cessation in Wave 1 is defined by everyday and some day smoking status. \n ***Cessation in Wave 2 and 3 is defined by not smoking in past 30 days.') +
  # scale_y_continuous(breaks = seq(0, 1, .1)) +
  xlab('Waves') +
  ylab('Proportion (Unweighted)') +
  scale_fill_manual(values = c( 'lightgreen', 'red', 'pink', 'darkgreen'))+
  scale_x_discrete(labels = c('1**', '2***', '3***')) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_wave_mosaic

ggsave('Figures/W1_FMR_Est_Smokers_Transitions.png', width = 6)


#### Compare P30 Quit Rate and P12M Quit Rate for W1 Cur. Est. Smokers ####

#Compare 30 Day Rate with Year Rate (WAVE 2) for W1 Cur. Est. Smokers
adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, as.numeric(R03_ADULTTYPE)==1,
         cig_use_now_w1 == 1,
         as.numeric(cig_num_life_w1) == 6,
         wave_2 == 1) %>% 
  select(smoked_past12M_w2, smoked_past30D_w2) %>% 
  count(smoked_past12M_w2, smoked_past30D_w2)
#One person did not report smoking in past year but missing data on past-30 smoking
#9 Smoked in past 30 days but not in past year?!! is this an error?


#Compare 30 Day Rate with Year Rate (WAVE 3) for W1 Cur. Est. Smokers
adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, as.numeric(R03_ADULTTYPE)==1,
         cig_use_now_w1 == 1,
         as.numeric(cig_num_life_w1) == 6,
         wave_3 == 1) %>% 
  select(smoked_past12M_w3, smoked_past30D_w3) %>% 
  count(smoked_past12M_w3, smoked_past30D_w3)
#11 Smoked in past 30 days but not in past year?!! is this an error?
#1 person smoked in past year but missing data on past-30 day smoking




#### NOTES #####
# Analyze maintenance and cessation rates among never smokers???!!!
# still_smoking_w3 = if_else(initiation_w2 == 1 &  smoked_past30D_w3==1, 1, 0),
# cessation_w3 = if_else(initiation_w2 == 1 & 
#                          (smoked_past30D_w3==0 |
#                             smoked_past12M_w3 = 0), 1, 0),
# est_smoker_w2 = if_else(as.numeric(cig_num_life_w2) == 6, 1, 0),
# est_smoker_w3 = if_else(as.numeric(cig_num_life_w3) == 6, 1, 0)


# status = case_when(initiation_w2==0 & initiation_w3==0 ~'abstinence',
#                    initiation_w2==0 & initiation_w3==1 ~'initiation',
#                    initiation_w2 ==1 &initiation_w3 == 0 ~'cessation',
#                    initiation_w2 ==1 &initiation_w3 == 1 ~'still smoking'),
# wave = 3
#   

