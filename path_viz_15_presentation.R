


adult_panel %>%  
  count(wave_1, wave_2, wave_3) 

# Wave 1 Adult Type 
wave_1_adult_type_tab <-adult_panel %>% 
                            filter(wave_1 == 1) %>% 
                            mutate(adult_type = 'Continuing Adult') %>% 
                            count(adult_type) %>% 
                            mutate(wave = 1)
  

#Wave 2 Adult Type 
wave_2_adult_type_tab <- adult_panel %>% 
        mutate(adult_type = case_when(
            as.numeric( R02_CONTINUING_ADULT_LD)==1 ~ 'Continuing Adult',
            as.numeric(R02_NEW_BASELINE_ADULT_LD) ==1 ~ 'New Adult',
            is.na(R02_CONTINUING_ADULT_LD) ~ 'New Drop Out')) %>% 
        count(adult_type) %>% 
        mutate(wave = 2)
  

#Wave 3 Adult Type 
wave_3_adult_type_tab <- adult_panel %>%
                             mutate(adult_type = case_when( 
    as.numeric(R03_ADULTTYPE)==1 & wave_2==1 ~ 'Continuing Adult',
    as.numeric(R03_ADULTTYPE)==1 & wave_2==0 ~ 'Returning Adult',
    as.numeric(R03_ADULTTYPE)==2  ~ 'New Adult',
    is.na(R03_ADULTTYPE) & wave_2==1 ~ 'New Drop Out',
    is.na(R03_ADULTTYPE) & wave_2==0 ~ 'Old Drop Out')) %>% 
    count(adult_type) %>% 
    mutate(wave = 3)

#Plot Attrition Across Waves
wave_1_adult_type_tab %>% 
  rbind(wave_2_adult_type_tab) %>% 
  rbind(wave_3_adult_type_tab) %>% 
  select(wave, everything()) %>% 
  filter(adult_type != 'New Drop Out', adult_type != 'Old Drop Out') %>% 
  ggplot(aes(x = wave, y = n, fill = adult_type)) +
  geom_col() +
  theme_bw() +
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank()) +
  scale_y_continuous(limits = c(0, 35000), expand = c(0,0),
                     breaks = seq(0, 35000, 5000)) +
  scale_fill_discrete(name = 'Adult Type') +
  ggtitle('Attrition Across Waves') + 
  xlab('Waves')
#ggsave('Figures/attritions_across_waves.png')
?ggsave
adult_panel$
adult_panel$smoking_status_full
#### Look at Smoking Full Distribution 
adult_panel %>% 
  count(smoking_status_full_w1)


adult_panel %>% 
  count(smoking_status_full_w2)

adult_panel %>% 
  count(smoking_status_full_w3)


#Logistic Regression 

#Set Survey Weights 
