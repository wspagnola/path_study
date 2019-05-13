
summary(adult_panel$R01_AC1006)
hist(adult_panel$R01_AC1006)

adult_panel$R01_AC1120

adult_panel$
adult_panel$R02_AC1004
adult_panel$R01R_A_AC1006

#Age of Initiations: Wave 1 Smokers
w1_initiate_tab <- adult_panel %>% 
                              mutate(age = str_remove_all(R01R_A_AC1006, '\\([0-9]\\)'),
                                     age = str_remove_all(age, '[0-9] = '),
                                     age = str_trim(age),
                                     age = as.factor(age)) %>%  
                              drop_na(age) %>% 
                              count(age) %>% 
                              mutate(prop = n / sum(n),
                                     initiate = 'Wave 1')
                              
w2_initiate_tab <- adult_panel %>% 
                        mutate(age = str_remove_all(R02R_A_AGECAT7, '\\([0-9]\\)'),
                               age = str_remove_all(age, '[0-9] = '),
                               age = str_trim(age),
                               age = fct_collapse(age,
                                                  '18 to 24 years old' = '18 to 24 years old',
                                                  '25 to 34 years old' = '25 to 34 years old',
                                                  '35 to 44 years old' = '35 to 44 years old',
                                                  '45 to 54 years old' = '45 to 54 years old',
                                                  '55 years old or older' = c('55 to 64 years old',
                                                                           '65 to 74 years old',
                                                                           '75 years old or older'))) %>%
                        count(never_smoker_w1, never_smoker_w2, age) %>% 
                        filter(never_smoker_w1==1,  never_smoker_w2 == 0)  %>% 
                        mutate(prop = n / sum(n),
                               initiate = 'Wave 2') %>% 
                        select(-never_smoker_w1, -never_smoker_w2)

w3_initiate_tab <- adult_panel %>% 
                        mutate(age = str_remove_all( R03R_A_AGECAT7, '\\([0-9]\\)'),
                               age = str_remove_all(age, '[0-9] = '),
                               age = str_trim(age),
                               age = fct_collapse(age,
                                                  '18 to 24 years old' = '18 to 24 years old',
                                                  '25 to 34 years old' = '25 to 34 years old',
                                                  '35 to 44 years old' = '35 to 44 years old',
                                                  '45 to 54 years old' = '45 to 54 years old',
                                                  '55 years old or older' = c('55 to 64 years old',
                                                                           '65 to 74 years old',
                                                                           '75 years old or older')),
                               age = factor(age, levels = c(levels(age),  "Less than 18 years old"))) %>%
                        count(never_smoker_w1, 
                              never_smoker_w2, 
                              never_smoker_w3,
                               age) %>% 
                        filter(never_smoker_w1 == 1,
                               never_smoker_w2 == 1,
                               never_smoker_w3 == 0) %>%
                        mutate(prop = n / sum(n),
                               initiate = 'Wave 3') %>% 
                        select(-never_smoker_w1, 
                               -never_smoker_w2,
                               -never_smoker_w3)


# Add missing level (below 18 years old)
w2_initiate_tab <- w2_initiate_tab %>% 
                          full_join(data.frame(age = unique(w1_initiate_tab$age), initiate = 'Wave 2')) %>% 
                          mutate_all(~replace(., is.na(.), 0))
w3_initiate_tab <- w3_initiate_tab %>% 
                        full_join(data.frame(age = unique(w1_initiate_tab$age), initiate = 'Wave 3')) %>% 
                        mutate_all(~replace(., is.na(.), 0))

age_levels <- c("Less than 18 years old", '18 to 24 years old', '25 to 34 years old', '35 to 44 years old', '45 to 54 years old',
                '55 years old or older')



  
  
#Compare Distribution
 w1_initiate_tab  %>% 
              rbind(w2_initiate_tab) %>% 
              rbind(w3_initiate_tab) %>% 
              mutate(age = factor(age, levels = age_levels)) %>% 
              ggplot(aes(x= age, y = prop)) +
              geom_col(position = 'stack') +
              facet_wrap(~initiate, ncol = 1) +
              ggtitle('Age of Initiation')
ggsave('Figures/initiate_age_distribution.png', width = 10, height = 8)

#Compare Distribution
w1_initiate_tab  %>% 
  filter(age != 'Less than 18 years old') %>% 
  mutate(prop = n / sum(n)) %>% 
  rbind(w2_initiate_tab) %>% 
  rbind(w3_initiate_tab) %>% 
  ggplot(aes(x= age, y = prop)) +
  geom_col(position = 'stack') +
  facet_wrap(~initiate, ncol = 1) +
  ggtitle('Age of Initiation (Only Above 18)')
ggsave('Figures/initiate_age_distribution_above_18.png', width = 10, height = 8)



