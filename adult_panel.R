adult_panel <- adult_w1 %>%
  full_join(y = adult_w2, by = c('PERSONID')) %>%
  full_join(y= adult_w3, by = c('PERSONID')
  )



remove(list = c('adult_w1', 'adult_w2', 'adult_w3'))


#Recode NAs as Zero for obs. that was not included in a given wave
adult_panel <- adult_panel %>% 
  mutate(wave_1 = ifelse(is.na(wave_1), 0, wave_1),
         wave_2 = ifelse(is.na(wave_2), 0, wave_2),
         wave_3 = ifelse(is.na(wave_3), 0, wave_3)
  ) 
