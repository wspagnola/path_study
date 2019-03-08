require(ggplot2)

#Wave 3 Stats


#Smoking Status (Current Experimental Smokers Doesn't Match)
adult_w3 %>% 
  group_by(smoking_status_w3) %>% 
  count()

#Current Smoking Frequency
adult_w3 %>% 
  group_by(cigarette_current_freq_w3) %>% 
  count()

#Sex (MATCHES)
adult_w3 %>% 
  group_by(gender_w3) %>% 
  count()

#Age (MATCHES)
adult_w3 %>% 
  group_by(age_w3) %>% 
  count()


#Race 
adult_w3 %>%  
  group_by(race_w3) %>% 
  count()
#NOTE: RACE INCLUDES HISPANICS

#Race (DOESN'T MATCH)
adult_w3 %>%  
  group_by(race_ethnicity_w3) %>% 
  count()

#Hispanic (MATCHES)
adult_w3 %>%  
  group_by(hispanic_w3) %>% 
  count()

#Income  (MATCHES)
adult_w3 %>%  
  group_by(income_w3) %>% 
  count()

#Sexual Orientation  (MATCHES)
adult_w3 %>%  
  group_by(sexual_orientation_w3) %>% 
  count()

#Abstinence at Wave 2

#Quit Cigarette Smoking
adult_panel %>% 
    filter(adult_w2$cigarette_current_use_w1 == ) 

adult_panel$cig %>%  table