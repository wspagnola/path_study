#Lifetime Smokers

#Sociodemographic Characteristics among 16,327 Lifetime Smokers at Wave 1

#Excel Sheet: Quit Attempts and Quit Rates 6.4.18

#NOTE: CHECK GENDER AND RACE VARIABLES!!!!
###May be more than one

#Keep only respondents who have smoked more than 100 in their lives
lifetime_smoke <- adult_panel %>% 
                        filter(as.numeric(cigarette_num_life_w1)== 6) 

#Gender (W1)
lifetime_smoke %>% 
  group_by(gender_w1) %>% 
  count()
#NOTE: Gender from WAVE 1 does not match Excel table.  
##Check to see if there is another variable for Gender used here

# Age (W1)
lifetime_smoke %>% 
  group_by(age_w1) %>% 
  count()

# Race/Ethnicity (W1)
lifetime_smoke %>% 
  group_by(race_ethnicity_w1) %>% 
  count()
#NOTE: RACE from WAVE 1 does not match Excel table.  
##Check to see if there is another variable for RACE used here


#Education (W1)
lifetime_smoke %>% 
  group_by(education_w1) %>% 
  count()

#Income (W1)
lifetime_smoke %>% 
  group_by(income_w1) %>% 
  count()

#Sexual Orientation (W1)
lifetime_smoke %>% 
  group_by(sexual_orientation_w1) %>% 
  count()

#Poverty (W1)
lifetime_smoke %>% 
  group_by(poverty_w1) %>% 
  count()

#Census (W1)
lifetime_smoke %>% 
  group_by(region_w1) %>% 
  count()


#### WAVE 2 VARIABLES ####

#Gender (W2)
lifetime_smoke %>% 
  group_by(gender_w2) %>% 
  count()

#Race (W2)
lifetime_smoke %>% 
  group_by(race_ethnicity_w2) %>% 
  count()

#Education (W2)
lifetime_smoke %>% 
  group_by(education_w2) %>% 
  count()

#Income (W2)
lifetime_smoke %>% 
  group_by(income_w2) %>% 
  count()

#Sexual Orientation (W2)
lifetime_smoke %>% 
  group_by(sexual_orientation_w2) %>% 
  count()

#Psychological Distress (W2)
###??????



#### WAVE 2: QUITTING VARIABLES ####

#Quit Attempts (W2)

#Quit Rate (W2)


#Quit Rate (# of Days?) (W2)



