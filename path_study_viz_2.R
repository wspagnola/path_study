#Sociodemographic Characteristics 
#Among 13,529 Everyday & Someday Smokers at Wave 1

#Excel Sheet Name: 9.20.18 Descriptives

#NOTE: THIS ONE IS WAY OFF!


#Filter Smokers 
smokers_w1 <- adult_panel %>%  
               filter(cigarette_current_use_w1  == 'Yes') 

#### CURRENT SMOKERS DATA (W1) ####

#Gender (W1)
smokers_w1 %>% 
  group_by(gender_w1) %>% 
  count
#NOTE: Missing 7 for Gender

#Age (W1)
smokers_w1 %>% 
  group_by(age_w1) %>% 
  count

#Race (W1)
smokers_w1 %>% 
  group_by(race_ethnicity_w1) %>%  count

#Education
smokers_w1 %>% 
  group_by(education_w1) %>% 
  count

#Income
smokers_w1 %>% 
  group_by(income_w1) %>% 
  count

#Sexual Orientation 
smokers_w1 %>% 
  group_by(sexual_orientation_w1) %>% 
  count

#Poverty Status
smokers_w1 %>% 
  group_by(poverty_w1) %>% 
  count

#Census Region (W1)
smokers_w1 %>% 
  group_by(region_w1) %>% 
  count
  
#Smoking Status (W1)
smokers_w1 %>% 
  group_by(smoking_status_w1) %>% 
  count

# Table: Experiment vs. Established among Current Smokers (W1)
smokers_w1 %>% 
  mutate(est_smoker_w1 = if_else(as.numeric(cigarette_num_life_w1)==6, 1, 0)) %>% 
  group_by(cigarette_current_use_w1, est_smoker_w1 ) %>% 
  count %>% 
  as.data.frame %>% 
  mutate(current_est_vs_exp = case_when(
                       est_smoker_w1 == 0 ~ 'Current Experimental Smoker',
                       est_smoker_w1 == 1 ~ 'Current Established Smoker')) %>% 
  select(current_est_vs_exp, n) 

#Note 'smoking_status_w1' omits experimental smokers both current and former
#Hence, I had to recreate 'CURRENT EXPERIMENTAL SMOKER' here 


adult_w1$R01_AC1009
adult_w1 %>% 
  mutate(days_quit_cigs_w1 = case_when(
    as.numeric(R01_AC1009_UN)==1 & R01_AC1009_NN>=0 ~ R01_AC1009_NN,
    as.numeric(R01_AC1009_UN)==2 & R01_AC1009_NN>=0 ~ R01_AC1009_NN * 30.4375,
    as.numeric(R01_AC1009_UN)==3 & R01_AC1009_NN>=0 ~  R01_AC1009_NN * 365.25), 
    days_quit_cigs_cat_w1 = case_when(
      days_quit_cigs_w1 >= 2 &  days_quit_cigs_w1 <= 7 ~ '2 to 7 Days',
      days_quit_cigs_w1 >= 8 &  days_quit_cigs_w1 < 30 ~ '8 to 30 Days',
      days_quit_cigs_w1 >= 31 &  days_quit_cigs_w1 <= 91 ~ '31 to 91 days',
      days_quit_cigs_w1 > 91 &  days_quit_cigs_w1 <= 365 ~ '91 to 364 days',
      days_quit_cigs_w1 == 365 ~ '1 year',
      days_quit_cigs_w1 > 365   ~ 'More than 1 year'),
    days_quit_cigs_cat_w1 = factor(days_quit_cigs_cat_w1,
                                   levels = c('2 to 7 Days', 
                                              '8 to 30 Days',  
                                              '31 to 91 days',
                                              '91 to 364 day',  
                                              'More than 1 year'))) %>% 
#Note: Mean of days_quit_cigs_w1 Matches Codebook (pg. 1342) 


#### QUIT RATE (W1) (ALL RESPONDENTS) ####
adult_w1 %>% 
  mutate(quit_rate_w1 = case_when(
        R01_AC1009_UN == '(1) 1 = Days' &  R01_AC1009_NN >= 2 & R01_AC1009_NN <= 7 ~ 
                      '2 to 7 days',
        R01_AC1009_UN == '(1) 1 = Days' & R01_AC1009_NN >= 8 & R01_AC1009_NN <= 30  ~
                     '8-30days/ Less than 1 month',
        (R01_AC1009_UN == '(1) 1 = Days' & R01_AC1009_NN >= 31 & R01_AC1009_NN <= 91) ~
                     '31-91 days/3 months',  
          (R01_AC1009_UN == '(1) 1 = Days' & R01_AC1009_NN >= 91 & R01_AC1009_NN <= 364) ~
                     '92-364 days/<12 months',
          (R01_AC1009_UN == '(1) 1 = Days' & R01_AC1009_NN == 365) |
          (R01_AC1009_UN ==  '(2) 2 = Months' & R01_AC1009_NN == 12) |
          (R01_AC1009_UN ==  '(3) 3 = Years' & R01_AC1009_NN == 1) ~
                      '1 year',
          (R01_AC1009_UN == '(1) 1 = Days' & R01_AC1009_NN > 365) |
          (R01_AC1009_UN ==  '(2) 2 = Months' & R01_AC1009_NN > 12) |
          (R01_AC1009_UN ==  '(3) 3 = Years' & R01_AC1009_NN > 1) 
                   ~'More than 1 year')) %>% 
  group_by(quit_rate_w1) %>% 
  count
# NOTE: Excel table does not include month data for quit length less than 1 year
# NOTE: One-year category does not match Excel Table
adult_w1
  

#Calculate Max Quite Length Calculated in Unit of Months
adult_w1 %>% 
  filter(R01_AC1009_UN == '(2) 2 = Months',
         !is.na(R01_AC1009_NN)) %>% 
  pull(R01_AC1009_NN) %>% 
  max

#Find Max Quit Length Calculated in Unit of Days
adult_w1 %>% 
  filter(R01_AC1009_UN == '(1) 1 = Days',
         !is.na(R01_AC1009_NN)) %>% 
  filter(R01_AC1009_NN >= 365)  %>% 
  pull(R01_AC1009_NN) %>% 
  max

adult_w2$R02_AN0105_04
#### WAVE 2 ####

table(smokers_w1$race_w1, smokers_w1$race_w2)
table(smokers_w1$gender_w1, smokers_w1$gender_w2)
table(smokers_w1$income_w1, smokers_w1$income_w2)

#Gender (W1)
smokers_w1 %>% 
  group_by(gender_w2) %>% 
  count

smokers_w1 %>% 
  group_by(age_w2) %>% 
  count




#### Other Quit Variables ####

#R01_AN0256: Stopped smoking / using [tobacco products / specific product] more than 3
##times for one day or longer because you were trying to quit, in the past 12 months

#R02_AN0120: In past 12 months, stopped smoking/using [tobacco products / specific product]
#for one day or longer because you were trying to quit
adult_w1$R01_AN0120 %>%  table

#R01_AN0130_NN: Length of time you stopped smoking / using [tobacco products / specific
#product] because you were trying to quit, in the past 12 months - Number


#R01_AN0300: Length of time stopped smoking / using [tobacco products / specific product]
#the last time you were trying to quit, in the past 12 months: Week measure

#R01_AN0140: Was the duration of last quit attempt reported the longest you went without
#smoking / using [tobacco products / specific product] because you were trying to quit, in the
#past 12 months

#R01_AN0135: Approximate end date of last [tobacco products / specific product] quit attempt

#R01_AN0145_NN: Longest time period for which you stopped smoking / using [tobacco
#products / specific product] because you were trying to quit, in the past 12 months - Number

#R01_AN0145_UN: Longest time period for which you stopped smoking / using [tobacco
# products / specific product] because you were trying to quit, in the past 12 months - Unit

#R01_AN0305: Longest time you stopped smoking / using [tobacco products / specific product]
#because you were trying to quit, in the past 12 months - Week measure


#R01_AN0105_04: [Tobacco products / Specific product] quitting effort: 
#No, I have not tried to quit at all

#R01_AN0120: 
#Stopped smoking / using [tobacco products / specific product] for one day or
#longer because you were trying to quit, in the past 12 months

#R01_AN0125: How many times you stopped smoking / using [tobacco products / specific
#product] for one day or longer because you were trying to quit in the past 12 months

#R01_AN0256: Stopped smoking / using [tobacco products / specific product] more than 3
#times for one day or longer because you were trying to quit, in the past 12 months


#R01_AN0130_UN: Length of time you stopped smoking / using [tobacco products / specific
#product] because you were trying to quit, in the past 12 months - Unit

#R01R_A_INTERVIEW_MMYR: DERIVED - Month and year Adult Interview was completed