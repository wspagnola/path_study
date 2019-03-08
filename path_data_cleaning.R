require(tidyverse)
require(stringr)
#### Load in Data ####

#Load Adult Waves 1, 2, 3
load('Input/36498-1001-Data.rda')
load('Input/36498-2001-Data.rda')
load('Input/36498-3001-Data.rda')

#### WAVE 1: Clean ####

#Rename Varaiables (W1)
adult_w1 <- da36498.1001 %>%  
                rename(gender_w1 = R01R_A_SEX,
                       race_w1 = R01R_A_RACECAT3,
                       hispanic_w1 = R01R_A_HISP,
                       sexual_orientation_w1 = R01R_A_SEXORIENT2,
                       poverty_w1 =R01R_POVCAT2,
                       region_w1 = R01X_CB_REGION,
                       cigarette_use_ever_w1 = R01_AC1002,
                       cigarette_current_freq_w1 = R01_AC1003,
                       cigarette_num_life_w1 = R01_AC1005
)

#Collapse variables: age, income, education, current cigarette use; Recode cigarette ever us
adult_w1 <- adult_w1 %>% 
                mutate(age_w1 = fct_collapse(R01R_A_AGECAT7,
                            'btwn_18_to_34' = c('(1) 1 = 18 to 24 years old', 
                                                '(2) 2 = 25 to 34 years old'),
                            'btwn_35_to_64' = c('(3) 3 = 35 to 44 years old',
                                                '(4) 4 = 45 to 54 years old',
                                                '(5) 5 = 55 to 64 years old'),
                            'older_than_65' = c('(6) 6 = 65 to 74 years old',
                                                '(7) 7 = 75 years old or older')),
                       education_w1 = fct_collapse(R01R_A_AM0018,
                            'less_than_hs'= "(1) 1 = Less than High School",
                            'high_school' = c("(2) 2 = GED",
                                              "(3) 3 = High school graduate"),
                            'some_college' =  "(4) 4 = Some college (no degree) or associates degree",
                            "college_or_more" = c("(5) 5 = Bachelor's degree", 
                                                  "advanced_degree" =  "(6) 6 = Advanced degree")),
                       income_w1 = fct_collapse(R01R_A_AM0030,
                            'less_than_25k' =  c("(1) 1 = Less than $10,000",
                                                 "(2) 2 = $10,000 to $24,999"),
                            'btwn_25_to_50k'= "(3) 3 = $25,000 to $49,999",
                            'btwn_50k_100k' = "(4) 4 = $50,000 to $99,999",
                            'more_than_100k' =  "(5) 5 = $100,000 or more"),
                       cigarette_current_use_w1 = recode(cigarette_current_freq_w1, 
                                                          "(1) 1 = Every day" = 1,  
                                                          "(2) 2 = Some days" = 1,
                                                          "(3) 3 = Not at all" = 0),
                       cigarette_use_ever_w1 = recode(cigarette_use_ever_w1,
                                                          '(1) 1 = Yes' = 1,
                                                          '(2) 2 = No' = 0)
)

#Create Smoking Status Factor Variable and Binary Variables 
## est_smoker = established smoker (current & former); smoked 100 cigs in lifetime
#Smoking Status Full has all categories cur/fmr est/exp smoker and non-smoers
#Smoking Status collapsed smoking status full into current, former, never
adult_w1 <- adult_w1 %>%  
                    mutate(
        est_smoker_w1 = if_else(as.numeric(cigarette_num_life_w1) == 6, 1, 0),
        smoking_status_full_w1 = case_when(
              cigarette_current_use_w1 == 1 & est_smoker_w1 == 1 ~ 'current_est_smoker',
              cigarette_current_use_w1 == 0 & est_smoker_w1 == 1 ~ 'former_est_smoker',
              cigarette_current_use_w1 == 1 & est_smoker_w1 == 0 ~ 'current_exp_smoker',
              cigarette_current_use_w1 == 0 & est_smoker_w1 == 0 &
                        cigarette_use_ever_w1 == 1 ~ 'former_exp_smoker',
              cigarette_use_ever_w1 == 0 ~'never_smoker'), 
        smoking_status_w1 = fct_collapse(smoking_status_full_w1,
                                         'current' = c('current_est_smoker', 'current_exp_smoker'),
                                         'former' = c('former_est_smoker', 'former_exp_smoker')),
        current_est_smoker_w1 = if_else(smoking_status_full_w1 == 'current_est_smoker', 1, 0),
        former_est_smoker_w1 = if_else(smoking_status_full_w1 == 'former_est_smoker', 1, 0),
        current_exp_smoker_w1 = if_else(smoking_status_full_w1 == 'current_exp_smoker', 1, 0),
        former_exp_smoker_w1 = if_else(smoking_status_full_w1 == 'former_exp_smoker', 1, 0),
        never_smoker_w1 = if_else(smoking_status_full_w1 == 'never_smoker', 1, 0)
)



#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W1)
adult_w1 <- adult_w1 %>% 
              mutate(race_ethnicity_w1 = case_when(
                race_w1=='(1) 1 = White alone'& 
                  hispanic_w1 == '(2) 2 = Not Hispanic'~ 'NH White',
                race_w1=='(2) 2 = Black alone' &
                  hispanic_w1 == '(2) 2 = Not Hispanic' ~ 'NH Black', 
                hispanic_w1=='(1) 1 = Hispanic' ~ 'Hispanic',
                race_w1=='(3) 3 = Other' & hispanic_w1== '(2) 2 = Not Hispanic' ~ 'Other')
)


# Psychological Variable: R01_AX0161 (Sad) or R01_AX0163 (Anxious) in past month
adult_w1 <- adult_w1 %>% 
              mutate(psychdist_w1 = if_else( 
                  as.numeric(R01_AX0161) == 1 | as.numeric(R01_AX0163) == 1, 1, 0)
)

#### WAVE 2: Clean ####

#Load Data and Rename Variables (W2)
adult_w2 <- da36498.2001 %>% 
                rename(gender_w2 = R02R_A_SEX,
                       race_w2 = R02R_A_RACECAT3,
                       hispanic_w2 = R02R_A_HISP,
                       sexual_orientation_w2 = R02R_A_SEXORIENT2,
                       cigarette_current_freq_w2 = R02_AC1003,
                       cigarette_num_life_w2 = R02_AC1005,
                       smoked_past12M_w2 = R02_AC1002_12M
) 

#Collapse Age, Education, and Income Factors (W2)
adult_w2 <- adult_w2 %>%  
                mutate(age_w2 = fct_collapse(R02R_A_AGECAT7,
                         'btwn_18_to_34' = c('(1) 1 = 18 to 24 years old', 
                                             '(2) 2 = 25 to 34 years old'),
                         'btwn_35_to_64' = c('(3) 3 = 35 to 44 years old',
                                             '(4) 4 = 45 to 54 years old',
                                             '(5) 5 = 55 to 64 years old'),
                         'older_than_65' = c('(6) 6 = 65 to 74 years old',
                                             '(7) 7 = 75 years old or older')),
                        education_w2 = fct_collapse(R02R_A_AM0018,
                           'less_than_hs'= "(1) 1 = Less than High School",
                           'high_school' = c("(2) 2 = GED",
                                             "(3) 3 = High school graduate"),
                           'some_college' =  "(4) 4 = Some college (no degree) or associates degree",
                           "college_or_more" = c("(5) 5 = Bachelor's degree", 
                                                 "advanced_degree" =  "(6) 6 = Advanced degree")),
                        income_w2 = fct_collapse(R02R_A_AM0030,
                                  'less_than_25k' =  c("(1) 1 = Less than $10,000",
                                                       "(2) 2 = $10,000 to $24,999"),
                                  'btwn_25_to_50k'= "(3) 3 = $25,000 to $49,999",
                                  'btwn_50k_100k' = "(4) 4 = $50,000 to $99,999",
                                  'more_than_100k' =  "(5) 5 = $100,000 or more"),
                       cigarette_current_use_w2 = recode(cigarette_current_freq_w2, 
                                                            "(1) 1 = Every day" = 1,  
                                                            "(2) 2 = Some days" = 1,
                                                            "(3) 3 = Not at all" = 0)
) 

# Use dummy coding (0/1) for (No/Yes)
adult_w2 <- adult_w2 %>%  
  mutate(
    smoked_past12M_w2 = recode(smoked_past12M_w2,
                               '(1) 1 = Yes' = 1,
                               '(2) 2 = No' = 0)
) 

#Smoking Status: Generate Dummy Variables then Group by factor (W2)
adult_w2 <- adult_w2 %>% 
              mutate(est_smoker_w2 = if_else(cigarette_num_life_w2 == 6, 1, 0),
                     current_est_smoker_w2 = if_else(R02R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
                     former_est_smoker_w2 = if_else(R02R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes', 1, 0),
                     current_exp_smoker_w2 = if_else(R02R_A_CUR_EXPR_CIGS  == '(1) 1 = Yes', 1, 0),
                     former_exp_smoker_w2 = if_else(R02R_A_FMR_EXPR_CIGS_REV == '(1) 1 = Yes', 1, 0),
                     never_smoker_w2 = if_else(R02R_A_EVR_CIGS == '(2) 2 = No', 1, 0),
                     smoking_status_full_w2 = case_when(
                            current_est_smoker_w2 == 1 ~ 'current_est_smoker',
                            former_est_smoker_w2 == 1 ~ 'former_est_smoker',
                            current_exp_smoker_w2 == 1 ~ 'current_exp_smoker',
                            former_exp_smoker_w2 == 1 ~ 'former_exp_smoker',
                            never_smoker_w2 == 1 ~ 'never_smoker'),
                     smoking_status_w2 = fct_collapse(smoking_status_full_w2,
                                                'current' = c('current_est_smoker', 'current_exp_smoker'),
                                                'former' = c('former_est_smoker', 'former_exp_smoker'))
) 

#Create Quit Variable in DAY units; Create Categorical Variable for abstinence by days quit
adult_w2 <- adult_w2 %>%  
  mutate(days_quit_cigs_w2 = case_when(
                      as.numeric(R02_AC1009_UN)==1 & R02_AC1009_NN>=0 ~ R02_AC1009_NN,
                      as.numeric(R02_AC1009_UN)==2 & R02_AC1009_NN>=0 ~ R02_AC1009_NN * 30.4375,
                      as.numeric(R02_AC1009_UN)==3 & R02_AC1009_NN>=0 ~  R02_AC1009_NN * 365.25),
        abs_w2 = case_when(
                      days_quit_cigs_w2 == 1 ~ 'One Day',
                      days_quit_cigs_w2 >= 2 &  days_quit_cigs_w2 <= 6 ~ 'Two to Six Days',
                      days_quit_cigs_w2 >= 7 &  days_quit_cigs_w2 < 30 ~ 'More than 7 Days',
                      days_quit_cigs_w2 >= 30 &  days_quit_cigs_w2 <= 90 ~ 'One Month',
                      days_quit_cigs_w2 > 90 &  days_quit_cigs_w2 <= 180 ~ 'Three Months',
                      days_quit_cigs_w2 > 180 &  days_quit_cigs_w2 < 365 ~ 'Six Months',
                      days_quit_cigs_w2 >= 365 ~ 'One Year'),
                      abs_w2 = factor(abs_w2, levels = c('One Day', 'Two to Six Days',  
                                                         'More than 7 Days', 'One Month',  
                                                         'Three Months', 'Six Months','One Year'))
)  


#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W2)
adult_w2 <- adult_w2 %>% 
              mutate(race_ethnicity_w2 = case_when(
                race_w2 =='(1) 1 = White alone'&  hispanic_w2 == '(2) 2 = Not Hispanic'~ 'NH White',
                race_w2 =='(2) 2 = Black alone' & hispanic_w2 == '(2) 2 = Not Hispanic' ~ 'NH Black', 
                hispanic_w2 =='(1) 1 = Hispanic' ~ 'Hispanic',
                race_w2=='(3) 3 = Other' & hispanic_w2== '(2) 2 = Not Hispanic' ~ 'Other')
)

# Psychological Variable: R02_AX0161 (Sad) or R02_AX0163 (Anxious) in past month
adult_w2 <- adult_w2 %>% 
  mutate(psychdist_w2 = if_else( 
    as.numeric(R02_AX0161) == 1 | as.numeric(R02_AX0163) == 1, 1, 0)
)


#### WAVE 3: Clean ####

#Rename Varaibles
adult_w3 <- da36498.3001 %>%  
                      rename(gender_w3 = R03R_A_SEX,
                             race_w3 = R03R_A_RACECAT3,
                             hispanic_w3 = R03R_A_HISP,
                             sexual_orientation_w3 = R03R_A_SEXORIENT2,
                             cigarette_current_freq_w3 = R03_AC1003,
                             cigarette_num_life_w3 = R03_AC1005,
                             smoked_past12M_w3 = R03_AC1002_12M
)

#Collapse Education, Income, Age, and Cigarette Use Variables 
adult_w3 <- adult_w3 %>%  
                 mutate(education_w3 = fct_collapse(R03R_A_AM0018,
                           'less_than_hs'= "(1) 1 = Less than High School",
                           'high_school' = c("(2) 2 = GED",
                                             "(3) 3 = High school graduate"),
                           'some_college' =  "(4) 4 = Some college (no degree) or associates degree",
                           "college_or_more" = c("(5) 5 = Bachelor's degree", 
                                                 "advanced_degree" =  "(6) 6 = Advanced degree")),
                        income_w3 = fct_collapse(R03R_A_AM0030,
                             'less_than_25k' =  c("(1) 1 = Less than $10,000",
                                                  "(2) 2 = $10,000 to $24,999"),
                             'btwn_25_to_50k'= "(3) 3 = $25,000 to $49,999",
                             'btwn_50k_100k' = "(4) 4 = $50,000 to $99,999",
                             'more_than_100k' =  "(5) 5 = $100,000 or more"),
                        age_w3 = fct_collapse(R03R_A_AGECAT7,
                            'btwn_18_to_34' = c('(1) 1 = 18 to 24 years old', 
                                                '(2) 2 = 25 to 34 years old'),
                            'btwn_35_to_64' = c('(3) 3 = 35 to 44 years old',
                                                '(4) 4 = 45 to 54 years old',
                                                '(5) 5 = 55 to 64 years old'),
                            'older_than_65' = c('(6) 6 = 65 to 74 years old',
                                                '(7) 7 = 75 years old or older')),
                        cigarette_current_use_w3 = recode(cigarette_current_freq_w3, 
                                                             '(1) 1 = Every day' = 1,
                                                             '(2) 2 = Some days' = 1,
                                                             '(3) 3 = Not at all' = 0)
)

# Use dummy coding (0/1) for (No/Yes)
adult_w3 <- adult_w3 %>%  
    mutate(
      smoked_past12M_w3 = recode(smoked_past12M_w3,
                                 '(1) 1 = Yes' = 1,
                                 '(2) 2 = No' = 0)
) 
                             
        


#Create binary variables for smoking status; then combine into single factor variable (w3)
adult_w3 <- adult_w3 %>% 
              mutate(est_smoker_w3 = if_else(cigarette_num_life_w3 == 6, 1, 0),
                     current_est_smoker_w3 = if_else(R03R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
                     former_est_smoker_w3 = if_else(R03R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes', 1, 0),
                     current_exp_smoker_w3 = if_else(R03R_A_CUR_EXPR_CIGS  == '(1) 1 = Yes', 1, 0),
                     former_exp_smoker_w3 = if_else(R03R_A_FMR_EXPR_CIGS_REV == '(1) 1 = Yes', 1, 0),
                     never_smoker_w3 = if_else(R03R_A_EVR_CIGS == '(2) 2 = No', 1, 0),
                     smoking_status_full_w3 = case_when(
                                           current_est_smoker_w3 == 1 ~ 'current_est_smoker',
                                           former_est_smoker_w3 == 1 ~ 'former_est_smoker',
                                           current_exp_smoker_w3 == 1 ~ 'current_exp_smoker',
                                           former_exp_smoker_w3 == 1 ~ 'former_exp_smoker',
                                           never_smoker_w3 == 1 ~ 'never_smoker'),
                     smoking_status_w3 = fct_collapse(smoking_status_full_w3,
                                          'current' = c('current_est_smoker', 'current_exp_smoker'),
                                          'former' = c('former_est_smoker', 'former_exp_smoker'))
)

#Create Quit Variable in DAY units; Create Categorical Variable for abstinence by days quit
adult_w3 <- adult_w3 %>%
  mutate(days_quit_cigs_w3 = case_when(
                      as.numeric(R03_AC1009_UN)==1 & R03_AC1009_NN>=0 ~ R03_AC1009_NN,
                      as.numeric(R03_AC1009_UN)==2 & R03_AC1009_NN>=0 ~ R03_AC1009_NN * 30.4375,
                      as.numeric(R03_AC1009_UN)==3 & R03_AC1009_NN>=0 ~  R03_AC1009_NN * 365.25),
    abs_w3 = case_when(
      days_quit_cigs_w3 == 1 ~ 'One Day',
                      days_quit_cigs_w3 >= 2 &  days_quit_cigs_w3 <= 6 ~ 'Two to Six Days',
                      days_quit_cigs_w3 >= 7 &  days_quit_cigs_w3 < 30 ~ 'More than 7 Days',
                      days_quit_cigs_w3 >= 30 &  days_quit_cigs_w3 <= 90 ~ 'One Month',
                      days_quit_cigs_w3 > 90 &  days_quit_cigs_w3 <= 180 ~ 'Three Months',
                      days_quit_cigs_w3 > 180 &  days_quit_cigs_w3 < 365 ~ 'Six Months',
                      days_quit_cigs_w3 >= 365 ~ 'One Year'),
    abs_w3 = factor(abs_w3, levels = c('One Day', 'Two to Six Days',  
                                       'More than 7 Days', 'One Month',  
                                       'Three Months', 'Six Months','One Year'))
)

#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W2)
adult_w3 <- adult_w3 %>% 
                  mutate(race_ethnicity_w3 = case_when(
                   race_w3=='(1) 1 = White alone'& hispanic_w3 =='(2) 2 = Not Hispanic'~ 'NH White',
                   race_w3=='(2) 2 = Black alone' & hispanic_w3 =='(2) 2 = Not Hispanic' ~ 'NH Black', 
                   hispanic_w3=='(1) 1 = Hispanic' ~ 'Hispanic',
                   race_w3=='(3) 3 = Other' & hispanic_w3== '(2) 2 = Not Hispanic' ~ 'Other')
)

# Psychological Variable: R03_AX0161 (Sad) or R03_AX0163 (Anxious) in past month
adult_w3 <- adult_w3 %>% 
  mutate(psychdist_w3 = if_else( 
    as.numeric(R03_AX0161) == 1 | as.numeric(R03_AX0163) == 1, 1, 0)
)

#### MERGE WAVES ####
adult_w1$PERSONID <- as.character(adult_w1$PERSONID )
adult_w2$PERSONID <- as.character(adult_w2$PERSONID )
adult_w3$PERSONID <- as.character(adult_w3$PERSONID )


#Note: Must by Full Join; Only use PERSONID for 'by' argument
adult_panel <- adult_w1 %>%  
  full_join(y = adult_w2, by = c('PERSONID')) %>% 
  full_join(y= adult_w3, by = c('PERSONID'))


#### Generate Quit Variables (W2 & W3) ####

#Quit: Yes=stopped smoking, No=stayed smoking,
#Quit Category: Yes=stopped smoking, No=stayed smoking, Else: Stayed Non Smokers

# WAVE 2: Quit Stats for W1 Smokers at W2
adult_panel <- adult_panel %>%
  mutate(quit_w2  = case_when(
    cigarette_current_use_w1== 1 & 
      cigarette_current_use_w2== 1 ~ 'No',
    cigarette_current_use_w1== 1 & 
      (as.numeric(smoked_past12M_w2)==2 | 
         cigarette_current_use_w2== 0) ~ 'Yes'),
    quit_cat_w2  = case_when(
      cigarette_current_use_w1==1 & 
        cigarette_current_use_w2==1 ~ 'No',
      cigarette_current_use_w1==1 & 
        (as.numeric(smoked_past12M_w2)==2 | 
           cigarette_current_use_w2==0) ~ 'Yes',
      cigarette_current_use_w1==0 & 
        (as.numeric(smoked_past12M_w2)==2 | 
           cigarette_current_use_w2==0) ~ 'Stayed Non-Smoker')
)

# WAVE 3: Quit Stats for W1 Current Smokers at W3
adult_panel <- adult_panel %>% 
                  mutate(
                      quit_w3  = case_when(
                            cigarette_current_use_w1==1 & 
                                cigarette_current_use_w3==1 ~  'No',
                            cigarette_current_use_w1==1 & 
                                (as.numeric(smoked_past12M_w3)==2 | 
                                  cigarette_current_use_w3== 0) ~ 'Yes'),
                      quit_cat_w3  = case_when(
                            cigarette_current_use_w1==1 & 
                                  cigarette_current_use_w3==1 ~ 'No',
                           cigarette_current_use_w1==1 & 
                                  (as.numeric(smoked_past12M_w3)==2 | 
                                     cigarette_current_use_w3==0) ~ 'Yes',
                           cigarette_current_use_w1==0 & 
                                  (as.numeric(smoked_past12M_w3)==2 | 
                                  cigarette_current_use_w3==0) ~ 'Stayed Non-Smoker'),
                      quit_cat_w3  = factor(quit_cat_w3,
                                            levels = c('Yes', 'No', 'Stayed Non-Smoker'))
) 
  
                      
#### REMOVE ORIGINAL DATASETS ####
remove(list = c('da36498.1001', 'da36498.2001', 'da36498.3001'))

#### WRITE to CSVs ####
# write.csv(adult_w1, file = 'Output/adult_w1.csv' )
# write.csv(adult_w2, file = 'Output/adult_w2.csv' )
# write.csv(adult_w3, file = 'Output/adult_w3.csv' )
#write.csv(adult_panel, file = 'Output/adult_panel.csv' )

#remove(list = c('adult_w1', 'adult_w2', 'adult_w3', 'adult_panel'))


#### NOTES ####


#NOTE: WAVE 3 missing Poverty and region
#R03R_POVCAT2
#R03X_CB_REGION,

#NOTE: WAVE 3 missing ever smoked a cigarette variable
#R01_AC1002: Ever smoked a cigarette, even one or two puffs


#NOTE: 32 PEOPLE had NA as gender
# adult_w1 %>% 
#   group_by(gender_w1) %>% 
#   count

#NOTE: Multiple measures for age in survey daya 
### R01R_A_AGE: DERIVED - Age (in years) when interviewed
#### Excel table uses raw numbers for age not imputed age (R01R_A_AGECAT7_IMP)

#NOTE: Excel table does not include SOME COLLEGE/AA  category

#NOTE: How should I code the people who aren't estabished, former established or never? 
## EXPERIMENTAL SMOKERS
###: Smokes Now but less than 100 lifetime
###: DOES NOT Smoke Now; less than 100 lifetime; but tried smoking

#NOTE: Does R01_CURRENT_USER_TOBACCO exist in data?

#NOTE: R01R_A: Root for DERIVED Variables 

###NOTE THIS VARIABLE IS IMPORTANT
#R02_CONTINUING_ADULT_LD: Respondent is a continuing adult from Wave 1
#R02_NEW_BASELINE_ADULT_LD: Respondent was a youth at Wave 1 and an adult at Wave 2

#NOTE: CAN WE ADD INFO from YOUTH Survey to WAVE 2 

#NOTE: I am getting 5137 former established smokers in wave 2  but the excel table says 5153


#### DATASETS ####

'
DS-0001: Codebook for Master Linkage File
DS-1001: Codebook for Wave 1: Adult Questionnaire
DS-1002: Codebook for Wave 1: Youth / Parent
DS-2001: Codebook for Wave 2: Adult Questionnaire
DS-2002: Codebook for Wave 2: Youth / Parent
DS-3001: Codebook for Wave 3: Adult
DS-3002: Codebook for Wave 3: Youth/Parent
DS-3101: Codebook for Wave 3: Adult - All-Waves Weights
DS-3102: Codebook for Wave 3: Adult -Single-Wave Weights
DS-3201: Codebook for Wave 3: Youth / Parent - All-Waves Weights
DS-3202: Codebook for Wave 3: Youth / Parent - Single-Wave Weights
'

##### ALGORITHMS TO DERIVE VARIALBES ####


#WAVE 2: Current Established Smokers
#R02R_A_CUR_ESTD_CIGS: DERIVED
#IF ((R01_AC1005 = 6 OR R01_YC1005 = 7 OR R02_AC1005 = 6) AND R02_AC1003 in (1,2)), 
#THEN R02R_A_CUR_ESTD_CIGS =1; 
#ELSE IF R02R_A_EVR_CIGS=2 OR R02_AC1003 =3 OR R02_AC1002_12M=2 OR R02_AC1005 in (1,2,3,4,5), 
#THEN R02R_A_CUR_ESTD_CIGS = 2;
#ELSE IF R02R_A_EVR_CIGS= -99999 OR R02_AC1002_12M= -9 OR R02_AC1005= -9 OR R02_AC1003= -9, 
#THEN R02R_A_CUR_ESTD_CIGS = -99999;
#ELSE IF R02R_A_EVR_CIGS= -99988 OR R02_AC1002_12M= -8 OR R02_AC1005= -8 OR R02_AC1003= -8, 
#THEN R02R_A_CUR_ESTD_CIGS = -99988; 
#ELSE IF R02R_A_EVR_CIGS= -99977 OR R02_AC1002_12M= -7 OR R02_AC1005= -7 OR R02_AC1003= -7, 
#THEN R02R_A_CUR_ESTD_CIGS = -99977; 
#ELSE IF R02R_A_EVR_CIGS= -99966 OR ((R01_AC1005 in (-9,-8,-7,-1) OR R01_YC1005 in (-9,-8,-7,-1)) AND R02_AC1005 != -1), THEN R02R_A_CUR_ESTD_CIGS = -99966;
#ELSE IF R02R_A_EVR_CIGS= -99911 OR R02_AC1002_12M= -1 OR R02_AC1005= -1 OR R02_AC1003= -1,
#THEN R02R_A_CUR_ESTD_CIGS = -99911;


#WAVE 2 EVER SMOKERS (NEVER SMOKERS = 'NO')
#R02R_A_EVR_CIGS: DERIVED - Wave 2 Adult Ever Cigarette Smoker
#Long Description: Wave 2 Adult respondents who have ever smoked a cigarette.
#Algorithm: 
#IF R01R_A_NVR_CIGS= 2 OR R01R_Y_EVR_CIGS = 1 OR R02_AC1004 = 1 OR R02_AC1002_12M=1,
#THEN R02R_A_EVR_CIGS = 1;
#ELSE IF ((R01R_A_NVR_CIGS=1 OR R01R_Y_EVR_CIGS =2) 
#####AND (R02_AC1004 in (2,-7,-8,-9) AND R02_AC1002_12M=2)), 
#THEN R02R_A_EVR_CIGS = 2; 
#ELSE IF (R02_AC1004=-9 AND R02_AC1002_12M=-1) OR R02_AC1002_12M=-9, 
#THEN R02R_A_EVR_CIGS = -99999; 
#ELSE IF R02_AC1002_12M=-8, 
#THEN R02R_A_EVR_CIGS = -99988; 
#ELSE IF R02_AC1002_12M=-7, 
#THEN R02R_A_EVR_CIGS = -99977; 
#ELSE IF R01R_A_NVR_CIGS in (-99999,-99988,-99977,-99955,-99911), 
#####OR R01R_Y_EVR_CIGS in (-99999,-99988,-99977,-99955,-99911), 
#THEN R02R_A_EVR_CIGS = -99966; ELSE IF


#R03R_A_NEW_CIGS: DERIVED - Wave 3 Adult Never to Ever Cigarette Smoker
#Long Description: Wave 3 Adult respondents who started cigarette smoking between Wave 2 and Wave 3.
#IF (R02R_A_EVR_CIGS=2 OR R02R_Y_EVR_CIGS=2) AND (R03_AC1004=1 OR smoked_past12M_w3=1)
#THEN R03R_A_NEW_CIGS=1;
#ELSE IF R02R_A_EVR_CIGS=1 OR R02R_Y_EVR_CIGS=1 OR (smoked_past12M_w3=2 AND WAVE2_INTERVIEW ! = 9) 
#THEN R03R_A_NEW_CIGS=2; 
#ELSE IF (R03_AC1004=-9 AND smoked_past12M_w3=-1) OR smoked_past12M_w3=-9 
#THEN R03R_A_NEW_CIGS = -99999; 
#ELSE IF smoked_past12M_w3=-8 
#THENR03R_A_NEW_CIGS = -99988; 
#ELSE IF smoked_past12M_w3=-7 
#THEN R03R_A_NEW_CIGS = -99977; 
#ELSE IFR02R_A_EVR_CIGS in (-99999,-99988,-99977,-99966,-99911) OR 
#R02R_Y_EVR_CIGS in (-99999,-99988,-99977,-99966,-99911) OR WAVE2_INTERVIEW=9 
#THEN R03R_A_NEW_CIGS = -99966;
#ELSE IF smoked_past12M_w3=-1 
#THEN R03R_A_NEW_CIGS = -99911;


#R03R_A_EVR_CIGS: DERIVED - Wave 3 Adult Ever Cigarette Smoker
#Long Description: Wave 3 Adult respondents who have ever smoked a cigarette.
#IF (R01R_A_NVR_CIGS=2 OR R01R_Y_EVR_CIGS=1 OR R02R_A_EVR_CIGS= 1 OR 
### R02R_Y_EVR_CIGS = 1 OR R03_AC1004 = 1 OR smoked_past12M_w3=1) , 
#THEN R03R_A_EVR_CIGS = 1;
#ELSE IF (((R02R_A_EVR_CIGS=2 OR R02R_Y_EVR_CIGS =2) AND WAVE2_INTERVIEW ! = 9) 
###AND (R01R_A_NVR_CIGS=1 OR R01R_Y_EVR_CIGS=2) AND smoked_past12M_w3=2), 
#THEN R03R_A_EVR_CIGS = 2;
#ELSE IF (R03_AC1004=-9 AND smoked_past12M_w3=-1) OR smoked_past12M_w3=-9, 
###THENR03R_A_EVR_CIGS = -99999; 
#ELSE IF smoked_past12M_w3=-8, 
#THEN R03R_A_EVR_CIGS = -99988;
#ELSE IF smoked_past12M_w3=-7, 
#THEN R03R_A_EVR_CIGS = -99977; 
#ELSE IF R02R_A_EVR_CIGS in (-99999,-99988,-99977,-99966, -99911) 
####OR R02R_Y_EVR_CIGS in (-99999,-99988,-99977,-99966, -99911)
####OR R01R_A_NVR_CIGS in (-99999,-99988,-99977,-99955,-99911) 
####OR R01R_Y_EVR_CIGS in (-99999,-99988,-99977,-99955,-99911) 
####OR WAVE2_INTERVIEW =9, 
#THEN R03R_A_EVR_CIGS = -99966;
#ELSE IF smoked_past12M_w3=-1, 
#THEN R03R_A_EVR_CIGS = -99911;


#R03R_A_CUR_EXPR_CIGS: DERIVED - Wave 3 Adult Current Experimental Cigarette Smoker
#Wave 3 Adult respondents who have not smoked at least 100 cigarettes
#in their lifetime, and currently smoke every day or some days.
#IF (R03R_A_THRSHLD_CIGS=2 AND R03_AC1003 in (1,2)), 
#THEN R03R_A_CUR_EXPR_CIGS=1;
#ELSE IF R03R_A_THRSHLD_CIGS=1 OR R03_AC1003 = 3 OR smoked_past12M_w3 = 2, 
#THENR03R_A_CUR_EXPR_CIGS=2; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99999 OR R03_AC1003 = -9 OR smoked_past12M_w3 = -9, 
#THEN R03R_A_CUR_EXPR_CIGS=-99999; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99988 OR R03_AC1003 = -8 OR smoked_past12M_w3 = -8, 
#THEN R03R_A_CUR_EXPR_CIGS=-99988; 
#ELSE IFR03R_A_THRSHLD_CIGS =-99977 OR R03_AC1003 = -7 OR smoked_past12M_w3 = -7, 
#THENR03R_A_CUR_EXPR_CIGS=-99977; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99966, 
#THENR03R_A_CUR_EXPR_CIGS=-99966; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99911 OR R03_AC1003 = -1 OR smoked_past12M_w3 = -1, 
#THEN R03R_A_CUR_EXPR_CIGS=-99911;

#R03R_A_DAYSQUIT_CIGS: DERIVED - Wave 3 Adult Number of Days Since Last Smoked a Cigarette
#Wave 3 Adult Recode of length of time since last smoked a cigarette into days.
#IF R03_AC1009_UN = 1 AND R03_AC1009_NN => 0 
#THEN R03R_A_DAYSQUIT_CIGS = R03_AC1009_NN;
#ELSE IF R03_AC1009_UN = 2 AND R03_AC1009_NN => 0 
#THEN R03R_A_DAYSQUIT_CIGS = R03_AC1009_NN * 30.4375;
#ELSE IF R03_AC1009_UN = 3 AND R03_AC1009_NN => 0 
#THEN R03R_A_DAYSQUIT_CIGS = R03_AC1009_NN * 365.25;
#ELSE IF R03_AC1009_NN = -9 
#THEN R03R_A_DAYSQUIT_CIGS = -99999; 
#ELSE IF R03_AC1009_NN = -8 
#THENR03R_A_DAYSQUIT_CIGS = -99988; 
#ELSE IF R03_AC1009_NN = -7 
#THEN R03R_A_DAYSQUIT_CIGS = -99977; 
#ELSEIF R03_AC1009_NN = -5 
#THEN R03R_A_DAYSQUIT_CIGS = -99955; 
#ELSE IF R03_AC1009_NN = -1 
#THEN R03R_A_DAYSQUIT_CIGS = -99911;


#R02R_A_FMR_EXPR_CIGS_REV: DERIVED - Wave 2 Adult Former Experimental Cigarette Smoker - Revised Version
#IF R03R_A_EVR_CIGS=1 AND R03R_A_THRSHLD_CIGS=2 AND (R03_AC1003=3 OR R03_AC1002_12M=2) AND R03_AC1010 ! =3 
#THEN R03R_A_FMR_EXPR_CIGS_REV=1; 
#ELSE IF (R03R_A_THRSHLD_CIGS=1 OR R03_AC1003 in (1,2) OR R03R_A_EVR_CIGS=2) AND R03_AC1010 ! =3 
#THEN R03R_A_FMR_EXPR_CIGS_REV=2; 
#ELSE IFR03R_A_THRSHLD_CIGS =-99999 OR R03R_A_EVR_CIGS=-99999 OR R03_AC1003 = -9 OR R03_AC1002_12M = -9
####OR R03_AC1010 =-9 
#THEN R03R_A_FMR_EXPR_CIGS_REV=-99999; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99988 OR R03R_A_EVR_CIGS=-99988 OR R03_AC1003 = -8 OR R03_AC1002_12M = -8 
###OR R03_AC1010 =-8 
#THENR03R_A_FMR_EXPR_CIGS_REV=-99988; 
#ELSE IF R03R_A_THRSHLD_CIGS =-99977 OR R03R_A_EVR_CIGS=-99977 OR R03_AC1003 = -7 OR R03_AC1002_12M = -7 
###OR R03_AC1010 =-7 
#THEN R03R_A_FMR_EXPR_CIGS_REV=-99977;
#ELSE IF R03R_A_THRSHLD_CIGS =-99966 OR R03R_A_EVR_CIGS=-99966 
#THEN R03R_A_FMR_EXPR_CIGS_REV=-99966;
#ELSE IF R03R_A_THRSHLD_CIGS =-99911 OR R03R_A_EVR_CIGS=-99911 OR R03_AC1003 = -1 OR R03_AC1002_12M = -1 OR
####R03_AC1010 in (-1,3) 
#THEN R03R_A_FMR_EXPR_CIGS_REV=-99911;


#### QUIT VARIABLES #####


#R01_AC1009_NN: How long since you completely quit smoking cigarettes - Number
#R01_AC1009_UN: How long since you completely quit smoking cigarettes - Unit



#R03R_A_DAYSQUIT_CIGS: 
#DERIVED - Wave 3 Adult Number of Days Since Last Smoked a Cigarette


#### WAVE 3: DERIVED INCOME VARIABLES ####

#R03_AM0015: 
#In past 30 days, because of shortage of money, 
#were you unable to pay important bills on time


#R03_AM0039:
#In past 12 months, received assistance or income from 
#federal, state or local programs

#R03_AM0030: In past 12 months, total household income

#R03_AM0031: Nonresponse follow-up probe: 
#During past 12 months, total household income above or below $50,000

#R03_AM0036: In past 12 months, parents' total household income

#R03_AM0037: Nonresponse follow-up probe:
#During past 12 months, parents' total household income above or below $50,000


#### Important Variables ####

#DEMOGRAPHIC VARIABLES
#R01R_A_SEX: DERIVED - Gender from the interview
#R01R_A_HISP: DERIVED Hispanic origin from the interview (2 levels)
#R01R_A_SEXORIENT2-  2 category recode of adult sexual orientation
#R01R_POVERTYCAT2: DERIVED based on annual household income and HHS poverty guidelines
#R01X_CB_REGION: DERIVED - Census region
#R01R_A_AGECAT7 in data.frame
#R01R_A_AM0018: Highest grade or level of school completed
#R01R_A_AM0030: Total household income in the past 12 months

#CIGARETTE VARIABLES
#R01_AC1002: Ever smoked a cigarette, even one or two puffs
#R01_AC1003: Now smoke cigarettes
#R01_AC1005: Number of cigarettes smoked in your entire life

#smoked_past12M_w3: In past 12 months, smoked a cigarette, even one or two puffs


#R02R_A_AGE: DERIVED - Wave 2 Adult Age (in years) when interviewed
#NOTE: R02R_A_AGECAT7 in datset

#R02R_A_SEX: DERIVED - Wave 2 Adult Gender
#R02_AM0063: Sexual orientation: DOESN'T Exist???
#R02R_A_SEXORIENT2
#R02R_A_HISP: DERIVED - Wave 2 Adult Hispanic Origin (2 levels)
#R02R_A_RACECAT3: Race ?  
## Also R02R_A_RACE: DERIVED: DOESN"T Exist?????


#R03R_A_FMR_EXPR_CIGS_REV: DERIVED - Wave 3 Adult Former Experimental Cigarette Smoker - Revised Version
#Long Description: Wave 3 adult respondents who have not smoked at least 100 cigarettes
#in their lifetime, and did not smoke in the past 12 months or currently smoke not at all.
#Recanters are excluded from the derivation.

#R02R_A_CUR_ESTD_CIGS: Wave 2 Adult Current Established Cigarette Smoker
#Adult respondents who have smoked at least 100 cigarettes
#AND currently smoke every day or some days.

#R02R_A_FMR_ESTD_CIGS_REV: DERIVED - Wave 2 Adult Former Cigarette Smoker

#R02R_A_EVR_CIGS: DERIVED - Wave 2 Adult Ever Cigarette Smoker
#Includes anyone who has ever smoked


#Create Variable For Smoking Status  (My variable for WAVE 1)
#CURRENT EST. SMOKER = Smoked more than 100 cigarettes in life and Smoke Now
#FORMER EST. SMOKER = Smoked more than 100 cigarettes in life but does NOT Smoke Now
#NEVER SMOKER = Never Smoked Cigarettes

#R03R_A_P30D_CIGS: DERIVED - Wave 3 Adult Past 30 Day Cigarette Smoker

#Derived Collapsed Variables (We created these)
#Collapse Education Factor : Combine GED with HSgrad and bachelor's with adv. degree
#Collapse Income Variable: Combine <10k and <25k
#Collapse Cigarette Variables: Everyday/someday to Yes; No day to no
#Collapse 7 age categories into 3 


#R01_AC1009_NN: How long since you completely quit smoking cigarettes - Number
#R01_AC1009_UN: How long since you completely quit smoking cigarettes - Unit


#### WAVE 1: PSYCHOLOGICAL DISTRESS ####


#R01_AX0161: Last time you had significant problems with: 
#Feeling very trapped, lonely, sad,blue, depressed or hopeless about the future

#R01_AX0162: Last time you had significant problems with: 
#Sleep trouble - such as bad dreams, sleeping restlessly or falling asleep during the day

#R01_AX0163: Last time you had significant problems with: 
#Feeling very anxious, nervous, tense, scared, panicked or like something bad was going to happen

#R01_AX0164: Last time you had significant problems with: 
#Becoming very distressed and upset when something reminded you of the past

#NOTE: Do not know how derived this

#### MISSING VALUE CODES ####

#-99999 = Missing due to data not ascertained on one or more component variables
# -99988 = Missing due to a don't know response on one or more component variables
# -99977 = Missing due to a refused response on one or more component variables
#-99955 = Missing due to an improbable response on one or more component variables
#-99911 = Missing due to an instrument skip pattern for one or more component
#-97777 = Missing due to data removed per respondent request 0 0.0 %


#### ASK SPECIFICATIONS ####


#ASK Specification: R02_AC1009_UN &   R02_AC1009_NN
#How long since last smoked a cigarette - Unit & Number
##IF R02_AC1003 = 3 OR (R02_AC1002_12M = 2 AND (R01R_A_EVER_USER_CIGS = 1 OR R01_YC1002 = 1) 
####AND R01_FORMER_USER_CIGS != 1 AND R01_EXPERIMENTAL_FORMER_CIGS !=1



#### REVISED VARIABLE NOTES ####

##FORMER ESTABLISH SMOKERS == NA: MAYBE THESE ARE THE SMOKERS WHO ARE RECODED AS MISSING

#  current_exp_smoker_w3 = if_else(as.numeric(R03R_A_CUR_ESTD_CIGS) == 2 & 
#(as.numeric(R03R_A_FMR_ESTD_CIGS_REV) == 2 | is.na(R03R_A_FMR_ESTD_CIGS_REV) )&
#as.numeric(R03R_A_EVR_CIGS) == 1, 1, 0)
