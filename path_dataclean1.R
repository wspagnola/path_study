
#This data cleaning script uses equivalent commands from Stata do file
source('source.R')


#### WAVE 1: Clean ####

#Load Adult Waves 1
adult_w1 <- read_tsv('data/Input/36498-1001-Data.tsv')

#Rename Variables (W1) and mutate PERSONID to character
adult_w1 <- adult_w1  %>%  
  rename(gender_w1 = R01R_A_SEX,
         race_w1 = R01R_A_RACECAT3,
         hispanic_w1 = R01R_A_HISP,
         sexual_orientation_w1 = R01R_A_SEXORIENT2,
         poverty_w1 =R01R_POVCAT2,
         region_w1 = R01X_CB_REGION,
         cig_use_ever_w1 = R01_AC1002,
         cig_current_freq_w1 = R01_AC1003,
         cig_num_life_w1 = R01_AC1005) %>% 
  mutate(PERSONID = as.character(PERSONID)
  )

#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W1)
adult_w1 <- adult_w1 %>% 
  mutate(race_ethnicity_w1 = case_when(
    race_w1 == 1 & hispanic_w1 == 2  ~ 'NH White',
    race_w1 == 2 & hispanic_w1 == 2 ~ 'NH Black', 
    hispanic_w1 == 1   ~ 'Hispanic',
    race_w1== 3 & hispanic_w1 == 2 ~ 'Other')
  )


#Recode/Relevel Race, Sexual Orientation, Region, and Poverty Variables
adult_w1 <- adult_w1 %>% 
  mutate(race_ethnicity_w1 = as.factor(race_ethnicity_w1),
         race_ethnicity_w1 = relevel(race_ethnicity_w1, ref = 'NH White')
  )

# Recode Missing Codes to NA
adult_w1  <- adult_w1 %>%  
  mutate(across(where(is.numeric), ~na_if(., -99988)),
         across(where(is.numeric), ~na_if(., -99977)),
         # across(where(is.numeric), ~na_if(., -97777)),
         across(where(is.numeric), ~na_if(., -99999)),
         across(where(is.numeric), ~na_if(., -99966)),
         across(where(is.numeric), ~na_if(.,  -99911))
  )



adult_w1 <- adult_w1 %>%  
  mutate(sexual_orientation_w1 = recode_factor(sexual_orientation_w1, 
                                               `1`='LGBTQIA+', 
                                               `2`='Straight'),
         poverty_w1 = recode_factor(poverty_w1, 
                                    `1`="Below poverty", 
                                    `2`='At or above poverty')
  )


#Collapse variables: age, income, education, current cigarette use; Recode cigarette ever us
adult_w1 <- adult_w1 %>% 
  mutate(agecat7_w1 = recode_factor(R01R_A_AGECAT7,
                                    `1` = '18 to 24 years old',
                                    `2` = '25 to 34 years old',
                                    `3` = '35 to 44 years old',
                                    `4` = '45 to 54 years old',
                                    `5` = '55 to 64 years old',
                                    `6` = '65 to 74 years old',
                                    `7` = '75 years old or older'),
         age_w1 = fct_collapse(agecat7_w1,
                               'btwn_18_to_34' = c('18 to 24 years old', 
                                                   '25 to 34 years old'),
                               'btwn_35_to_64' = c('35 to 44 years old',
                                                   '45 to 54 years old',
                                                   '55 to 64 years old'),
                               'older_than_65' = c('65 to 74 years old',
                                                   '75 years old or older'))
  )

# Collapse education levels
adult_w1 <- adult_w1 %>% 
  mutate(educat6_w1 = recode_factor(R01R_A_AM0018, 
                                    `1`='Less than High School',
                                    `2`='GED',
                                    `3`='High School graduate',
                                    `4`='Some college (no degree) or associates degree',
                                    `5`="Bachelor's degree",
                                    `6`="Advanced degree"),
         education_w1 = fct_collapse(educat6_w1,
                                     'less_than_hs'= "Less than High School",
                                     'high_school' = c("GED", "High School graduate"),
                                     'some_college' =  "Some college (no degree) or associates degree",
                                     "college_or_more" = c("Bachelor's degree", "Advanced degree"))
  )

adult_w1 <- adult_w1 %>% 
  mutate(incomecat5_w1 = recode(R01R_A_AM0030,
                                `1` = "Less than $10,000",
                                `2` = "$10,000 to $24,999",
                                `3` = "$25,000 to $49,999",
                                `4` = "$50,000 to $99,999",
                                `5` = "$100,000 or more"),
         income_w1 = fct_collapse(incomecat5_w1,
                                  'less_than_25k' =  c("Less than $10,000", "$10,000 to $24,999"),
                                  'btwn_25_to_50k'= "$25,000 to $49,999",
                                  'btwn_50k_100k' = "$50,000 to $99,999",
                                  'more_than_100k' =  "$100,000 or more")
  )

adult_w1 <- adult_w1 %>% 
  mutate(cig_use_now_w1 = recode(cig_current_freq_w1, `1`  = 1,  `2`  = 1, `3`  = 0),
         cig_use_ever_w1 = recode_binary(cig_use_ever_w1)
  )

adult_w1 %>%  count(cig_current_freq_w1)
adult_w1 %>%  count(cig_use_now_w1)

#Create Smoking Status Factor Variable and Binary Variables 
## est_smoker = established smoker (current & former); smoked 100 cigs in lifetime
#Smoking Status Full has all categories cur/fmr est/exp smoker and non-smoers
#Smoking Status collapsed smoking status full into current, former, never
adult_w1 <- adult_w1 %>%  
               mutate(est_smoker_w1 = case_when(                       cig_num_life_w1 == 6 ~ 1,
                                                cig_num_life_w1 >= 1 & cig_num_life_w1 <= 5 ~ 0),
             smoking_status_full_w1 = case_when(
                                         cig_use_now_w1 == 1 & est_smoker_w1 == 1 ~ 'current_est_smoker',
                                         cig_use_now_w1 == 0 & est_smoker_w1 == 1 ~ 'former_est_smoker',
                                         cig_use_now_w1 == 1 & est_smoker_w1 == 0 ~ 'current_exp_smoker',
                                         cig_use_now_w1 == 0 & est_smoker_w1 == 0 &
                                                             cig_use_ever_w1 == 1 ~ 'former_exp_smoker',
                                                             cig_use_ever_w1 == 0 ~'never_smoker'), 
             smoking_status_w1 = fct_collapse(smoking_status_full_w1,
                                              'current' = c('current_est_smoker', 'current_exp_smoker'),
                                              'former' = c('former_est_smoker', 'former_exp_smoker')),
             current_est_smoker_w1 = if_else(smoking_status_full_w1 == 'current_est_smoker', 1, 0),
             former_est_smoker_w1 = if_else(smoking_status_full_w1 == 'former_est_smoker', 1, 0),
             current_exp_smoker_w1 = if_else(smoking_status_full_w1 == 'current_exp_smoker', 1, 0),
             former_exp_smoker_w1 = if_else(smoking_status_full_w1 == 'former_exp_smoker', 1, 0),
             never_smoker_w1 = if_else(smoking_status_full_w1 == 'never_smoker', 1, 0)
  )
adult_w1 %>%  count(smoking_status_full_w1, est_smoker_w1)
adult_w1 %>%  count(est_smoker_w1)
adult_w1 %>%  count(cig_use_now_w1)

# Psychological Variable: R01_AX0161 (Sad) or R01_AX0163 (Anxious) in past month
adult_w1 <- adult_w1 %>% 
  mutate(psychdist_w1 = if_else( 
    as.numeric(R01_AX0161) == 1 | as.numeric(R01_AX0163) == 1, 1, 0)
  )

adult_w1$wave_1 <- 1



#### Check ####


# cig_use_ever_w1 = R01_AC1002
# cig_current_freq_w1 = R01_AC1003
# cig_num_life_w1 = R01_AC1005
adult_w1 %>%  names %>%  str_subset('EVR')
adult_w1 %>%  names %>%  str_subset('NVR')

adult_w1 %>% 
  filter(current_exp_smoker_w1==1) %>% 
  count(cig_use_ever_w1, cig_current_freq_w1, cig_num_life_w1) %>% 
  View
  
adult_w1 %>%  count(current_exp_smoker_w1)
adult_w1 %>%  count(former_exp_smoker_w1)

adult_w1 %>% 
  filter(cig_use_ever_w1 >= 0, 
         cig_current_freq_w1 %in% c(1,2), 
         cig_num_life_w1 <= 5 & cig_num_life_w1 >= 1) %>% 
  count(current_exp_smoker_w1)

adult_w1 %>% 
  count(cig_use_ever_w1, cig_current_freq_w1, cig_num_life_w1)  %>% 
  View
