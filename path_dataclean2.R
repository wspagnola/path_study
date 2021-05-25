

#This data cleaning script uses equivalent commands from Stata do file
source('source.R')

#### WAVE 2: Clean ####

#Load Data and Rename Variables (W2)
adult_w2 <- read_tsv('data/Input/36498-2001-Data.tsv')

#Note: no-region or poverty variable

#Rename Variables and mutate PERSONID to character
adult_w2 <- adult_w2 %>% 
              rename(gender_w2 = R02R_A_SEX,
                     race_w2 = R02R_A_RACECAT3,
                     hispanic_w2 = R02R_A_HISP,
                     sexual_orientation_w2 = R02R_A_SEXORIENT2,
                
                     cig_current_freq_w2 = R02_AC1003,
                     cig_num_life_w2 = R02_AC1005,
                     smoked_past12M_w2 = R02_AC1002_12M,
                     smoked_past30D_w2 = R02R_A_P30D_CIGS,
                     attempt_quit_completely = R02_AN0105_01,
                     attempt_quit_reduce = R02_AN0105_02, 
                     attempt_reduce = R02_AN0105_03, 
                     attempt_none = R02_AN0105_04,
                     cig_use_ever_w2 = R02R_A_EVR_CIGS,
                     quit_nonresp_reason_w2 = R02_AC1132,
                     adult_cont_w2 = R02_CONTINUING_ADULT_LD) %>% 
              mutate(PERSONID = as.character(PERSONID)
)

# Filter out respondents who opted out 
adult_w2 <- adult_w2 %>%   filter( adult_cont_w2  != -97777)


# Check missing codes 
adult_w2 %>%  group_by(gender_w2) %>%  count

# Recode Missing Codes to NA
adult_w2  <- adult_w2 %>%  
  mutate(across(where(is.numeric), ~na_if(., -99988)),
         across(where(is.numeric), ~na_if(., -99977)),
         # across(where(is.numeric), ~na_if(., -97777)),
         across(where(is.numeric), ~na_if(., -99999)),
         across(where(is.numeric), ~na_if(., -99966)),
         across(where(is.numeric), ~na_if(.,  -99911))
  )


#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (w2)
adult_w2 <- adult_w2 %>% 
  mutate(race_ethnicity_w2 = case_when(
                                race_w2 == 1 & hispanic_w2 == 2 ~ 'NH White',
                                race_w2 == 2 & hispanic_w2 == 2 ~ 'NH Black', 
                                               hispanic_w2 == 1 ~ 'Hispanic',
                                 race_w2== 3 & hispanic_w2 == 2 ~ 'Other'),
         race_ethnicity_w2 = as.factor(race_ethnicity_w2),
         race_ethnicity_w2 = relevel(race_ethnicity_w2, ref = 'NH White')
)


# Recode Sexual Orientation and Poverty as factors
adult_w2 <- adult_w2 %>%  
  mutate(sexual_orientation_w2 = recode_factor(sexual_orientation_w2, 
                                               `1`='LGBTQIA+', 
                                               `2`='Straight')
)
# NOte: no poverty variable

# ------------ Finish Recoding this ------------#

#Collapse variables: age, income, education, current cigarette use; Recode cigarette ever us
adult_w2 <- adult_w2 %>% 
  mutate(agecat7_w2 = recode_factor(R02R_A_AGECAT7,
                                    `1` = '18 to 24 years old',
                                    `2` = '25 to 34 years old',
                                    `3` = '35 to 44 years old',
                                    `4` = '45 to 54 years old',
                                    `5` = '55 to 64 years old',
                                    `6` = '65 to 74 years old',
                                    `7` = '75 years old or older'),
         age_w2 = fct_collapse(agecat7_w2,
                               'btwn_18_to_34' = c('18 to 24 years old', 
                                                   '25 to 34 years old'),
                               'btwn_35_to_64' = c('35 to 44 years old',
                                                   '45 to 54 years old',
                                                   '55 to 64 years old'),
                               'older_than_65' = c('65 to 74 years old',
                                                   '75 years old or older'))
  )

# Collapse education levels
adult_w2 <- adult_w2 %>% 
  mutate(educat6_w2 = recode_factor(R02R_A_AM0018, 
                                    `1`='Less than High School',
                                    `2`='GED',
                                    `3`='High School graduate',
                                    `4`='Some college (no degree) or associates degree',
                                    `5`="Bachelor's degree",
                                    `6`="Advanced degree"),
         education_w2 = fct_collapse(educat6_w2,
                                     'less_than_hs'= "Less than High School",
                                     'high_school' = c("GED", "High School graduate"),
                                     'some_college' =  "Some college (no degree) or associates degree",
                                     "college_or_more" = c("Bachelor's degree", "Advanced degree"))
  )

adult_w2 <- adult_w2 %>% 
  mutate(incomecat5_w2 = recode(R02R_A_AM0030,
                                `1` = "Less than $10,000",
                                `2` = "$10,000 to $24,999",
                                `3` = "$25,000 to $49,999",
                                `4` = "$50,000 to $99,999",
                                `5` = "$100,000 or more"),
         income_w2 = fct_collapse(incomecat5_w2,
                                  'less_than_25k' =  c("Less than $10,000", "$10,000 to $24,999"),
                                  'btwn_25_to_50k'= "$25,000 to $49,999",
                                  'btwn_50k_100k' = "$50,000 to $99,999",
                                  'more_than_100k' =  "$100,000 or more")
  )

adult_w2 <- adult_w2 %>% 
  mutate(cig_use_now_w2 = recode(cig_current_freq_w2, `1`  = 1,  `2`  = 1, `3`  = 0),
         cig_use_ever_w2 = recode_binary(cig_use_ever_w2)
  )


adult_w1 %>%  count(cig_current_freq_w1)
adult_w2 %>%  count(cig_current_freq_w2)
adult_w2 %>%  count(cig_use_now_w2)
adult_w2 %>%  count(cig_num_life_w2) #R02_AC1005,

#Create Smoking Status Factor Variable and Binary Variables 
## est_smoker = established smoker (current & former); smoked 100 cigs in lifetime
#Smoking Status Full has all categories cur/fmr est/exp smoker and non-smoers
#Smoking Status collapsed smoking status full into current, former, never


# Note: need to account for established smokers from previous wave


# Psychological Variable: R02_AX0161 (Sad) or R02_AX0163 (Anxious) in past month
adult_w2 <- adult_w2 %>% 
  mutate(psychdist_w2 = if_else( 
    as.numeric(R02_AX0161) == 1 | as.numeric(R02_AX0163) == 1, 1, 0)
  )

adult_w2$wave_2 <- 1

#### Check ####

adult_w2 %>%  names %>%  str_subset('EVR')
adult_w2 %>%  names %>%  str_subset('NVR')

