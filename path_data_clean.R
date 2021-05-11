
#This data cleaning script uses equivalent commands from Stata do file
source('source.R')

#STATA <- TRUE  #TRUE to match STAT file
STATA <- FALSE  #TRUE to match STAT file

#### WAVE 1: Clean ####

#Load Adult Waves 1
#load('Input/36498-1001-Data.rda')

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


# Recode NAs 
adult_w1  <- adult_w1 %>%  
    mutate(across(where(is.numeric), ~na_if(., -99988)),
           across(where(is.numeric), ~na_if(., -99977)),
           across(where(is.numeric), ~na_if(.,-97777))
           
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

#Create Smoking Status Factor Variable and Binary Variables 
## est_smoker = established smoker (current & former); smoked 100 cigs in lifetime
#Smoking Status Full has all categories cur/fmr est/exp smoker and non-smoers
#Smoking Status collapsed smoking status full into current, former, never
adult_w1 <- adult_w1 %>%  
              mutate(est_smoker_w1 = if_else(as.numeric(cig_num_life_w1) == 6, 1, 0),
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


# Psychological Variable: R01_AX0161 (Sad) or R01_AX0163 (Anxious) in past month
adult_w1 <- adult_w1 %>% 
  mutate(psychdist_w1 = if_else( 
    as.numeric(R01_AX0161) == 1 | as.numeric(R01_AX0163) == 1, 1, 0)
)

adult_w1$wave_1 <- 1


#### WAVE 2: Clean ####

#Load Data and Rename Variables (W2)
# load('Input/36498-2001-Data.rda')
adult_w2 <- read_tsv('data/Input/36498-2001-Data.tsv')

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
         cig_use_ever_w2 = R02R_A_EVR_CIGS) %>% 
  mutate(PERSONID = as.character(PERSONID)
)

#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W2)
adult_w2 <- adult_w2 %>% 
  mutate(race_ethnicity_w2 = case_when(
    race_w2 == 1 &  hispanic_w2 == 2 ~ 'NH White',
    race_w2 == 2 & hispanic_w2 == 2  ~ 'NH Black', 
    hispanic_w2 == 1  ~ 'Hispanic',
    race_w2== 3 & hispanic_w2 ==  2~ 'Other')
)


#-------------- NOTE: NEED TO CONVERT THESE TO FACTORS -------------*/

#Recode/Relevel Race and Sexual Orientation Variables
adult_w2 <- adult_w2 %>% 
      mutate(
          race_ethnicity_w2 = as.factor(race_ethnicity_w2),
          race_ethnicity_w2 = relevel(race_ethnicity_w2, ref = 'NH White'),
          sexual_orientation_w2 = recode(sexual_orientation_w2,
                               '(1) 1 = Lesbian, Gay, Bisexual, Something else' = 'LGBT',
                               '(2) 2 = Straight' = 'Straight'),
          sexual_orientation_w2 = relevel(sexual_orientation_w2, ref = 'Straight')
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
         cig_use_now_w2 = recode(cig_current_freq_w2, 
                                           "(1) 1 = Every day" = 1,  
                                           "(2) 2 = Some days" = 1,
                                           "(3) 3 = Not at all" = 0)
) 

# Use dummy coding (0/1) for (No/Yes)
adult_w2 <- adult_w2 %>%  
  mutate(
    smoked_past12M_w2 = recode_binary(smoked_past12M_w2),
    smoked_past30D_w2 = recode_binary(smoked_past30D_w2)
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

#Recode Quit Attempt Variables as binary 
adult_w2 <- adult_w2 %>% 
              mutate_if(.predicate = grepl('^attempt', names(adult_w2)), .funs = recode_multi_choice
) 

# Psychological Variable: R02_AX0161 (Sad) or R02_AX0163 (Anxious) in past month
adult_w2 <- adult_w2 %>% 
  mutate(psychdist_w2 = if_else( 
    as.numeric(R02_AX0161) == 1 | as.numeric(R02_AX0163) == 1, 1, 0)
)

adult_w2$wave_2 <- 1

#### WAVE 3: Clean ####

load('Input/36498-3001-Data.rda')

#Rename Variables
adult_w3 <- da36498.3001 %>%  
  rename(gender_w3 = R03R_A_SEX,
         race_w3 = R03R_A_RACECAT3,
         hispanic_w3 = R03R_A_HISP,
         sexual_orientation_w3 = R03R_A_SEXORIENT2,
         cig_current_freq_w3 = R03_AC1003,
         cig_num_life_w3 = R03_AC1005,
         smoked_past12M_w3 = R03_AC1002_12M,
         smoked_past30D_w3 = R03R_A_P30D_CIGS,
         attempt_quit = R03_AN0105,
         attempt_quit_reduce = R03_AN0334,
         cig_use_ever_w3 = R03R_A_EVR_CIGS) %>% 
  mutate(PERSONID = as.character(PERSONID)
)

#Race/Ethnicity Variable: NH-White, NH-black, Hispanic, Other (W2)
adult_w3 <- adult_w3 %>% 
  mutate(race_ethnicity_w3 = case_when(
    race_w3=='(1) 1 = White alone'& hispanic_w3 =='(2) 2 = Not Hispanic'~ 'NH White',
    race_w3=='(2) 2 = Black alone' & hispanic_w3 =='(2) 2 = Not Hispanic' ~ 'NH Black', 
    hispanic_w3=='(1) 1 = Hispanic' ~ 'Hispanic',
    race_w3=='(3) 3 = Other' & hispanic_w3== '(2) 2 = Not Hispanic' ~ 'Other')
)

#Recode/Relevel Race, Sexual Orientation, Region, and Poverty Variables
adult_w3 <- adult_w3 %>% 
  mutate(
    race_ethnicity_w3 = as.factor(race_ethnicity_w3),
    race_ethnicity_w3 = relevel(race_ethnicity_w3, ref = 'NH White'),
    sexual_orientation_w3 = recode(sexual_orientation_w3,
                                   '(1) 1 = Lesbian, Gay, Bisexual, Something else' = 'LGBT',
                                   '(2) 2 = Straight' = 'Straight'),
    sexual_orientation_w3 = relevel(sexual_orientation_w3, ref = 'Straight')
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
         cig_use_now_w3 = recode(cig_current_freq_w3, 
                                           '(1) 1 = Every day' = 1,
                                           '(2) 2 = Some days' = 1,
                                           '(3) 3 = Not at all' = 0)
)

# Use dummy coding (0/1) for (No/Yes)
adult_w3 <- adult_w3 %>%  
                mutate(
                    smoked_past12M_w3 = recode_binary(smoked_past12M_w3),
                    smoked_past30D_w3 = recode_binary(smoked_past30D_w3)
)

#Create binary variables for smoking status; then combine into single factor variable (w3)
adult_w3 <- adult_w3 %>% 
  mutate(est_smoker_w3 = if_else(cig_num_life_w3 == 6, 1, 0),
         current_est_smoker_w3 = if_else(R03R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
         never_smoker_w3 = if_else(cig_use_ever_w3 == '(2) 2 = No', 1, 0),
         former_est_smoker_w3 = if_else(R03R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes' , 1, 0),
         current_non_est_smoker_w3 = if_else(as.numeric(R03R_A_CUR_ESTD_CIGS) == 2 &
                                               (as.numeric(R03R_A_FMR_ESTD_CIGS_REV) == 2 |
                                                  is.na(R03R_A_FMR_ESTD_CIGS_REV)) &
                                               as.numeric(cig_use_ever_w3) ==1, 1, 0),
         smoking_status_w3 = case_when(
           current_est_smoker_w3 == 1 ~ 'current_est_smoker',
           former_est_smoker_w3 == 1 ~ 'former_est_smoker',
           current_non_est_smoker_w3 == 1 ~ 'current_non_est_smoker_w3',
           never_smoker_w3 == 1 ~ 'never_smoker')
)


#Recode Quit Attemp Variables as binary
adult_w3 <- adult_w3 %>% 
  mutate_if(.predicate = grepl('^attempt', names(adult_w3)), 
            .funs = recode_binary
) 

# Psychological Variable: R03_AX0161 (Sad) or R03_AX0163 (Anxious) in past month
adult_w3 <- adult_w3 %>% 
  mutate(psychdist_w3 = if_else( 
    as.numeric(R03_AX0161) == 1 | as.numeric(R03_AX0163) == 1, 1, 0)
)


adult_w3$wave_3 <- 1
#### MERGE WAVES ####

#Note: Must by Full Join; Only use PERSONID for 'by' argument
adult_panel <- adult_w1 %>%  
  full_join(y = adult_w2, by = c('PERSONID')) %>% 
  full_join(y= adult_w3, by = c('PERSONID')
)


#### Generate Quit Variables (W2 & W3) ####



#Quit: Yes=stopped smoking, No=stayed smoking,
#Quit Category: Yes=stopped smoking, No=stayed smoking, Else: Stayed Non Smokers

# WAVE 2: Quit Status at W2 for 'W1 Current Smokers'
adult_panel <- adult_panel %>%
  mutate(quit_w1_w2  = case_when(
    cig_use_now_w1== 1 & 
      cig_use_now_w2== 1 ~ 'No',
    cig_use_now_w1== 1 & 
      (smoked_past12M_w2== 0 | 
         cig_use_now_w2== 0) ~ 'Yes'),
    quit_cat_w1_w2  = case_when(
      cig_use_now_w1==1 & 
        cig_use_now_w2==1 ~ 'No',
      cig_use_now_w1==1 & 
        (as.numeric(smoked_past12M_w2)==2 | 
           cig_use_now_w2==0) ~ 'Yes',
      cig_use_now_w1==0 & 
        (as.numeric(smoked_past12M_w2)==2 | 
           cig_use_now_w2==0) ~ 'Stayed Non-Smoker')
)

# WAVE 3: Quit Status  at W3 for 'W1 Current Smokers'
adult_panel <- adult_panel %>% 
  mutate(
    quit_w1_w3  = case_when(
      cig_use_now_w1==1 & cig_use_now_w3==1 ~  'No',
      cig_use_now_w1==1 & (smoked_past12M_w3==0 | cig_use_now_w3== 0) ~ 'Yes'),
    quit_cat_w1_w3  = case_when(
      cig_use_now_w1==1 & cig_use_now_w3==1 ~ 'No',
      cig_use_now_w1==1 & (smoked_past12M_w3 == 1 | cig_use_now_w3==0) ~ 'Yes',
      cig_use_now_w1==0 & (smoked_past12M_w3==1 | cig_use_now_w3==0) ~ 'Stayed Non-Smoker'),
    quit_cat_w1_w3  = factor(quit_cat_w1_w3, levels = c('Yes', 'No', 'Stayed Non-Smoker'))
) 

# WAVE 3: Quit Status at W3 for 'W2 Current Smokers' 
adult_panel <- adult_panel %>% 
  mutate(
    quit_w2_w3  = case_when(
      cig_use_now_w2==1 & cig_use_now_w3==1 ~  'No',
      cig_use_now_w2==1 & (smoked_past12M_w3==0 | cig_use_now_w3== 0) ~ 'Yes'),
    quit_cat_w2_w3  = case_when(
      cig_use_now_w2==1 & cig_use_now_w3==1 ~ 'No',
      cig_use_now_w2==1 & (smoked_past12M_w3 == 1 | cig_use_now_w3==0) ~ 'Yes',
      cig_use_now_w2==0 & (smoked_past12M_w3==1 | cig_use_now_w3==0) ~ 'Stayed Non-Smoker'),
    quit_cat_w2_w3  = factor(quit_cat_w2_w3, levels = c('Yes', 'No', 'Stayed Non-Smoker'))
) 

# WAVE 3: P30D Quit Status at W3 for 'W2 Current Smokers' 
adult_panel <- adult_panel %>% 
                  mutate( quit_p30d_w2_w3  = case_when(
  cig_use_now_w2==1 & cig_use_now_w3==1 ~  'No',
  cig_use_now_w2==1 & (smoked_past12M_w3==0 | cig_use_now_w3== 0) ~ 'Yes')
)

# gen quitw2_2=1 if (smkstatus_w1==1) & (R02_AC1003==3 & p30cigsmoke_w2==0)
# replace quitw2_2=0 if (smkstatus_w1==1) & (R02_AC1003>=1 & R02_AC1003<3) & (p30cigsmoke_w2==1)

#### REMOVE ORIGINAL DATASETS ####
remove(list = c('da36498.1001', 'da36498.2001', 'da36498.3001'))


#Recode NAs as Zero for obs. that was not included in a given wave
adult_panel <- adult_panel %>% 
                    mutate(wave_1 = ifelse(is.na(wave_1), 0, wave_1),
                           wave_2 = ifelse(is.na(wave_2), 0, wave_2),
                           wave_3 = ifelse(is.na(wave_3), 0, wave_3)
) 


#### Old Code  ####


#load('Input/36498-1001-Data.rda')



#STATA <- TRUE  #TRUE to match STAT file
#STATA <- FALSE  #TRUE to match STAT file


if(STATA){
  adult_w1 <- adult_w1 %>%  
    mutate(
      est_smoker_w1 = if_else(as.numeric(cig_num_life_w1) == 6, 1, 0),
      current_est_smoker_w1 = if_else(R01R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
      never_smoker_w1 = if_else(R01R_A_NVR_CIGS == '(1) 1 = Yes', 1, 0),
      former_est_smoker_w1 = if_else(R01R_A_FMR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
      current_non_est_smoker_w1 = if_else(as.numeric(R01R_A_CUR_ESTD_CIGS) == 2 &
                                            (as.numeric(R01R_A_FMR_ESTD_CIGS) == 2 |
                                               is.na(R01R_A_FMR_ESTD_CIGS)) &
                                            as.numeric(R01R_A_NVR_CIGS) == 2, 1, 0),
      smoking_status_w1 = case_when(
        current_est_smoker_w1== 1 ~ 'current_est_smoker',
        never_smoker_w1  == 1 ~ 'never_smoker',
        former_est_smoker_w1 == 1 ~ 'former_est_smoker',
        current_non_est_smoker_w1 == 1 ~ 'current_non_est_smoker')
    )
} else {
  adult_w1 <- adult_w1 %>%  
    mutate(
      est_smoker_w1 = if_else(as.numeric(cig_num_life_w1) == 6, 1, 0),
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
}



#Smoking Status: Generate Dummy Variables then Group by factor (W2)
if (STATA){
  adult_w2 <- adult_w2 %>% 
    mutate(est_smoker_w2 = if_else(cig_num_life_w2 == 6, 1, 0),
           current_est_smoker_w2 = if_else(R02R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
           never_smoker_w2 = if_else(cig_use_ever_w2 == '(2) 2 = No', 1, 0),
           former_est_smoker_w2 = if_else(R02R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes' |
                                            is.na(R02R_A_FMR_ESTD_CIGS_REV) &
                                            is.na(est_smoker_w2) &
                                            is.na(cig_use_now_w2) &
                                            (is.na(R02R_A_CUR_ESTD_CIGS) |
                                               is.na(cig_use_ever_w2)), 1, 0),
           current_non_est_smoker_w2 = if_else(as.numeric(R02R_A_CUR_ESTD_CIGS) == 2 &
                                                 (as.numeric(R02R_A_FMR_ESTD_CIGS_REV) == 2 |
                                                    is.na(R02R_A_FMR_ESTD_CIGS_REV)) &
                                                 as.numeric(cig_use_ever_w2) ==1, 1, 0),
           smoking_status_w2 = case_when(
             current_est_smoker_w2 == 1 ~ 'current_est_smoker',
             former_est_smoker_w2 == 1 ~ 'former_est_smoker',
             current_non_est_smoker_w2 == 1 ~ 'current_non_est_smoker',
             never_smoker_w2 == 1 ~ 'never_smoker')
    )
} else {
  adult_w2 <- adult_w2 %>% 
    mutate(est_smoker_w2 = if_else(cig_num_life_w2 == 6, 1, 0),
           current_est_smoker_w2 = if_else(R02R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
           former_est_smoker_w2 = if_else(R02R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes', 1, 0),
           current_exp_smoker_w2 = if_else(R02R_A_CUR_EXPR_CIGS  == '(1) 1 = Yes', 1, 0),
           former_exp_smoker_w2 = if_else(R02R_A_FMR_EXPR_CIGS_REV == '(1) 1 = Yes', 1, 0),
           never_smoker_w2 = if_else(cig_use_ever_w2 == '(2) 2 = No', 1, 0),
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
}




#Create Quit Variable in DAY units; Create Categorical Variable for abstinence by days quit
if(STATA){
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
} else{
  adult_w3 <- adult_w3 %>% 
    mutate(est_smoker_w3 = if_else(cig_num_life_w3 == 6, 1, 0),
           current_est_smoker_w3 = if_else(R03R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0),
           former_est_smoker_w3 = if_else(R03R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes', 1, 0),
           current_exp_smoker_w3 = if_else(R03R_A_CUR_EXPR_CIGS  == '(1) 1 = Yes', 1, 0),
           former_exp_smoker_w3 = if_else(R03R_A_FMR_EXPR_CIGS_REV == '(1) 1 = Yes', 1, 0),
           never_smoker_w3 = if_else(cig_use_ever_w3 == '(2) 2 = No', 1, 0),
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
}


