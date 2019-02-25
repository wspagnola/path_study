require(tidyverse)
require(stringr)
#### Load in Data ####

#Load Adult Waves 1, 2, 3
load('Data_2310/36498-1001-Data.rda')
load('Data_2310/36498-2001-Data.rda')
load('Data_2310/36498-3001-Data.rda')

#Save Dataframes into object with different name; remove original dataframe
adult_w1 <- da36498.1001
adult_w2 <- da36498.2001
adult_w3 <- da36498.3001
remove(list = c('da36498.1001', 'da36498.2001', 'da36498.3001'))


#### EXPLORE DATA ###

#Explore Structure of Data
#adult_w1 %>%  glimpse 
#Explore Missing data
#adult_w1 %>%  is.na %>% colMeans


#### RENAME WAVE 1 VARIABLES ####

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


#Rename Demographic Variables
adult_w1 <- adult_w1 %>%  rename(gender_w1 = R01R_A_SEX,
                                 race_w1 = R01R_A_RACECAT3,
                                 hispanic_w1 = R01R_A_HISP,
                                 sexual_orientation_w1 = R01R_A_SEXORIENT2,
                                 poverty_w1 =R01R_POVCAT2,
                                 region_w1 = R01X_CB_REGION,
                                 age_w1 = R01R_A_AGECAT7,
                                 education_w1 = R01R_A_AM0018,
                                 income_w1 = R01R_A_AM0030)

#Rename Cigarette Variables
adult_w1 <- adult_w1 %>%  rename(cigarette_use_ever_w1 = R01_AC1002,
                                 cigarette_current_use_w1 = R01_AC1003,
                                 cigarette_num_life_w1 = R01_AC1005)


#### COLLAPSE WAVE 1 VARIABLES ####

#Collapse DEMOGRAPHIC VARIABLES: AGE, EDUCATION, INCOME


#Collapse 7 age categories into 3 
adult_w1 <- adult_w1 %>% 
                mutate(age_w1 = fct_collapse(age_w1,
                    'btwn_18_to_34' = c('(1) 1 = 18 to 24 years old', 
                                        '(2) 2 = 25 to 34 years old'),
                    'btwn_35_to_64' = c('(3) 3 = 35 to 44 years old',
                                        '(4) 4 = 45 to 54 years old',
                                        '(5) 5 = 55 to 64 years old'),
                    'older_than_65' = c('(6) 6 = 65 to 74 years old',
                                        '(7) 7 = 75 years old or older'))
)

#Collapse Education Factor : Combine GED with HSgrad and bachelor's with adv. degree
adult_w1 <- adult_w1 %>%  
              mutate(education_w1 = fct_collapse(education_w1,
                'less_than_hs'= "(1) 1 = Less than High School",
                'high_school' = c("(2) 2 = GED",
                                      "(3) 3 = High school graduate"),
                'some_college' =  "(4) 4 = Some college (no degree) or associates degree",
                "college_or_more" = c("(5) 5 = Bachelor's degree", 
                                      "advanced_degree" =  "(6) 6 = Advanced degree"))
) 

#Collapse Income Variable: Combine <10k and <25k
adult_w1 <- adult_w1 %>% 
              mutate(income_w1 = fct_collapse(income_w1,
                      'less_than_25k' =  c("(1) 1 = Less than $10,000",
                                           "(2) 2 = $10,000 to $24,999"),
                      'btwn_25_to_50k'= "(3) 3 = $25,000 to $49,999",
                      'btwn_50k_100k' = "(4) 4 = $50,000 to $99,999",
                      'more_than_100k' =  "(5) 5 = $100,000 or more"))

#Collapse Cigarette Variables
adult_w1 <- adult_w1 %>%  
  mutate(cigarette_current_use_w1 = fct_collapse(cigarette_current_use_w1, 
                                             'Yes'= c("(1) 1 = Every day",  
                                                      "(2) 2 = Some days" ),
                                             'No' = "(3) 3 = Not at all"),
         cigarette_use_ever_w1 = fct_collapse(cigarette_use_ever_w1,
                                              'Yes' = '(1) 1 = Yes',
                                              'No' = '(2) 2 = No'))
#### WAVE 1: SMOKING STATUS ####

#Create Variable For Smoking Status  
#CURRENT EST. SMOKER = Smoked more than 100 cigarettes in life and Smoke Now
#FORMER EST. SMOKER = Smoked more than 100 cigarettes in life but does NOT Smoke Now
#NEVER SMOKER = Never Smoked Cigarettes

adult_w1 <- adult_w1 %>% 
              mutate(smoking_status_w1 = case_when(
                        cigarette_current_use_w1 == 'Yes' & 
                               as.numeric(cigarette_num_life_w1) == 6 ~ 'est_smoker',
                        cigarette_current_use_w1 == 'No' & 
                               as.numeric(cigarette_num_life_w1) == 6 ~ 'former_est_smoker',
                        cigarette_use_ever_w1 == 'No' ~ 'never_smoker')
)


#### WAVE 1: RACE + ETHNICITY #####
adult_w1 <- adult_w1 %>% 
              mutate(race_ethnicity_w1 = case_when(
                race_w1=='(1) 1 = White alone'& 
                  hispanic_w1 == '(2) 2 = Not Hispanic'~ 'NH White',
                race_w1=='(2) 2 = Black alone' &
                  hispanic_w1 == '(2) 2 = Not Hispanic' ~ 'NH Black', 
                hispanic_w1=='(1) 1 = Hispanic' ~ 'Hispanic',
                race_w1=='(3) 3 = Other' & hispanic_w1== '(2) 2 = Not Hispanic' ~ 'Other')
)


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



#### WAVE 1: SMOKELESS ####

#R01_AS1003SM: Now use smokeless tobacco
levels(adult_w1$R01_AS1003SM)
adult_w1 %>% 
  mutate(smokeless_use_w1 = 
           fct_collapse(adult_w1$R01_AS1003SM,
                        'Yes' = c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(smokeless_use_w1) %>% 
  count()

#### WAVE 1: E-CIGS ####

#R01_AE1002: Ever used an e-cigarette, even one or two times
# R01_AE1003: Now use e-cigarettes
adult_w1 %>% 
  mutate(e_cig_use_w1 = 
           fct_collapse(adult_w1$R01_AE1003,
                        'Yes' = c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(e_cig_use_w1) %>% 
  count()

#### WAVE 1: CIGARILLOS ####

#R01_AG1003CG: Now smoke cigarillos / cigarillos as blunts
adult_w1 %>% 
  mutate(cigarillo_use_w1 =  
           fct_collapse(adult_w1$R01_AG1003CG, 
                        'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(cigarillo_use_w1) %>% 
  count()

## DOESN"T MATCH: 3501 vs. 2969

#### WAVE 1: CIGARS ####

#R01_AG9003: Ever smoked a traditional cigar, even one or two puffs
adult_w1$R01_AG9003 %>% summary() 

#R01_AG1003TC: Now smoke cigars
adult_w1$R01_AG1003TC%>% summary() 

#Any Current Tradional Cigar Use
adult_w1 %>% 
    mutate(cigar_use_w1 =  
         fct_collapse(adult_w1$R01_AG1003TC, 
                      'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                      'No' = "(3) 3 = Not at all")) %>% 
  group_by(cigar_use_w1) %>% 
  count()


#### WAVE 1: Hookah ###

#R01_AH1003: Now smoke a hookah
adult_w1 %>% 
  mutate(hookah_use_w1 = 
          fct_collapse(adult_w1$R01_AH1003, 
                  'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                  'No' = "(3) 3 = Not at all")) %>% 
  group_by(hookah_use_w1) %>% 
  count()


##DOESN'T MATCH!!!! 3049 vs. 3134


#### WAVE 1: PIPE USE ####

#R01_AP1003: Now smoke a pipe filled with tobacco
adult_w1 %>% 
  mutate(filtered_cigar_use_w1 =  
           fct_collapse(adult_w1$R01_AP1003, 
                        'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(filtered_cigar_use_w1) %>% 
  count()


#### WAVE 1: FILTERED CIGARS ####

#R01_AG1005FC: Number of filtered cigars smoked in entire life
#R01_AG1003FC: Now smoke filtered cigars (includes blunts)

adult_w1  %>% 
  mutate(filtered_cigar_use_w1 =  
           fct_collapse(adult_w1$R01_AG1003FC, 
                        'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(filtered_cigar_use_w1) %>% 
  count()

##DOESN'T MATCH!!!! 1301 vs. 1295


#### WAVE 1: SNUS ####


#R01_AS1003SU: Now use snus pouches
adult_w1  %>% 
  mutate(snus_use_w1 =  
           fct_collapse(adult_w1$R01_AS1003SU, 
                        'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(snus_use_w1 ) %>% 
  count()


## DOESN'T MATCH: 482 vs. 479




#### WAVE 1: DISSOLVABLE ####

#R01_AD1003: Now use dissolvable tobacco
adult_w1  %>% 
  mutate(dissolvable_use_w1 =  
           fct_collapse(adult_w1$R01_AD1003, 
                        'Yes'= c("(1) 1 = Every day",  "(2) 2 = Some days" ),
                        'No' = "(3) 3 = Not at all")) %>% 
  group_by(dissolvable_use_w1) %>% 
  count()


#### WAVE 2: RENAME VARIABLES ####

#R02R_A_SEX: DERIVED - Wave 2 Adult Gender
#R02_AM0063: Sexual orientation: DOESN'T Exist???
#R02R_A_SEXORIENT2
#R02R_A_HISP: DERIVED - Wave 2 Adult Hispanic Origin (2 levels)
#R02R_A_RACECAT3: Race ?  
## Also R02R_A_RACE: DERIVED: DOESN"T Exist?????

adult_w2 <- adult_w2 %>% 
                rename(gender_w2 = R02R_A_SEX,
                       race_w2 = R02R_A_RACECAT3,
                       hispanic_w2 = R02R_A_HISP,
                       sexual_orientation_w2 = R02R_A_SEXORIENT2) 

#### WAVE 2: COLLAPSE FACTOR VARIABLES  ####

#R02R_A_AGE: DERIVED - Wave 2 Adult Age (in years) when interviewed
#NOTE: R02R_A_AGECAT7 in datset

#Collapse Age to from 7 to 3 Categories (W2)
adult_w2 <- adult_w2 %>% 
  mutate(age_w2 = fct_collapse(R02R_A_AGECAT7,
                               'btwn_18_to_34' = c('(1) 1 = 18 to 24 years old', 
                                                   '(2) 2 = 25 to 34 years old'),
                               'btwn_35_to_64' = c('(3) 3 = 35 to 44 years old',
                                                   '(4) 4 = 45 to 54 years old',
                                                   '(5) 5 = 55 to 64 years old'),
                               'older_than_65' = c('(6) 6 = 65 to 74 years old',
                                                   '(7) 7 = 75 years old or older'))
)

#Collapse Education (W2)
adult_w2 <- adult_w2 %>%  
  mutate(education_w2 = fct_collapse(R02R_A_AM0018,
                                     'less_than_hs'= "(1) 1 = Less than High School",
                                     'high_school' = c("(2) 2 = GED",
                                                       "(3) 3 = High school graduate"),
                                     'some_college' =  "(4) 4 = Some college (no degree) or associates degree",
                                     "college_or_more" = c("(5) 5 = Bachelor's degree", 
                                                           "advanced_degree" =  "(6) 6 = Advanced degree"))
  ) 

#Collapse Income (W2)
adult_w2 <- adult_w2 %>% 
  mutate(income_w2 = fct_collapse(R02R_A_AM0030,
                                  'less_than_25k' =  c("(1) 1 = Less than $10,000",
                                                       "(2) 2 = $10,000 to $24,999"),
                                  'btwn_25_to_50k'= "(3) 3 = $25,000 to $49,999",
                                  'btwn_50k_100k' = "(4) 4 = $50,000 to $99,999",
                                  'more_than_100k' =  "(5) 5 = $100,000 or more")
  ) 



#### WAVE 2: SMOKING STATUS ####


#R02R_A_CUR_ESTD_CIGS: Wave 2 Adult Current Established Cigarette Smoker
#Adult respondents who have smoked at least 100 cigarettes
#AND currently smoke every day or some days.

#R02R_A_FMR_ESTD_CIGS_REV: DERIVED - Wave 2 Adult Former Cigarette Smoker

#R02R_A_EVR_CIGS: DERIVED - Wave 2 Adult Ever Cigarette Smoker
#Includes anyone who has ever smoked

#Generate Dummy Variable for Established Smoker
adult_w2 <- adult_w2 %>% 
                mutate(est_smoker_w2 = if_else(
                  R02R_A_CUR_ESTD_CIGS == '(1) 1 = Yes', 1, 0)) 

#Generate Dummy Variable for Former Established Smoker
adult_w2 <- adult_w2 %>%  
                mutate(former_est_smoker_w2 = if_else(
                  R02R_A_FMR_ESTD_CIGS_REV == '(1) 1 = Yes', 1, 0)) 
#NOTE: I am getting 5137 but table says 5153

#Generate Dummy Variable for Never Smoker (Zero cigs lifetime)
adult_w2 <- adult_w2 %>% 
      mutate(never_smoker_w2 = if_else(R02R_A_EVR_CIGS == '(2) 2 = No',
                                  1, 0)) 

#Create Factor Variable for Smoking Status
adult_w2 <- adult_w2 %>% 
              mutate(smoking_status_w2 = case_when(
                        est_smoker_w2 == 1 ~ 'est_smoker',
                        former_est_smoker_w2 == 1 ~ 'former_est_smoker',
                        never_smoker_w2 == 1 ~ 'never_smoker')) 
adult_w2 %>% pull(smoking_status_w2) %>%  table

#### WAVE 2: RACE + ETHNICITY #####
adult_w2 <- adult_w2 %>% 
  mutate(race_ethnicity_w2 = case_when(
    race_w2 =='(1) 1 = White alone'& 
      hispanic_w2 == '(2) 2 = Not Hispanic'~ 'NH White',
    race_w2 =='(2) 2 = Black alone' &
      hispanic_w2 == '(2) 2 = Not Hispanic' ~ 'NH Black', 
    hispanic_w2 =='(1) 1 = Hispanic' ~ 'Hispanic',
    race_w2=='(3) 3 = Other' & hispanic_w2== '(2) 2 = Not Hispanic' ~ 'Other')
  )


#### Merge WAVE 1 and WAVE 2 ####
adult_w1$PERSONID <- as.character(adult_w1$PERSONID )
adult_w2$PERSONID <- as.character(adult_w2$PERSONID )
names(adult_w1)[which(names(adult_w1) %in% names(adult_w2))]
adult_w1$PERSONID %in%  adult_w2$PERSONID %>%  sum
adult_w1$CASEID %in%  adult_w2$CASEID %>%  sum

#Note: Must by Full Join; Only use PERSONID for 'by' argument
adult_panel <- adult_w1 %>%  
            full_join(y = adult_w2, by = c('PERSONID'))



#### NOTES ####

#NOTE: 32 PEOPLE had NA as gender
adult_w1 %>% 
  group_by(gender_w1) %>% 
  count

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


