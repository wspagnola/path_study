#### Codes ####


#R01_AC1002: Ever smoked a cigarette, even one or two puffs
#R01_AC0100: When last smoked a cigarette
#R01_AC1003: Now smoke cigarettes
#R01_AC1005: Number of cigarettes smoked in your entire life


### Current Established Smoker #### 

# R01R_A_CUR_ESTD_CIGS: DERIVED - Wave 1 Adult Current Established Cigarette User
# Long Description: 
#1) Has ever smoked a cigarette
#2) as smoked more than 100 cigarettes in lifetime
#3)and currently smokes every day or some days.

# Algorithm:
#   IF R01_AC1002=1 AND R01_AC1005=6 AND R01_AC1003 in (1, 2) THEN R01R_A_CUR_ESTD_CIGS = 1; 

#ELSE IF R01_AC1002= 2 OR R01_AC1003= 3 OR (R01_AC1003 in (1,2,-7,-8,-9) AND R01_AC1005 in (1, 2, 3, 4, 5)) , THEN R01R_A_CUR_ESTD_CIGS =2;

#ELSE IF R01_AC1002=-9 OR R01_AC1005=-9  OR R01_AC1003=-9 THEN R01R_A_CUR_ESTD_CIGS=-99999 

#ELSE IF R01_AC1002=-8 OR R01_AC1005=-8 OR R01_AC1003=-8 THEN R01R_A_CUR_ESTD_CIGS=-99988 

#ELSE IF R01_AC1002=-7 OR R01_AC1005=-7 OR R01_AC1003=-7 THEN R01R_A_CUR_ESTD_CIGS=-99977 ELSE IF R01_AC1002=-5 OR R01_AC1005=-5 OR R01_AC1003=-5 THEN R01R_A_CUR_ESTD_CIGS=-99955 

#ELSE IF R01_AC1002=-1 OR R01_AC1005=-1 OR R01_AC1003=-1 THEN R01R_A_CUR_ESTD_CIGS=-99911


# R01R_A_CUR_EXPR_CIGS: DERIVED - Wave 1 Adult Current Experimental Cigarette User
#Long Description: Has ever smoked a cigarette, has not smoked more than 100 cigarettes in lifetime, and smokes every day or some days.

# Algorithm:
#   IF R01_AC1002=1 AND R01_AC1005 in (1, 2, 3, 4, 5) AND R01_AC1003 in (1, 2) THEN R01R_A_CUR_EXPR_CIGS = 1; ELSE
# IF R01_AC1002= 2 OR R01_AC1003= 3 OR (R01_AC1003 in (1,2,-7,-8,-9) AND
#                                       - 1092 -
#                                         R01_AC1005 = 6), THEN R01R_A_CUR_EXPR_CIGS=2;
# ELSE IF R01_AC1002=-9 OR R01_AC1005=-9 OR R01_AC1003=-9 THEN R01R_A_CUR_EXPR_CIGS=-99999 ELSE IF R01_AC1002=-8
# OR R01_AC1005=-8 OR R01_AC1003=-8 THEN R01R_A_CUR_EXPR_CIGS=-99988 ELSE IF R01_AC1002=-7 OR R01_AC1005=-7 OR
# R01_AC1003=-7 THEN R01R_A_CUR_EXPR_CIGS=-99977 ELSE IF R01_AC1002=-5
# OR R01_AC1005=-5 OR R01_AC1003=-5 THEN
# R01R_A_CUR_EXPR_CIGS=-99955 ELSE IF R01_AC1002=-1 OR R01_AC1005=-1 OR R01_AC1003=-1 THEN R01R_A_CUR_EXPR_CIGS=-99911
# 
# 


# R01R_A_FMR_EXPR_CIGS: DERIVED - Wave 1 Adult Former Experimental Cigarette User
# Long Description: Has ever smoked a cigarette, has not smoked more than 100 cigarettes in lifetime, and now does not smoke at all.
# Algorithm:
#   IF R01_AC1002=1 AND R01_AC1005 in (1, 2, 3, 4, 5) AND R01_AC1003 = 3 THEN R01R_A_FMR_EXPR_CIGS= 1; ELSE IF
# - 1095 -
#   R01_AC1002= 2 OR R01_AC1003 in (1,2) OR (R01_AC1003 in (3, -7,-8,-9) AND R01_AC1005 = 6) , THEN R01R_A_FMR_EXPR_CIGS=2;
# ELSE IF R01_AC1002=-9 OR R01_AC1005=-9 OR R01_AC1003=-9 THEN R01R_A_FMR_EXPR_CIGS=-99999 ELSE IF R01_AC1002=-8
# OR R01_AC1005=-8 OR R01_AC1003=-8 THEN R01R_A_FMR_EXPR_CIGS=-99988 ELSE IF R01_AC1002=-7 OR R01_AC1005=-7 OR
# R01_AC1003=-7 THEN R01R_A_FMR_EXPR_CIGS=-99977 ELSE IF R01_AC1002=-5 OR R01_AC1005=-5 OR R01_AC1003=-5 THEN
# R01R_A_FMR_EXPR_CIGS=-99955 ELSE IF R01_AC1002=-1 OR R01_AC1005=-1 OR R01_AC1003=-1 THEN R01R_A_FMR_EXPR_CIGS=-99911
# 
# # R01R_A_FMR_ESTD_CIGS: DERIVED - Wave 1 Adult Former Established Cigarette User
# Long Description: Has ever smoked a cigarette, has smoked more than 100 cigarettes in lifetime, and now does not smoke at all.
# Algorithm:
#   IF R01_AC1002=1 AND R01_AC1005=6 AND R01_AC1003=3 THEN R01R_A_FMR_ESTD_CIGS = 1;
#ELSE IF R01_AC1002= 2 OR R01_AC1003 In (1, 2) OR (R01_AC1003 in (3, -7,-8,-9) AND R01_AC1005 in (1,2, 3, 4, 5)) , 
                                                    # THEN R01R_A_FMR_ESTD_CIGS=2;
# ELSE IF R01_AC1002=-9 OR R01_AC1005=-9 OR R01_AC1003=-9 THEN R01R_A_FMR_ESTD_CIGS=-99999 ELSE IF R01_AC1002=-8
# OR R01_AC1005=-8 OR R01_AC1003=-8 THEN R01R_A_FMR_ESTD_CIGS=-99988 ELSE IF R01_AC1002=-7 OR R01_AC1005=-7 OR
# - 1093 -
#   R01_AC1003=-7 THEN R01R_A_FMR_ESTD_CIGS=-99977 ELSE IF R01_AC1002=-5 OR R01_AC1005=-5 OR R01_AC1003=-5 THEN
# R01R_A_FMR_ESTD_CIGS=-99955 ELSE IF R01_AC1002=-1 OR R01_AC1005=-1 OR R01_AC1003=-1 THEN R01R_A_FMR_ESTD_CIGS=-99911


# R01R_A_NVR_CIGS: DERIVED - Wave 1 Adult Never Cigarette User
# Long Description: Has never smoked a cigarette, even one or two puffs. Algorithm:
#   IF R01_AC1002=2 THEN R01R_A_NVR_CIGS = 1; ELSE IF R01_AC1002=1 THEN R01R_A_NVR_CIGS = 2; ELSE IF
#   R01_AC1002=-9 THEN R01R_A_NVR_CIGS=-99999 ELSE IF R01_AC1002=-8 THEN R01R_A_NVR_CIGS=-99988 ELSE IF R01_AC1002=-7
#   THEN R01R_A_NVR_CIGS=-99977 ELSE IF R01_AC1002=-5 THEN R01R_A_NVR_CIGS=-99955 ELSE IF R01_AC1002=-1 THEN
#   R01R_A_NVR_CIGS=-99911

#### NOTES ####

#PSYCH DISTRESSS
# Psychological Variable: R03_AX0161 (Sad) or R03_AX0163 (Anxious) in past month


#QUIT STATUS
#Quit: Yes=stopped smoking, No=stayed smoking,
#Quit Category: Yes=stopped smoking, No=stayed smoking, Else: Stayed Non Smokers

#SMOKING CATEGORIES
#Create Smoking Status Factor Variable and Binary Variables 
## est_smoker = established smoker (current & former); smoked 100 cigs in lifetime
#Smoking Status Full has all categories cur/fmr est/exp smoker and non-smoers
#Smoking Status collapsed smoking status full into current, former, never

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


#R03R_A_P30D_CIGS: DERIVED - Wave 3 Adult Past 30 Day Cigarette Smoker
#Wave 3 Adult respondents who have smoked a cigarette within the past 30 days.
#Algorithm: 
#IF R03R_A_EVR_CIGS=1 AND 
###(R03_AC1004=1 OR R03_AC0100RY in (1,2,3,4) OR R03_AC0100MC in (1,2,3,4) OR 
###(0<=R03_AC1009_NN<=30 AND R03_AC1009_UN=1) OR (R03_AC1009_NN in (1,0) AND
###R03_AC1009_UN=2) OR R03_AC1022>0) 
#THEN R03R_A_P30D_CIGS=1; 
#ELSE IF R03R_A_EVR_CIGS=2 OR R03_AC1004=2 OR (R03_AC1009_NN>30 AND R03_AC1009_UN=1) 
###OR (R03_AC1009_NN>1 AND R03_AC1009_UN=2) OR R03_AC1009_UN=3 OR R03_AC1022=0 
#THEN R03R_A_P30D_CIGS=2; 
#### MISSING
#ELSE IF R03R_A_EVR_CIGS=-99999 OR R03_AC1004=-9 OR R03_AC0100RY=-9 OR R03_AC0100MC=-9 
####OR R03_AC1009_NN=-9 OR R03_AC1022=-9
#THEN R03R_A_P30D_CIGS=-99999; 
#ELSE IF R03R_A_EVR_CIGS=-99988 OR R03_AC1004=-8 OR R03_AC0100RY=-8
###OR R03_AC0100MC=-8 OR R03_AC1009_NN=-8 OR R03_AC1022=-8 
#THEN R03R_A_P30D_CIGS=-99988; 
#ELSE IF R03R_A_EVR_CIGS=-99977 OR R03_AC1004=-7 OR R03_AC0100RY=-7 
####OR R03_AC0100MC=-7 OR R03_AC1009_NN=-7OR R03_AC1022=-7 
#THEN R03R_A_P30D_CIGS=-99977; 
#ELSE IF R03_AC1009_NN= -5 
#THEN R03R_A_P30D_CIGS = -99955; 
#ELSE IF R03R_A_EVR_CIGS=-99966 THEN R03R_A_P30D_CIGS=-99966; 
#ELSE IF R03R_A_EVR_CIGS=-99911 OR R03_AC1004=-1 OR R03_AC0100RY=-1 OR R03_AC0100MC=-1 
###OR R03_AC1009_NN=-1 OR R03_AC1022=-1
#THEN R03R_A_P30D_CIGS=-99911;



#### QUIT VARIABLES #####


#R01_AC1009_NN: How long since you completely quit smoking cigarettes - Number
#R01_AC1009_UN: How long since you completely quit smoking cigarettes - Unit



#R03R_A_DAYSQUIT_CIGS: 
#DERIVED - Wave 3 Adult Number of Days Since Last Smoked a Cigarette


# WAVE 3: DERIVED INCOME VARIABLES 

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


# //R02_AC1023_NN: In past 30 days, average number of cigarettes smoked per day on days smoked - Number
# //R02_AC1023_UN: In past 30 days, average number of cigarettes smoked per day on days smoked - Unit


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

#R02_AN0120: In past 12 months, stopped smoking/using [tobacco products / specific product]
#for one day or longer because you were trying to quit
#R02_AC1010: Completely quit smoking cigarettes
#R02_AC1134: Completely quit smoking cigarettes
#R02_AC1132: Reason no answer for completely quit smoking cigarettes
#R02_AN0105_01: 
#[Tobacco products / Specific product] quitting effort:
#Yes, I have tried to quit completely


# PSYCHOLOGICAL DISTRESS 

#R01_AX0161: Last time you had significant problems with: 
#Feeling very trapped, lonely, sad,blue, depressed or hopeless about the future

#R01_AX0163: Last time you had significant problems with: 
#Feeling very anxious, nervous, tense, scared, panicked or like something bad was going to happen



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
