
# Merge Data
adult_panel <- adult_w1 %>%
  full_join(y = adult_w2, by = c('PERSONID')) %>%
  full_join(y= adult_w3, by = c('PERSONID')
)

adult_panel %>%  filter(wave_2 == 1, wave_3==1, wave_1 ==1) %>%  count()




##### W1 -> W2: Transition Status Quit Categories ####

#Note: quit_w1_w2 doesn't have category for Wave 1 non-smoker/former-smokers?

# Note: smoker must have quit for year in order to be considered to have quit
adult_panel <- adult_panel %>%
                    mutate(quit_w1_w2  = case_when(
                                                         cig_use_now_w1 == 1 & cig_use_now_w2 == 1  ~ 'No',
                                cig_use_now_w1 == 1 & (cig_use_past12M_w2 == 0 | cig_use_now_w2 == 0) ~ 'Yes'),
                           
                           quit_cat_w1_w2  = case_when(
                                                          cig_use_now_w1 == 1 & cig_use_now_w2 == 1  ~ 'No',
                                cig_use_now_w1 == 1 & (cig_use_past12M_w2 == 2 | cig_use_now_w2 == 0) ~ 'Yes',
                                cig_use_now_w1 == 0 & (cig_use_past12M_w2 == 2 | cig_use_now_w2 == 0) ~ 'Stayed Non-Smoker')
  )




# W1 -> W3: Quit Status  at W3 for 'W1 Current Smokers'
adult_panel <- adult_panel %>% 
                  mutate(
                    quit_w1_w3  = case_when(
                                                  cig_use_now_w1 == 1 & cig_use_now_w3 == 1 ~  'No',
                        cig_use_now_w1 == 1 & (cig_use_past12M_w3 == 0 | cig_use_now_w3 == 0) ~ 'Yes'),
                    
                    quit_cat_w1_w3  = case_when(
                                                cig_use_now_w1 == 1 & cig_use_now_w3 == 1  ~ 'No',
                      cig_use_now_w1 == 1 & (cig_use_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Yes',
                      cig_use_now_w1 == 0 & (cig_use_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Stayed Non-Smoker')
  ) 

##### W2 -> W3:  Quit Status at W3 for 'W2 Current Smokers'  ####
adult_panel <- adult_panel %>% 
                    mutate(
                        quit_w2_w3  = case_when(
                                                  cig_use_now_w2 == 1 & cig_use_now_w3 == 1  ~  'No',
                        cig_use_now_w2 == 1 & (cig_use_past12M_w3 == 0 | cig_use_now_w3 == 0) ~ 'Yes'),
                        
                      quit_cat_w2_w3  = case_when(
                                                  cig_use_now_w2 == 1 & cig_use_now_w3 == 1  ~ 'No',
                        cig_use_now_w2 == 1 & (cig_use_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Yes',
                        cig_use_now_w2 == 0 & (cig_use_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Stayed Non-Smoker'),
  ) 



#### Convert to Factors ####

adult_panel <- adult_panel %>% 
                  mutate(
                    quit_cat_w1_w2  = factor(quit_cat_w1_w3, levels = c('Yes', 'No', 'Stayed Non-Smoker')),
                    quit_cat_w1_w3  = factor(quit_cat_w1_w3, levels = c('Yes', 'No', 'Stayed Non-Smoker')),
                    quit_cat_w2_w3  = factor(quit_cat_w2_w3, levels = c('Yes', 'No', 'Stayed Non-Smoker'))
                    
)


#### What is this? ####

# WAVE 3: P30D Quit Status at W3 for 'W2 Current Smokers' 
# adult_panel <- adult_panel %>% 
#   mutate( quit_p30d_w2_w3  = case_when(
#     cig_use_now_w2==1 & cig_use_now_w3==1 ~  'No',
#     cig_use_now_w2==1 & (cig_use_past12M_w3==0 | cig_use_now_w3== 0) ~ 'Yes')
#   )




#### Recode NAs as Zero f####
#For obs. that was not included in a given wave
adult_panel <- adult_panel %>% 
  mutate(wave_1 = ifelse(is.na(wave_1), 0, wave_1),
         wave_2 = ifelse(is.na(wave_2), 0, wave_2),
         wave_3 = ifelse(is.na(wave_3), 0, wave_3)
  ) 


# Note: recode -8, -7, -1 as NA?
adult_panel %>%  group_by(cig_use_past12M_w2) %>%  count
adult_panel %>%  group_by(cig_use_now_w2)    %>%  count
adult_panel %>%  group_by(cig_use_now_w1)    %>%  count



# Note: Some inconsistencies (n= 3)
# 1 adult in wave 1 and wave 2 but marked as NA for wave 2
# 2 adults in all three waves but marked as NA for waves 2, 3 



# Note: quit_nonresp_reason_w2 == 1 are recanters
# Note: Maybe make current/former category and establish category then use these categories to create 4 sub categories 
adult_panel <- adult_panel %>%  
                  mutate( est_smoker_w2 = case_when(cig_num_life_w2== 6 | est_smoker_w1 == 1 ~1,
                                                    cig_num_life_w2 >= 1 & cig_num_life_w2 <= 5 ~ 0),
                               smoking_status_full_w2 = case_when(cig_current_freq_w2 %in% c(1,2) & est_smoker_w2 == 1 ~ 'current_est_smoker',
                    (cig_use_now_w2 == 0  | cig_use_past12M_w2 ==2) & est_smoker_w2 == 1 & quit_nonresp_reason_w2  != 1 ~ 'former_est_smoker',
                                                                              cig_use_now_w2 == 1 & est_smoker_w2 == 0 ~ 'current_exp_smoker',
                    (cig_use_now_w2 == 0  | cig_use_past12M_w2 ==2)  & est_smoker_w2 == 0 & quit_nonresp_reason_w2 != 1 ~ 'former_exp_smoker',
                                                                                                  cig_use_ever_w2 == 0 ~'never_smoker'),
                          smoking_status_full_w2 = as.factor(smoking_status_full_w2),
                  smoking_status_w2 = fct_collapse(smoking_status_full_w2,
                                                   'current' = c('current_est_smoker', 'current_exp_smoker'),
                                                   'former' = c('former_est_smoker', 'former_exp_smoker')),
                  current_est_smoker_w2 = if_else(smoking_status_full_w2 == 'current_est_smoker', 1, 0),
                  former_est_smoker_w2 = if_else(smoking_status_full_w2 == 'former_est_smoker', 1, 0),
                  current_exp_smoker_w2 = if_else(smoking_status_full_w2 == 'current_exp_smoker', 1, 0),
                  former_exp_smoker_w2 = if_else(smoking_status_full_w2 == 'former_exp_smoker', 1, 0),
                  never_smoker_w2 = if_else(smoking_status_full_w2 == 'never_smoker', 1, 0)
  )


#### New Variable Codes #### 
# R02_AC1003: cig_current_freq_w2 
# R02_AC1005: cig_num_life_w2
# R02_AC1002_12M: cig_use_past12M_w2  
# R01_YC1005: from youth survey
# R02R_A_EVR_CIGS: cig_use_ever_w2
# R02_AC1132: quit_nonresp_reason_w2


#### My Derived Vars ####
adult_panel %>% 
  filter(wave_1 == 1, wave_2 == 1) %>% 
  count(smoking_status_full_w2,  adult_cont_w2  )

#### Paths Vars ####

adult_panel %>% 
  filter(  adult_cont_w2  == 1) %>% 
  count(R02R_A_CUR_ESTD_CIGS, R02R_A_CUR_EXPR_CIGS, R02R_A_FMR_ESTD_CIGS_REV,  R02R_A_FMR_EXPR_CIGS_REV )




# Note: Maybe make current/former category and establish category then use these categories to create 4 sub categories 
adult_panel <- adult_panel %>%  
  mutate( est_smoker_w3 = case_when(cig_num_life_w3== 6 | est_smoker_w2 == 1 ~1,
                                    cig_num_life_w3 >= 1 & cig_num_life_w3 <= 5 ~ 0),
          smoking_status_full_w3 = case_when(cig_current_freq_w3 %in% c(1,2) & est_smoker_w3 == 1 ~ 'current_est_smoker',
        (cig_use_now_w3 == 0  | cig_use_past12M_w3 ==2) & est_smoker_w3 == 1 & quit_nonresp_reason_w3  != 3 ~ 'former_est_smoker',
                                             cig_use_now_w3 == 1 & est_smoker_w3 == 0 ~ 'current_exp_smoker',
        (cig_use_now_w3 == 0  | cig_use_past12M_w3 ==2)  & est_smoker_w3 == 0 & quit_nonresp_reason_w3 != 3 ~ 'former_exp_smoker',
                                             cig_use_ever_w3 == 0 ~'never_smoker'),
          smoking_status_full_w3 = as.factor(smoking_status_full_w3),
          smoking_status_w3 = fct_collapse(smoking_status_full_w3,
                                           'current' = c('current_est_smoker', 'current_exp_smoker'),
                                           'former' = c('former_est_smoker', 'former_exp_smoker')),
          current_est_smoker_w3 = if_else(smoking_status_full_w3 == 'current_est_smoker', 1, 0),
          former_est_smoker_w3 = if_else(smoking_status_full_w3 == 'former_est_smoker', 1, 0),
          current_exp_smoker_w3 = if_else(smoking_status_full_w3 == 'current_exp_smoker', 1, 0),
          former_exp_smoker_w3 = if_else(smoking_status_full_w3 == 'former_exp_smoker', 1, 0),
          never_smoker_w3 = if_else(smoking_status_full_w3 == 'never_smoker', 1, 0)
  )


#### My Derived Vars ####
adult_panel %>% 
  filter(wave_1 == 1, wave_2 == 1, wave_3== 1) %>% 
  count(smoking_status_full_w3)

#### Paths Vars ####

adult_panel %>% 
  filter(  adult_cont_w2  == 1,   adult_cont_w3== 1) %>% 
  count(R03R_A_CUR_ESTD_CIGS, R03R_A_CUR_EXPR_CIGS, R03R_A_FMR_ESTD_CIGS_REV,  R03R_A_FMR_EXPR_CIGS_REV )



#### Write to Output File ####

# Note: not sure about CASEID, VARPSU, and VARSTAT

cols <- names(adult_panel) %>%  str_subset("[a-z]")
cols <- cols[!str_detect(cols, '\\.x|\\.y') ]
cols

derived_vars <- c("R01R_A_CUR_ESTD_CIGS", 
                  "R01R_A_CUR_EXPR_CIGS", 
                  "R01R_A_FMR_EXPR_CIGS" , 
                  "R01R_A_FMR_EXPR_CIGS",
                  "R02R_A_CUR_ESTD_CIGS", 
                  "R02R_A_CUR_EXPR_CIGS",  
                  "R02R_A_FMR_ESTD_CIGS_REV",
                  "R02R_A_FMR_EXPR_CIGS_REV",
                  "R03R_A_CUR_ESTD_CIGS" , 
                  "R03R_A_CUR_EXPR_CIGS", 
                  "R03R_A_FMR_ESTD_CIGS_REV", 
                  "R03R_A_FMR_EXPR_CIGS_REV")     

# R01R_A RO2R_A , RO3R_A : Derived Variables
adult_panel_final <- adult_panel %>%  select(PERSONID, all_of(cols),  all_of(derived_vars))
                                    

object.size(adult_panel_final) # 25.6 mb

write.csv(adult_panel_final, 'data/Output/adult_panel.csv', row.names =FALSE)
# note make sure git attributes includes csv files in lfs git system 
# git lfs track "*.csv"
# git lfs ls-files -> see files being tracked

#### Check Representation across waves ####
adult_panel %>%  glimpse
adult_panel %>%  group_by(wave_1, wave_2, wave_3,   adult_cont_w2 ,   adult_cont_w3) %>%  count


# Derived Variable 
adult_panel %>%  dplyr::select(starts_with('R02R_A'))


## Matching tabel 



# Five Cateories 

adult_panel$smoking_status_full_w1 %>%  table

# wave 1
adult_panel %>%  count(R01R_A_NVR_CIGS) 
adult_panel %>%  count(R01R_A_CUR_ESTD_CIGS)
adult_panel %>% count(  R01R_A_FMR_ESTD_CIGS)  
adult_panel %>%  count(R01R_A_FMR_EXPR_CIGS)  # different number
adult_panel %>%  count(R01R_A_CUR_EXPR_CIGS) # different number


# wave 2 
adult_panel$smoking_status_full_w2 %>%  table

adult_panel %>%  count(R02R_A_EVR_CIGS) # Difference  var name
adult_panel %>% count(  R02R_A_FMR_ESTD_CIGS_REV ) # Difference var name
adult_panel %>%  count(R02R_A_CUR_ESTD_CIGS)
adult_panel %>%  count(R02R_A_FMR_EXPR_CIGS_REV) # Different var name 
adult_panel %>%  count(R02R_A_CUR_EXPR_CIGS)


#wave 3
adult_panel$smoking_status_full_w3 %>%  table
adult_panel%>%   filter(  adult_cont_w3 ==1 ) %>%  count(smoking_status_full_w3)

#adult_panel %>%  select(starts_with('R03R_A_EVR')) %>%  View # Cann't finn ever cigarette
adult_panel %>% count(  R03R_A_FMR_ESTD_CIGS_REV ) 
adult_panel %>%  count(R03R_A_CUR_ESTD_CIGS)
adult_panel %>%  count(R03R_A_FMR_EXPR_CIGS_REV)
adult_panel %>%  count(R03R_A_CUR_EXPR_CIGS)


#### checkes ####

# adult_panel %>%  
#   count(est_smoker_w2, cig_num_life_w1, cig_num_life_w2)  %>% 
#   filter(est_smoker_w2 == 1) %>%  View

adult_panel %>%  
  count(R02R_A_CUR_ESTD_CIGS)


## Wave1, Wave2 adult numbers match
## Does not include est. smokers from youth wave 1
## Does include wave 1 youths who became established smokers between waves 1 & 2 
adult_panel %>%  
  filter(R02R_A_CUR_ESTD_CIGS== 1) %>% 
  count(  adult_cont_w2 ) 

adult_panel %>% 
  filter(wave_1 == 1, wave_2 == 1) %>% 
  filter(smoking_status_full_w2 == 'current_est_smoker') %>%  count
# 
# adult_panel %>% 
#   filter(  adult_cont_w2  == 1, cig_num_life_w2 >= 0, cig_use_now_w2 >= 0)  %>% 
#   count(R02R_A_CUR_EXPR_CIGS, cig_num_life_w2,cig_use_now_w2, est_smoker_w2, smoking_status_full_w2 )  %>% 
#   View

adult_panel %>% 
  filter(  adult_cont_w2  == 1,  smoking_status_full_w2 == 'current_exp_smoker')  %>% 
  count(cig_num_life_w2,cig_use_now_w2, est_smoker_w2, smoking_status_full_w2)

adult_panel %>% 
  filter(  adult_cont_w2  == 1, smoking_status_full_w2 == 'former_est_smoker') %>% 
  count( R02R_A_FMR_ESTD_CIGS_REV, smoking_status_full_w2,  est_smoker_w2, cig_use_now_w2, cig_use_past12M_w2 )

# adult_panel %>%  
#   count(cig_current_freq_w2, est_smoker_w2) %>% 
#   filter_all(all_vars(. >= 0)) %>%  
#   View

adult_panel %>%  count(cig_use_ever_w2)
adult_panel %>%  count(R01R_A_CUR_ESTD_CIGS) 


adult_panel %>%  count(cig_use_now_w1, quit_w1_w2)
adult_panel %>%  count(cig_use_now_w1, quit_cat_w1_w2)


# Think I missing youths who reached 100 lifetime cigarette during youth survey
# Also think there are some new adults in wave 3

# adult_panel %>% 
#   count(R03R_A_THRSHLD_CIGS, adult_cont_w3, adult_cont_w2 ,est_smoker_w3, est_smoker_w2, est_smoker_w1) %>% 
#   View

#remove(list = c('adult_w1', 'adult_w2', 'adult_w3'))
#remove(list = c('adult_w1', 'adult_w2', 'adult_w3', 'adult_panel'))
#