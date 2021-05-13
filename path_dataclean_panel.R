adult_panel <- adult_w1 %>%
  full_join(y = adult_w2, by = c('PERSONID')) %>%
  full_join(y= adult_w3, by = c('PERSONID')
  )



remove(list = c('adult_w1', 'adult_w2', 'adult_w3'))



##### W1 -> W2: Transition Status Quit Categories ####

#Note: quit_w1_w2 doesn't have category for Wave 1 non-smoker/former-smokers?

# Note: smoker must have quit for year in order to be considered to have quit
adult_panel <- adult_panel %>%
                    mutate(quit_w1_w2  = case_when(
                                                         cig_use_now_w1 == 1 & cig_use_now_w2 == 1  ~ 'No',
                                cig_use_now_w1 == 1 & (smoked_past12M_w2 == 0 | cig_use_now_w2 == 0) ~ 'Yes'),
                           
                           quit_cat_w1_w2  = case_when(
                                                          cig_use_now_w1 == 1 & cig_use_now_w2 == 1  ~ 'No',
                                cig_use_now_w1 == 1 & (smoked_past12M_w2 == 2 | cig_use_now_w2 == 0) ~ 'Yes',
                                cig_use_now_w1 == 0 & (smoked_past12M_w2 == 2 | cig_use_now_w2 == 0) ~ 'Stayed Non-Smoker')
  )



adult_panel %>%  count(cig_use_now_w1, quit_w1_w2)
adult_panel %>%  count(cig_use_now_w1, quit_cat_w1_w2)




# W1 -> W3: Quit Status  at W3 for 'W1 Current Smokers'
adult_panel <- adult_panel %>% 
                  mutate(
                    quit_w1_w3  = case_when(
                                                  cig_use_now_w1 == 1 & cig_use_now_w3 == 1 ~  'No',
                        cig_use_now_w1 == 1 & (smoked_past12M_w3 == 0 | cig_use_now_w3 == 0) ~ 'Yes'),
                    
                    quit_cat_w1_w3  = case_when(
                                                cig_use_now_w1 == 1 & cig_use_now_w3 == 1  ~ 'No',
                      cig_use_now_w1 == 1 & (smoked_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Yes',
                      cig_use_now_w1 == 0 & (smoked_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Stayed Non-Smoker')
  ) 

##### W2 -> W3:  Quit Status at W3 for 'W2 Current Smokers'  ####
adult_panel <- adult_panel %>% 
                    mutate(
                        quit_w2_w3  = case_when(
                                                  cig_use_now_w2 == 1 & cig_use_now_w3 == 1  ~  'No',
                        cig_use_now_w2 == 1 & (smoked_past12M_w3 == 0 | cig_use_now_w3 == 0) ~ 'Yes'),
                        
                      quit_cat_w2_w3  = case_when(
                                                  cig_use_now_w2 == 1 & cig_use_now_w3 == 1  ~ 'No',
                        cig_use_now_w2 == 1 & (smoked_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Yes',
                        cig_use_now_w2 == 0 & (smoked_past12M_w3 == 1 | cig_use_now_w3 == 0) ~ 'Stayed Non-Smoker'),
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
#     cig_use_now_w2==1 & (smoked_past12M_w3==0 | cig_use_now_w3== 0) ~ 'Yes')
#   )




#### Recode NAs as Zero f####
#For obs. that was not included in a given wave
adult_panel <- adult_panel %>% 
  mutate(wave_1 = ifelse(is.na(wave_1), 0, wave_1),
         wave_2 = ifelse(is.na(wave_2), 0, wave_2),
         wave_3 = ifelse(is.na(wave_3), 0, wave_3)
  ) 


# Note: recode -8, -7, -1 as NA?
adult_panel %>%  group_by(smoked_past12M_w2) %>%  count
adult_panel %>%  group_by(cig_use_now_w2)    %>%  count
adult_panel %>%  group_by(cig_use_now_w1)    %>%  count


#### Write to Output File ####

# Note: not sure about CASEID, VARPSU, and VARSTAT

cols <- names(adult_panel) %>%  str_subset("[a-z]")
cols <- cols[!str_detect(cols, '\\.x|\\.y') ]
adult_panel <- adult_panel %>%  select(PERSONID, all_of(cols), R02_CONTINUING_ADULT_LD,R03_ADULTTYPE)

object.size(adult_panel) # 25.6 mb

write.csv(adult_panel, 'data/Output/adult_panel.csv')
# note make sure git attributes includes csv files in lfs git system 
# git lfs track "*.csv"


#### Check Representation across waves ####
adult_panel %>%  glimpse
adult_panel %>%  group_by(wave_1, wave_2, wave_3, R02_CONTINUING_ADULT_LD, R03_ADULTTYPE) %>%  count
# Note: Some inconsistencies (n= 3)
# 1 adult in wave 1 and wave 2 but marked as NA for wave 2
# 2 adults in all three waves but marked as NA for waves 2, 3 