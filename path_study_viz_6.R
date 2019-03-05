#Quit Rates W1-W3 Missing


#Quit Rate among 11,402 Current Established Smokers at Wave 1 *

est_smokers_w1 <-  adult_panel %>% 
                    filter(current_est_smoker_w1==1)
nrow(est_smokers_w1) #11,402 Cur. Est. Smokers in Wave 1

#Current Use/Quit at Wave 2
est_smokers_w1 %>%
  mutate(quit_w2 = case_when(
            cigarette_current_use_w2 == 'No' | R02_AC1002_12M == '(2) 2 = No' ~ 'Yes',
            cigarette_current_use_w2 == 'Yes' | R02_AC1002_12M == '(1) 1 = Yes' ~ 'No')) %>% 
  group_by(quit_w2) %>% 
  count()
  
#Quit Rate of W1 Cur. Est. Smokers at Wave 2
est_smokers_w1 %>% 
  mutate(days_quit_cigs_w2 = case_when(
                            as.numeric(R02_AC1009_UN)==1 & R02_AC1009_NN>=0 ~ R02_AC1009_NN,
                            as.numeric(R02_AC1009_UN)==2 & R02_AC1009_NN>=0 ~ R02_AC1009_NN * 30.4375,
                            as.numeric(R02_AC1009_UN)==3 & R02_AC1009_NN>=0 ~  R02_AC1009_NN * 365.25), 
         days_quit_cigs_cat_w2 = case_when(
                            days_quit_cigs_w2 == 1 ~ 'One Day',
                            days_quit_cigs_w2 >= 2 &  days_quit_cigs_w2 <= 6 ~ 'Two to Six Days',
                            days_quit_cigs_w2 >= 7 &  days_quit_cigs_w2 < 30 ~ 'More than 7 Days',
                            days_quit_cigs_w2 >= 30 &  days_quit_cigs_w2 <= 90 ~ 'One Month',
                            days_quit_cigs_w2 > 90 &  days_quit_cigs_w2 <= 180 ~ 'Three Months',
                            days_quit_cigs_w2 > 180 &  days_quit_cigs_w2 < 365 ~ 'Six Months',
                            days_quit_cigs_w2 >= 365   ~ 'One Year',
                            is.na(R02_AC1009_UN)==T & R02_AC1002_12M == '(2) 2 = No' ~ 'One Year'),
          days_quit_cigs_cat_w2 = factor(days_quit_cigs_cat_w2,
                                         levels = c('One Day', 'Two to Six Days',  'More than 7 Days',
                                         'One Month',  'Three Months', 'Six Months',  'One Year',
                                         "Didn't Answer"))) %>% 
          group_by(days_quit_cigs_cat_w2) %>% 
          count()


#Quit Rate of W1 Cur. Est. Smokers at Wave 3
est_smokers_w1 %>% 
  mutate(days_quit_cigs_w3 = case_when(
    as.numeric(R03_AC1009_UN)==1 & R03_AC1009_NN>=0 ~ R03_AC1009_NN,
    as.numeric(R03_AC1009_UN)==2 & R03_AC1009_NN>=0 ~ R03_AC1009_NN * 30.4375,
    as.numeric(R03_AC1009_UN)==3 & R03_AC1009_NN>=0 ~  R03_AC1009_NN * 365.25), 
    days_quit_cigs_cat_w3 = case_when(
      days_quit_cigs_w3 == 1 ~ 'One Day',
      days_quit_cigs_w3 >= 2 &  days_quit_cigs_w3 <= 6 ~ 'Two to Six Days',
      days_quit_cigs_w3 >= 7 &  days_quit_cigs_w3 < 30 ~ 'More than 7 Days',
      days_quit_cigs_w3 >= 30 &  days_quit_cigs_w3 <= 90 ~ 'One Month',
      days_quit_cigs_w3 > 90 &  days_quit_cigs_w3 <= 180 ~ 'Three Months',
      days_quit_cigs_w3 > 180 &  days_quit_cigs_w3 < 365 ~ 'Six Months',
      days_quit_cigs_w3 >= 365   ~ 'One Year' ,
      is.na(R02_AC1009_UN)==T & R02_AC1002_12M == '(2) 2 = No' ~ 'One Year'),
    days_quit_cigs_cat_w3 = factor(days_quit_cigs_cat_w3,
                                   levels = c('One Day', 'Two to Six Days',  'More than 7 Days',
                                              'One Month',  'Three Months', 'Six Months',  'One Year',
                                              "Didn't Answer"))) %>% 
  group_by(days_quit_cigs_cat_w3) %>% 
  count()
#NOTE: Year is Different 

#Current Use/Quit of W1 Cur. Est. Smokers at Wave 3
est_smokers_w1 %>%
  mutate(quit_w3 = case_when(
    cigarette_current_use_w3 == 'No' | R03_AC1002_12M == '(2) 2 = No' ~ 'Yes',
    cigarette_current_use_w3 == 'Yes' | R03_AC1002_12M == '(1) 1 = Yes' ~ 'No')) %>% 
  group_by(quit_w3) %>% 
  count()
#NOTE: YES is fine; NO is off by 2


#### Wave 2 Cur. Est. Smokers ####
cur_est_smokers_w2 <- adult_panel %>% 
                            filter(current_est_smoker_w2==1)
nrow(cur_est_smokers_w2) #9694 Current Established Smokers at Wave 2 

#Quit Rate of W2 Cur. Est. Smokers at Wave 3
cur_est_smokers_w2 %>% 
  mutate(days_quit_cigs_w3 = case_when(
                as.numeric(R03_AC1009_UN)==1 & R03_AC1009_NN>=0 ~ R03_AC1009_NN,
                as.numeric(R03_AC1009_UN)==2 & R03_AC1009_NN>=0 ~ R03_AC1009_NN * 30.4375,
                as.numeric(R03_AC1009_UN)==3 & R03_AC1009_NN>=0 ~  R03_AC1009_NN * 365.25), 
    days_quit_cigs_cat_w3 = case_when(
                days_quit_cigs_w3 == 1 ~ 'One Day',
                days_quit_cigs_w3 >= 2 &  days_quit_cigs_w3 <= 6 ~ 'Two to Six Days',
                days_quit_cigs_w3 >= 7 &  days_quit_cigs_w3 < 30 ~ 'More than 7 Days',
                days_quit_cigs_w3 >= 30 &  days_quit_cigs_w3 <= 90 ~ 'One Month',
                days_quit_cigs_w3 > 90 &  days_quit_cigs_w3 <= 180 ~ 'Three Months',
                days_quit_cigs_w3 > 180 &  days_quit_cigs_w3 < 365 ~ 'Six Months',
                days_quit_cigs_w3 >= 365   ~ 'One Year',
                is.na(R03_AC1009_UN)==T & R03_AC1002_12M == '(2) 2 = No' ~ 'One Year'),
    days_quit_cigs_cat_w3 = factor(days_quit_cigs_cat_w3,
                                   levels = c('One Day', 'Two to Six Days',  'More than 7 Days',
                                              'One Month',  'Three Months', 'Six Months',  'One Year',
                                              "Didn't Answer"))) %>% 
  group_by(days_quit_cigs_cat_w3) %>% 
  count()
#NOTE: One Year Doesn't Match





#Current Use/Quit of W2 Cur. Est. Smokers at Wave 3
cur_est_smokers_w2 %>%
  mutate(quit_w3 = case_when(
    cigarette_current_use_w3 == 'No' | R03_AC1002_12M == '(2) 2 = No' ~ 'Yes',
    cigarette_current_use_w3 == 'Yes' | R03_AC1002_12M == '(1) 1 = Yes' ~ 'No')) %>% 
  group_by(quit_w3) %>% 
  count()
#YES is correct, but NO is off by 2



#### NOTES ####

#NOTE: Some Smokers (84 total) stated that they did not have a single puff in a year but 
#Said the last time they smoked a cigarette was 90 days ago or less 
#25 Smokers
est_smokers_w1 %>% 
    filter(R02_AC1002_12M == '(2) 2 = No',
           R02_AC1009_UN == '(1) 1 = Days') %>% 
    pull(R02_AC1009_NN) %>% 
    length

#59 Smokers 
est_smokers_w1 %>% 
  filter(R02_AC1002_12M == '(2) 2 = No',
         R02_AC1009_UN == '(2) 2 = Months',
         R02_AC1009_NN < 12) %>% 
  pull(R02_AC1009_NN) %>% 
  length

      
      
      
