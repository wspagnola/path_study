#Page 12 

#Smoking Trajectories Wave 1-3 among ALL Smokers

adult_panel$smo
adult_w1$smoking



#replace smkstatus_w1=2 if r01r_a_cur_estd_cigs==2 & r01r_a_nvr_cigs==2
adult_w1 %>% 
  filter(as.numeric(R01R_A_NVR_CIGS) ==2, 
         as.numeric(R01R_A_CUR_ESTD_CIGS)==2,
         as.numeric(R01R_A_FMR_ESTD_CIGS) != 1)  %>%  nrow


#Create Smoking Types Table for Wave 1
check_current_non_est_w1 <- adult_w1 %>% 
  group_by(R01R_A_CUR_ESTD_CIGS, R01R_A_FMR_ESTD_CIGS,R01R_A_NVR_CIGS, R01R_A_CUR_EXPR_CIGS,
           R01R_A_FMR_EXPR_CIGS, cigarette_current_use_w1, est_smoker_w1)  %>% 
  count
#write.csv(check_current_non_est_w1, 'check_smoking_types_w1.csv')



#Create Smoking Types Table for Wave 2
check_current_non_est_w2 <- adult_w2 %>% 
  group_by(R02R_A_CUR_ESTD_CIGS, R02R_A_FMR_ESTD_CIGS_REV, R02R_A_EVR_CIGS, R02R_A_CUR_EXPR_CIGS,
           R02R_A_FMR_EXPR_CIGS_REV, cigarette_current_use_w2, est_smoker_w2)  %>% 
  count
#write.csv(check_current_non_est_w2, 'check_smoking_types_w2.csv')


#Create Smoking Types Table for Wave 3
check_smoking_types_w3<- adult_w3 %>% 
  group_by(R03R_A_CUR_ESTD_CIGS, R03R_A_FMR_ESTD_CIGS_REV, R03R_A_EVR_CIGS, R03R_A_CUR_EXPR_CIGS,
           R03R_A_FMR_EXPR_CIGS_REV, cigarette_current_use_w3, est_smoker_w3)  %>% 
  count
#write.csv(check_smoking_types_w3, 'check_smoking_types_w3.csv')



#### Create Tables to Understand 30-Day Data Patterns ####

quit_past_30_days_table_w1_w2 <- adult_panel %>%  
  filter(current_est_smoker_w1 == 1) %>% 
  group_by(cigarette_current_use_w2, smoked_past30D_w2, smoked_past12M_w2) %>% 
  count
#write.csv(file = 'quit_past_30_days_table_w1_w2.csv', x =quit_past_30_days_table_w1_w2)

quit_past_30_days_table_w1_w3 <- adult_panel %>%  
  filter(current_est_smoker_w1 == 1) %>% 
  group_by(cigarette_current_use_w3, smoked_past30D_w3, smoked_past12M_w3) %>% 
  count
#write.csv(file = 'quit_past_30_days_table_w1_w3.csv', x =quit_past_30_days_table_w1_w3)


quit_past_30_days_table_w2_w3 <- adult_panel %>%  
  filter(current_est_smoker_w2 == 1) %>% 
  group_by(cigarette_current_use_w3, smoked_past30D_w3, smoked_past12M_w3) %>% 
  count
#write.csv(file = 'quit_past_30_days_table_w2_w3.csv', x =quit_past_30_days_table_w2_w3)