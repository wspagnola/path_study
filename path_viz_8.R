#Need Marginal Frequencies to check this

#No Change = Former or Current Smoker to Former or CurrentSmoker 
#Cessation = Former or Current Smoker to Not Smoking in Past Year
relevel_factor <- function(x) factor(x, levels = c('Never', 'Current', 'Former'))

adult_panel$nev
adult_panel <- adult_panel %>% 
  mutate(
  smoking_status_cur_w1 = case_when(
      as.numeric(R01R_A_NVR_CIGS) == 1 ~ 'Never',
      as.numeric(R01R_A_CUR_ESTD_CIGS) == 1 |  as.numeric(R01R_A_CUR_EXPR_CIGS) == 1 ~ 'Current',
      as.numeric(R01R_A_FMR_ESTD_CIGS) == 1 |  as.numeric(R01R_A_FMR_EXPR_CIGS) == 1 ~ 'Former'),
  smoking_status_cur_w2 = case_when(
      as.numeric(R02R_A_EVR_CIGS) == 2 ~ 'Never',
      as.numeric(R02R_A_CUR_ESTD_CIGS) == 1 |  as.numeric(R02R_A_CUR_EXPR_CIGS) == 1 ~ 'Current',
      current_non_est_smoker_w2==1 & as.numeric(R02R_A_CUR_EXPR_CIGS) == 2 | 
              former_est_smoker_w2==1 ~ 'Former'),
  smoking_status_cur_w3 = case_when(
    as.numeric(R03R_A_EVR_CIGS) == 2 ~ 'Never',
    as.numeric(R03R_A_CUR_ESTD_CIGS) == 1 |  as.numeric(R03R_A_CUR_EXPR_CIGS)==1 ~ 'Current',
    as.numeric(R03R_A_FMR_ESTD_CIGS_REV)==1 | as.numeric(R03R_A_FMR_EXPR_CIGS_REV)==1 ~ 'Former')
)  


adult_panel <- adult_panel %>% 
    mutate_if(grepl('smoking_status_cur', names(adult_panel)), 
              relevel_factor)

table(adult_panel$smoking_status_cur_w1, adult_panel$smoking_status_cur_w2)


#Smoking Trajectory From Wave 1 to Wave 2  
adult_panel %>% 
    mutate(smoking_traj_w1_w2 = case_when(
        smoking_status_w1 == 'never_smoker' & smoked_past12M_w2 == 0 ~ 'No Change',
        smoking_status_w1 == 'current' & smoked_past12M_w2 == 1 ~ 'No Change',
        smoking_status_w1 == 'former' & smoked_past12M_w2 == 1 ~ 'Relapse',
        smoking_status_w1 == 'never_smoker' & smoked_past12M_w2 == 1 ~ 'Initiation',
        smoking_status_w1 =='current' & smoked_past12M_w2==0 ~ 'Cessation')
) %>%  group_by(smoking_traj_w1_w2) %>%  count
           

adult_panel %>% 
    mutate(smoke_status_w1 = case_when(
                cigarette_use_ever_w1 == 0 ~ 'never',
                cigarette_current_use_w1 == 1 ~ 'current',
                cigarette_current_use_w1 == 0 & cigarette_use_ever_w1 == 1 ~ 'former'),
           smoke_status_w2 = case_when(
             never_smoker_w2 == 1 ~ 'never',
             current_est_smoker_w2 == 1  | current_exp_smoker_w2 ~ 'current',
             former_est_smoker_w2 == 1 | former_exp_smoker_w2 == 1 ~ 'former')
      
) %>%  group_by(smoking_status_w1, smoking_status_w2) %>%  count %>%  print(n =30)
  
adult_panel$cigarette_current_use_w1 %>%  unique
  
    filter(current_exp_smoker_w1 == 0, current_exp_smoker_w2 == 0) %>% 
    select(smoking_status_w1, smoking_status_w2) %>% 
    table
unique(adult_panel$former_est_smoker_w1)
table()
table(adult_panel$current_exp_smoker_w2, adult_panel$former_est_smoker_w2)