

#No Change = Former or Current Smoker to Former or CurrentSmoker 
#Cessation = Former or Current Smoker to Not Smoking in Past Year

#Smoking Trajectory From Wave 1 to Wave 2  
adult_panel %>% 
    mutate(smoking_traj_w1_w2 = case_when(
        smoking_status_w1 == 'never_smoker' & smoked_past12M_w2 == 0 ~ 'No Change',
        smoking_status_w1 == 'current_est_smoker' & smoked_past12M_w2 == 1 ~ 'No Change',
        smoking_status_w1 == 'former_est_smoker' & smoked_past12M_w2 == 1 ~ 'Relapse',
        smoking_status_w1 == 'never_smoker' & smoked_past12M_w2 == 1 ~ 'Initiation',
        smoking_status_w1 =='current_est_smoker' & smoked_past12M_w2==0 ~ 'Cessation')
) %>%  group_by(smoking_traj_w1_w2) %>%  count
           
           
adult_panel$smo