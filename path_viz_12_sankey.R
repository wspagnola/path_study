
source('source.R')

adult_panel <- read.csv('data/Output/adult_panel.csv')

#### Sankey Plot: All 5 levels (Complete Cases)  ####



step_1 <- adult_panel  %>% 
              filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
                     as.numeric(R03_ADULTTYPE)==1) %>% 
              drop_na(smoking_status_full_w1,
                      smoking_status_full_w2,
                      smoking_status_full_w3) %>% 
              dplyr::select(smoking_status_full_w1, smoking_status_full_w2)  %>%
              count(smoking_status_full_w1, smoking_status_full_w2) %>%  
              rename(before = smoking_status_full_w1,
                     after =  smoking_status_full_w2,
                     flow = n) %>%
              mutate(step_from = 'wave 1',
                     step_to = 'wave 2',
                     before =  paste('w1', before, sep = '_'),
                     after = paste('w2', after, sep = '_'))  

step_2 <- adult_panel  %>% 
            filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
                   as.numeric(R03_ADULTTYPE)==1) %>% 
            drop_na(smoking_status_full_w1,
                    smoking_status_full_w2,
                    smoking_status_full_w3) %>% 
            dplyr::select(smoking_status_full_w2, smoking_status_full_w3)  %>%
            count(smoking_status_full_w2, smoking_status_full_w3) %>% 
            rename(before = smoking_status_full_w2,
                   after =  smoking_status_full_w3,
                   flow = n) %>% 
            mutate(step_from = 'wave 2',
                   step_to = 'wave 3',
                   before = paste('w2', before, sep = '_'),
                   after = paste('w3', after, sep = '_'))
sankey_df <- rbind(step_1, step_2)
write.csv(sankey_df, 'Output/sankey_df.csv', row.names = FALSE)




####  Sankey Plot: All 5 levels (Include Subject and Item Non-Response)  ####

step_1 <- adult_panel  %>% 
  dplyr::select(smoking_status_full_w1, smoking_status_full_w2)  %>%
  count(smoking_status_full_w1, smoking_status_full_w2) %>%  
  rename(before = smoking_status_full_w1,
         after =  smoking_status_full_w2,
         flow = n) %>%
  mutate(step_from = 'wave 1',
         step_to = 'wave 2',
         before =  paste('w1', before, sep = '_'),
         after = paste('w2', after, sep = '_'))  

step_2 <- adult_panel  %>%
  dplyr::select(smoking_status_full_w2, smoking_status_full_w3)  %>%
  count(smoking_status_full_w2, smoking_status_full_w3) %>% 
  rename(before = smoking_status_full_w2,
         after =  smoking_status_full_w3,
         flow = n) %>% 
  mutate(step_from = 'wave 2',
         step_to = 'wave 3',
         before = paste('w2', before, sep = '_'),
         after = paste('w3', after, sep = '_'))
sankey_df <- rbind(step_1, step_2)
sankey_df <- sankey_df %>% 
                mutate(before = str_replace_all(before, 'NA', 'missing'),
                       after = str_replace_all(after, 'NA', 'missing')) 
write.csv(sankey_df, 'Output/sankey_non_response_df.csv', row.names = FALSE)





#### W1 Cur. Est. Smokers  (Complete Cases) ####

step_1 <- adult_panel  %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
         as.numeric(R03_ADULTTYPE)==1,
         current_est_smoker_w1 ==1 ) %>% 
  dplyr::select(smoking_status_full_w1, smoking_status_full_w2)  %>%
  drop_na(smoking_status_full_w2) %>% 
  count(smoking_status_full_w1, smoking_status_full_w2) %>% 
  rename(before = smoking_status_full_w1,
         after =  smoking_status_full_w2,
         flow = n) %>% 
  mutate(step_from = 'wave 1',
         step_to = 'wave 2',
         before =  paste('w1', before, sep = '_'),
         after = paste('w2', after, sep = '_')) 

step_2 <- adult_panel  %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, 
         as.numeric(R03_ADULTTYPE)==1,
         current_est_smoker_w1 ==1 ) %>% 
  dplyr::select(smoking_status_full_w2, smoking_status_full_w3)  %>%
  drop_na(smoking_status_full_w2,
          smoking_status_full_w3) %>% 
  count(smoking_status_full_w2, smoking_status_full_w3) %>% 
  rename(before = smoking_status_full_w2,
         after =  smoking_status_full_w3,
         flow = n) %>% 
  mutate(step_from = 'wave 2',
         step_to = 'wave 3',
         before = paste('w2', before, sep = '_'),
         after = paste('w3', after, sep = '_')) 
  

cur_est_sankey_df <- rbind(step_1, step_2)
View(cur_est_sankey_df)
write.csv(cur_est_sankey_df, 'Output/cur_est_sankey_df.csv', row.names = F)


#### W1 Cur. Est. Smokers  (Include Missing Data) ####

step_1 <- adult_panel  %>% 
  dplyr::filter(current_est_smoker_w1 == 1) %>% 
  dplyr::select(smoking_status_full_w1, smoking_status_full_w2)  %>%
  count(smoking_status_full_w1, smoking_status_full_w2) %>%  
  rename(before = smoking_status_full_w1,
         after =  smoking_status_full_w2,
         flow = n) %>%
  mutate(step_from = 'wave 1',
         step_to = 'wave 2',
         before =  paste('w1', before, sep = '_'),
         after = paste('w2', after, sep = '_'))  

step_2 <- adult_panel  %>%
  dplyr::filter(current_est_smoker_w1 == 1) %>% 
  dplyr::select(smoking_status_full_w2, smoking_status_full_w3)  %>%
  count(smoking_status_full_w2, smoking_status_full_w3) %>% 
  rename(before = smoking_status_full_w2,
         after =  smoking_status_full_w3,
         flow = n) %>% 
  mutate(step_from = 'wave 2',
         step_to = 'wave 3',
         before = paste('w2', before, sep = '_'),
         after = paste('w3', after, sep = '_'))
sankey_df <- rbind(step_1, step_2)
sankey_df <- sankey_df %>% 
  mutate(before = str_replace_all(before, 'NA', 'missing'),
         after = str_replace_all(after, 'NA', 'missing')) 
sankey_df %>%  View
write.csv(sankey_df, 'Output/sankey_cur_est_smokers_non_response_df.csv', row.names = FALSE)
