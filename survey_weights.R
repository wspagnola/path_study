source('source.R')

adult_wide <- adult_panel %>% 
  select(PERSONID,contains('w2'), contains('w1')) 

adult_wide %>% 
  select(-smoked_past12M_w2, -current_est_smoker_w2,
         -current_est_smoker_w2, -never_smoker_w2,
         -former_est_smoker_w2, -current_non_est_smoker_w2,
         -smoked_past30D_w2, -current_est_smoker_w1,
         -current_est_smoker_w1, -never_smoker_w1,
         -former_est_smoker_w1, -current_non_est_smoker_w1,
         -abs_w2, -cigarette_use_ever_w1) %>% 

  
colA = paste("gender", 1:2, sep = "_w")
colB = paste("hispanic", 1:2, sep = "_w")
colC = paste("race", 1:2, sep = "_w")
colD = paste("sexual_orientation", 1:2, sep = "_w")
colE = paste("age", 1:2, sep = "_w")
colF = paste("income", 1:2, sep = "_w")
colG = paste('smoking_status', 1:2, sep = "_w")

adult_wide %>% 
  select(PERSONID, gender_w1, gender_w2, 
         hispanic_w1, hispanic_w2, smoking_status_w1,
         smoking_status_w2) %>% 
       as.data.table %>% 
      melt(measure.vars =  list(colA, colB, colG),
        value.name = c("gender", "hispanic", 'smoking status')) %>% 
      arrange(PERSONID) %>%  
      head(n = 15)

#Maybe add weights next
adult_wide %>% 
  gather(key = 'Wave', value = 'cigarette_current_freq', 
                cigarette_current_freq_w1, cigarette_current_freq_w2) %>% 
  mutate(Wave = ifelse(grepl(Wave, pattern ='w1'), '1', '2')) %>% 
  head
adult_wide <
adult_w1_long <- adult_w1 %>%
                  select(PERSONID, contains('w1'), contains('PWGT')) 

adult_w2_long <- adult_w2 %>%
                    select(PERSONID, contains('w2'), contains('PWGT')) 

names(adult_w1_long) <- names(adult_w1_long) %>% 
                           str_remove('_w1') %>% 
                           str_remove('R01_')
names(adult_w2_long) <- names(adult_w2_long) %>% 
                              str_remove('_w2') %>% 
                              str_remove('R02_')
adult_w1_long$wave <- 1
adult_w1_long$wave <- 2

adult_w2_long$cigarette_use_ever <- NA

adult_w1_long$smoked_past12M <- NA
adult_w1_long$smoked_past30D <- NA
adult_w1_long$days_quit_cigs<- NA
adult_w1_long$days_quit_cigs<- NA

x <- which(names(adult_w2_long) %in% names(adult_w1_long) == F)
names(adult_w2_long)[x]
rbind(adult_w1_long, adult_w2_long)
  str_remove_all(names(adult_w1), pattern = '_w1')
str_re
svydesign(data = adult_w1, ids= ~1, strata = NULL)



adult_w1$wave <- 1
adult_w2$wave <- 2

adult_survey <- adult_w1 %>% 
                    left_join(adult_w2, by = 'PERSONID')

?gather
adult_survey %>% 
  select(PERSONID,age_w1, age_w2, gender_w1, gender_w2,
         R01_A_PWGT, R02_A_PWGT) %>% 
  gather(key = 'Wave', value = 'Age', age_w1,age_w2) %>% 
  gather(key = 'Wave', value = 'Gender', gender_w1, gender_w2) %>% 
  arrange(PERSONID) %>% 
  head
  gather()
## Create Survey Design with replication weights
#Balanced Repated Replication method (BRR)
#Fay's adjustment = 0.3
str(survey_w1)

#### Wave 1 ####
survey_w1 <- svrepdesign(data = adult_w1,
            weights = ~R01_A_PWGT,
            repweights = 'R01_A_PWGT[1-100]',
            type = 'BRR',
            rho = 0.3, 
            mse = TRUE,
            combined.weights = TRUE)
svymean(~age_w1, survey_w1, na.rm =T)  %>%  round(3) *100
svymean(~gender_w1, survey_w1, na.rm =T) %>%  round(3) *100
svymean(~education_w1, survey_w1, na.rm =T) %>%  round(3) *100
svymean(~income_w1, survey_w1, na.rm =T) %>%  round(3) *100
svymean(~sexual_orientation_w1, survey_w1, na.rm =T)

#### Wave 2  ####

adult_sub_w2 <- adult_panel %>% 
  select(PERSONID, ends_with('w2'), starts_with('R02_A_PWGT'), starts_with('wave'),
        smoking_status_w1) %>% 
  filter(wave_2 == 1)
adult_sub_w2 %>% names

cur_est_w1_at_w2 <- adult_panel %>% 
  select(PERSONID, ends_with('w2'), starts_with('R02_A_PWGT'), starts_with('wave'),
         smoking_status_w1) %>% 
  filter(wave_2 == 1,  smoking_status_w1 == 'current_est_smoker') 

colSums(is.na(adult_sub_w2))
survey_w2 <- svrepdesign(data = adult_sub_w2 ,
                         weights = ~ R02_A_PWGT,
                         repweights = 'R02_A_PWGT[1-9]+',
                         type = 'Fay',
                         rho = 0.3)
options(survey.replicates.mse=TRUE)
cur_est_survey_w2 <- svrepdesign(data = cur_est_w1_at_w2  ,
                         weights = ~R02_A_PWGT,
                         repweights = 'R02_A_PWGT[1-100]',
                         type = 'Fay',
                         rho = 0.3, 
                         mse = TRUE,
                         combined.weights = TRUE)
withReplicates(survey_w2, theta )
svymean(~gender_w2, survey_w2, na.rm =T)

svymean(~education_w2, survey_w2, na.rm =T)
svymean(~smoking_status_w2, cur_est_survey_w2, na.rm =T)*100

table(adult_sub_w2$education_w2)
library(nnet)
?svyglm
?multinom()
#### Wave 3 ####

load("Input/36498-3101-Data.rda")
load("Input/36498-3102-Data.rda")
all_waves_weights<- da36498.3101
single_waves_weights <- da36498.3102

adult_w3_merged <- adult_w3 %>% 
                left_join(all_waves_weights, by = c('PERSONID')) %>% 
                left_join(single_waves_weights,  by = c('PERSONID'))
adult_w3_merged_cc <- adult_w3_merged %>% 
                            filter(!is.na(R03_A_AWGT))
adult_w3_merged$R03_A_S
adult_w3_merged %>% 
  group_by(gender_w3) %>% 
  count
?mutate_if
all_weights_idx <- 
adult_w3_merged  <- adult_w3_merged %>% 
                          mutate_if(.predicate = grepl(names(adult_w3_merged), pattern = 'A_AWG'), 
                                    .funs = function(x) { ifelse(is.na(x), 1, x)}) %>% 
                          select(R03_A_AWGT) 

all_waves_weights_design <- svrepdesign(data =  adult_w3_merged_cc ,
                         weights = ~R03_A_AWGT*R03_A_SWGT,
                         repweights = 'AWGT[1-100]|SWGT[1-100]',
                         type = 'BRR',
                         rho = 0.3, 
                         combined.weights = TRUE)
svymean(~factor(gender_w3), all_waves_weights_design, na.rm = T)*2

adult_w3_merged$R03_A_S
single_waves_weights_design <- svrepdesign(data =  adult_w3_merged ,
                                        weights = ~R03_A_SWGT,
                                        repweights = 'SWGT[1-100]',
                                        type = 'BRR',
                                        rho = 0.3, 
                                        combined.weights = TRUE)
svymean(~gender_w3, single_waves_weights_design, na.rm = T)
svy_mean 
all_waves_weights_design
survey_w3 <- svrepdesign(data = adult_w3_merged_cc, 
                                        weights = ~R03_A_SWGT + R03_A_AWGT,
                                        repweights = c('AWGT[1-100]|SWGT[1-100]'),
                                        type = 'BRR',
                                        rho = 0.3, 
                                        combined.weights = TRUE)
names(single_waves_weights_design)
data.frame()
str(survey_w2)
merge(survey_w2, survey_w3)

adult_w3 %>%  
          left_join(single_waves_weights_design)
survey_w3 <- svrepdesign(data = all_waves_weights_w3, 
                         weights = ~R03_A_AWGT,
                         repweights = 'AWGT[1-100]',
                         type = 'BRR',
                         rho = 0.3, 
                         combined.weights = TRUE)
adult_w3$R03_A_SGWT
svymean(x = ~ age_w1, design = survey_w1, na.rm =T)
svymean(x = ~ race_w1, design = survey_w1, na.rm =T)

#### SMOKING TRANSITION TABLE ####
table(adult_panel$smoking_status_w2)
table(adult_panel$smoking_status_w1)
table(adult_panel$smoking_status_w1, adult_panel$smoking_status_w2)
adult_panel %>% 
  select(PERSONID, 
         smoking_status_w1, 
         smoking_status_w2, 
         smoking_status_w3) %>% 
  mutate(transition_w2 = case_when(
         smoking_status_w1 == smoking_status_w2 ~ 'No Change',
         smoking_status_w1 == 'never_smoker' &
            (smoking_status_w2 != 'never_smoker' & 
               !is.na(smoking_status_w2) )   ~ 'Initiation',
         smoking_status_w1 == 'current_est_smoker' &
           smoking_status_w2 == 'former_est_smoker' ~ 'Cessation',
         (smoking_status_w1 == 'former_est_smoker'|
          smoking_status_w1 == 'current_non_est_smoker') &
           smoking_status_w2 == 'current_est_smoker'~ 'Relapse')) %>% 
      group_by(transition_w2) %>% 
  count
