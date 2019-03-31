source('source.R')
options(survey.replicates.mse=TRUE)

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
            type = 'Fay',
            rho = 0.3)
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

adult_sub_w2  %>%  nrow
adult_panel %>%  
  filter(wave_1 == 1 | wave_2 == 1) %>% 
  mutate(pweight = if_else(wave_2 ==1, R02_A_PWGT, R01_A_PWGT),
         missing_pweight = is.na(pweight))  %>% 
  group_by(wave_1, wave_2,  missing_pweight) %>% 
  count

adult_panel %>% 
  filter(wave_1 == 1 | wave_2 == 1) %>% 
  mutate(missing_w1_weight = ifelse(is.na(R01_A_PWGT), 1, 0),
         missing_w2_weight = ifelse(is.na(R02_A_PWGT), 1, 0)) %>% 
  group_by(missing_w1_weight, missing_w2_weight) %>% 
  count

adult_panel$wave
adult_merged_weights$pweight %>%  is.na %>%  sum

adult_merge_weights$p_weight
adult_panel %>% 
  group_by(wave_1, wave_2, wave_3) %>% 
  count

adult_panel$R02_NEW_BASELINE_ADULT_LD %>%  table
adult_panel$R02_CONTINUING_ADULT_LD %>%  table
adult_panel$R03_ADULTTYPE %>%  table

adult_merged_weights <- adult_panel %>% 
                              filter(wave_1 == 1 | wave_2 == 1) %>% 
                              mutate( pweight = if_else(wave_2 ==1, R02_A_PWGT, R01_A_PWGT)
)

adult_merged_weights %>%  select(pweight)   %>%  is.na %>%  table
.grepl()
rep_weights_mat_w1 <- adult_merged_weights %>% 
                          select(starts_with('R01_A_PWGT')) %>% 
                          select(-R01_A_PWGT) 

rep_weights_mat_w2 <- adult_merged_weights %>% 
                          select(starts_with('R02_A_PWGT')) %>% 
                           select(-R02_A_PWGT)

merged_rep_weights <- rep_weights_mat_w2
merged_rep_weights[is.na(rep_weights_mat_w2)] <- rep_weights_mat_w1[is.na(rep_weights_mat_w2)]
is.na(merged_rep_weights) %>%  sum

adult_merged_weights[, grep('R02_A_PWGT[1-9]+', names(adult_merged_weights))] <- merged_rep_weights 
names(adult_merged_weights_final ) %>%  tail

  grep('R01_A_PWGT[1-9]+', names(adult_merged_weights)) 
rep_weights_mat_w1 <-grep('R02_A_PWGT[1-9]+', names(adult_merged_weights)) 

  mutate(missing_weight = is.na(pweight) )%>% 
                                   group_by(missing_weight, wave_1) %>% 
                                   count
survey_merged <- svrepdesign(data = adult_merged_weights  ,
                             weights = ~ pweight,
                             repweights = 'R02_A_PWGT[1-9]+',
                             type = 'Fay',
                             rho = 0.3)

survey_w2 <- svrepdesign(data = adult_sub_w2 ,
                         weights = ~ R02_A_PWGT,
                         repweights = 'R02_A_PWGT[1-9]+',
                         type = 'Fay',
                         rho = 0.3)
adult_w2 %>% 
  filter(R02_CONTINUING_ADULT_LD == '(1) 1 = Yes') %>% 
  count(gender_w2) 


continuing_adults_w2 <- svrepdesign(data = adult_sub_w2 ,
                         weights = ~ R02_A_PWGT,
                         repweights = 'R02_A_PWGT[1-9]+',
                         type = 'Fay',
                         rho = 0.3)

continuing_adults <- adult_w2 %>% 
  filter(R02_CONTINUING_ADULT_LD == '(1) 1 = Yes')


continuing_survey_w2 <- svrepdesign(data = continuing_adults ,
                                    weights = ~ R02_A_PWGT,
                                    repweights = 'R02_A_PWGT[1-9]+',
                                    type = 'Fay',
                                    rho = 0.3)

svymean(~gender_w2, design =continuing_survey_w2,
        na.rm =T)

svymean(~race_ethnicity_w2, design =continuing_survey_w2,
        na.rm =T)

svymean(~education_w2, design =continuing_survey_w2,
        na.rm =T)


svymean(~white_w2, design =continuing_survey_w2,
        na.rm =T)
continuing_adults %>% 
  count(R02R_A_AM0018) 

continuing_adults <- continuing_adults %>% 
  mutate(white_w2 = fct_collapse(race_ethnicity_w2,
    'NH White' =   'NH White',
    'Other' = 'Hispanic',
    'Other' = 'NH Black',
    'Other' = 'Other' )) 

continuing_adults %>% 
  count(white_w2)


  count(race_ethnicity_w2) 
continuing_adults %>% 
  count(education_w2) 
adult_w2 %>% 
  pull(cig_use_now_w2) %>% 
  as.logical %>% 
  sum(na.rm =T )
  

adult_sub_w2 %>%  
  filter(R02_CONTINUING_ADULT_LD)
svrepdesign(data = adult_sub_w2 ,
            weights = ~ R02_A_PWGT,
            repweights = 'R02_A_PWGT[1-9]+',
            type = 'Fay',
            rho = 0.3)
svymean(~gender_w2, 
        design = survey_w2, 
        na.rm =T)
?svytable
  
adult_w2 %>% 
  count(R02_AE1004)


cur_est_survey_w2 <- svrepdesign(data = cur_est_w1_at_w2  ,
                         weights = ~R02_A_PWGT,
                         repweights = 'R02_A_PWGT[1-100]',
                         type = 'Fay',
                         rho = 0.3, 
                         mse = TRUE)

svymean(~gender_w1, survey_w1, na.rm =T)
svymean(~gender_w2, survey_w2, na.rm =T)
svymean(~gender_w2, survey_merged, na.rm = T)
svymean(~education_w2, survey_w2, na.rm =T)
svymean(~smoking_status_w2, cur_est_survey_w2, na.rm =T)*100

table(adult_sub_w2$education_w2)

?svyglm
?multinom()
#### Wave 3 ####

load("Input/36498-3101-Data.rda")
load("Input/36498-3102-Data.rda")
all_waves_weights<- da36498.3101
single_waves_weights <- da36498.3102

adult_w3_merged <- adult_panel %>% 
                left_join(all_waves_weights, by = c('PERSONID')) 
adult_w3_merged_cc <- adult_w3_merged %>% 
                             filter(!is.na(R03_A_AWGT))

?as_survey.svyrep.design
all_waves_weights_design <- svrepdesign(data = adult_w3_merged_cc ,
                         weights = ~R03_A_AWGT,
                         repweights = 'R03_A_AWGT[1-100]',
                         type = 'Fay',
                         rho = 0.3)
survey_w3 <- as_survey(all_waves_weights_design)
survey_w3 %>%  str
survey_w3$variables$cur_est_smoker
?srvyr::filter
survey_w3$variables$smoked_past30D_w3
survey_w3 %>% 
 filter(current_est_smoker_w1 == 1) %>% 
  svymean(x = ~smoked_past30D_w3, na.rm =T) 

survey_w3 %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  mutate(smoked_past12M_w3 = 
           if_else(smoked_past30D_w3 ==1, 1, smoked_past12M_w3)) %>% 
  svymean(x = ~smoked_past12M_w3, na.rm =T) 

1-0.93181
plot(x, (1 -(0.06819))^(x/2))
library(survival)

?survreg
survreg(smoked_past30D_w3)
adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  count(smoked_past30D_w2,smoked_past30D_w3,
        wave_2, wave_3) %>%  print(n =40) %>% 
  ggplot(aes(x=  as.factor(smoked_past30D_w2), y = n,
             fill = as.factor(smoked_past30D_w2))) + 
  geom_col()


adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  count(smoked_past30D_w2,smoked_past30D_w3,
        wave_2, wave_3) %>%  print(n =40) %>% 
  ggplot(aes(x=  as.factor(smoked_past30D_w3), y = n,
             fill = as.factor(smoked_past30D_w3))) + 
  geom_col()


library(vcd)
library(ggmosaic)
adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  count(smoked_past30D_w2) %>% 
  mutate(prop = n / sum(n)) %>% 
  print(n=13) %>% 
  ggplot(aes(x = as.factor(smoked_past30D_w2), y = n)) +
  geom_tile() +
  theme(legend.position = 'none') 

 %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  
  ggplot(adult_panel) +
  geom_mosaic(aes(x = product(as.factor(smoked_past30D_w2))))
adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  ggplot(color = 'black')+
  geom_mosaic(aes(x = product(as.factor(smoked_past30D_w3), 
                              smoked_past30D_w2),
                  fill = as.factor(smoked_past30D_w3)),
              divider = mosaic('h')) +
  theme(legend.position = 'none') +
  theme_classic()

margins <- adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  count(smoked_past30D_w3) %>% 
  rename(n_sub = n) %>% 
  mutate(smoked_past30D_w3 = as.factor(smoked_past30D_w3))



quit_table <- adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  select(smoked_past30D_w2, smoked_past30D_w3) %>% 
  drop_na(smoked_past30D_w2, smoked_past30D_w3) %>%
  mutate(quit_status_w3 = case_when(
          smoked_past30D_w2==1 & smoked_past30D_w3==0 ~ 'Quit at w3', 
          smoked_past30D_w2==0 & smoked_past30D_w3==0 ~ 'Quit at w2', 
          smoked_past30D_w2==0 & smoked_past30D_w3==1 ~ 'Relapsed at w3',
          smoked_past30D_w2==1 & smoked_past30D_w3==1 ~ 'Stayed Smoker'),
         quit_status_w3 = as.factor(quit_status_w3),
         smoked_past30D_w3 = as.factor(smoked_past30D_w3)) %>% 
  drop_na(quit_status_w3) %>%
  count(quit_status_w3, smoked_past30D_w3) %>% 
  left_join(margins)

quit_table %>% 
  mutate(n_prop = n_sub*3.9 / sum(n_sub)) %>% 
  ggplot()+
  geom_col(aes(x = smoked_past30D_w3, y =n,
               fill = quit_status_w3,  width = n_prop), position = 'fill') +
  scale_fill_manual(values = c('blue', 'cyan',  'pink', 'red' ))
?scale

qui
quit_table %>% 
  mutate(smoker_w2 = if_else(quit_status_w3 == 'Quit at w2' | 
                               quit_status_w3 == 'Relapsed at w3', 0, 1),
        # smoker_w2 = as.integer(smoker_w2),
         n0 = if_else(smoker_w2 ==0, n, as.integer(0)),
         n1 = if_else(smoker_w2 ==1, n, as.integer(0)),
        n_sub_2 = if_else(smoker_w2 ==0, sum(n0), sum(n1)),
        prop_2 = n_sub_2*4 / sum(n_sub_2),
        smoker_w2 = as.factor(smoker_w2)) %>% 
  select(-n0, -n1) %>% 
  ggplot(aes(x = smoker_w2, y = n, fill =  quit_status_w3, width = prop_2)) +
  geom_col(position = 'fill', color = 'white', size = 2  )+
  scale_fill_manual(values = c('blue', 'cyan',  'pink', 'red' )) +
  scale_x_discrete(breaks = 0:2,   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'none') +
  geom_text(aes(label = quit_status_w3), position = position_fill(vjust = 0.5),
            size = 2.5, color = 'black', fontface = "bold")
  annotate_
?xlim
?geom_text
adult_panel$R02_CONTINUING_ADULT_LD
adult_panel %>% 
  filter(wave_2 ==1,as.numeric(R02_CONTINUING_ADULT_LD)==1) %>% 
  group_by(cig_use_ever_w1, smoked_past12M_w2,smoked_past30D_w2) %>% 
  count %>%  print(n =120)


  mutate(initiated = if_else(smoked_past12M_w2 ==1 | smoked_past30D_w2==1, 1, 0),
         current_est_smoker_w2 = as.factor( current_est_smoker_w2),
         initiated = as.factor(initiated)) %>% 
  group_by(initiated,  smoking_status_full_w2) %>% 
  count() %>% 
  ggplot(aes(x = initiated, y= n, fill = smoking_status_full_w2 )) +
  geom_col()

#INITIATION: How many intiated smokers became cur. est. at w2?
adult_panel %>% 
  filter(as.numeric(R02_CONTINUING_ADULT_LD)==1, cig_use_ever_w1 ==0) %>% 
  mutate(initiated = if_else(smoked_past12M_w2 ==1 | smoked_past30D_w2==1, 1, 0)) %>% 
  filter(initiated ==1) %>% 
  group_by(current_est_smoker_w2) %>% 
  count
  
 
  

adult_panel %>% 
  filter(current_est_smoker_w1 == 1) %>% 
  drop_na(smoked_past30D_w2, smoked_past30D_w3) %>%
  mutate(quit_status_w3 = case_when(
    smoked_past30D_w2==1 & smoked_past30D_w3==0 ~ 'Quit at w3', 
    smoked_past30D_w2==0 & smoked_past30D_w3==0 ~ 'Quit at w2', 
    smoked_past30D_w2==0 & smoked_past30D_w3==1 ~ 'Relapsed at w3',
    smoked_past30D_w2==1 & smoked_past30D_w3==1 ~ 'Stayed Smoker'),
    quit_status_w3 = as.factor(  quit_status_w3),
    smoked_past30D_w2 = as.factor(smoked_past30D_w2)) %>% 
  drop_na(quit_status_w3) %>%
  ggplot() +
  geom_mosaic(aes(x = product( quit_status_w3, smoked_past30D_w3),
                  fill = quit_status_w3),
              divider = mosaic('h')) +
  theme(legend.position = 'none',   axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  theme_classic() +
  xlab('Smoking at Wave 3') +
  ylab(NULL)


?geom_mosaic
?geom_tree                
adult_panel$smoked_past30D_w3
?e
?theme
?geom_bar
cur_est_smokers_w1 <- adult_panel %>% 
  filter(current_est_smoker_w1 == 1)


mosaicplot(~smoked_past30D_w2+ smoked_past30D_w3,
           color =T, data = cur_est_smokers_w1,
           na.action = stats::na.pass)
?na.action
?mosaicplot
1-  0.87659 
x <- seq(0, 50, 2)
plot(x, (1 -(0.12341))^(x/2))
adult_panel$cur_est_smoker
all_waves_weights_design %>% 
 svymean()
?svymean
adult_w3 %>% 
  count(cig_use_now_w3, smoked_past30D_w3) 
adult_w3$quit
#### NEXT ####

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


adult_w3 %>%  
          left_join(single_waves_weights_design)
survey_w3 <- svrepdesign(data = adult_w3_merged, 
                         weights = ~R03_A_AWGT,
                         repweights = 'AWGT[1-100]',
                         type = 'Fay',
                         rho = 0.3)
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




adult_w3_merged  <- adult_w3_merged %>% 
  mutate_if(.predicate = grepl(names(adult_w3_merged), pattern = 'A_AWG'), 
            .funs = function(x) { ifelse(is.na(x), 1, x)}) %>% 
  select(R03_A_AWGT) 

