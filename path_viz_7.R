#Wave 1-3: Descriptives

#Panel 3 Barplots
#panel 3 = across waves 1, 2, 3

source('source.R')

panel_3_wave_labs <- c('Wave 1', 'Wave 2', 'Wave 3')

#### Gender (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('gender_w1', 'gender_w2', 'gender_w3'), label = 'Gender') %>% 
  ggplot(aes(x = Wave, y = n, fill = Gender)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_manual(labels = c('Male', 'Female'),
                    values = c(gg_blue, gg_red)) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 18000, 2000),
                     limits = c(0, 18000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Gender Composition') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Gender_Barplot.png')


#### Age (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('age_w1', 'age_w2', 'age_w3'), label = 'Age') %>% 
  ggplot(aes(x = Wave, y = n, fill = Age)) +
  geom_col(position = position_dodge(), color = 'black', width = .8) +
  scale_fill_discrete (labels = c('18 to 34', '35 to 64','65 and older')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 16000, 2000),
                     limits = c(0, 16000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Age Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Age_Barplot.png', width = 8)


#### Race (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('race_w1', 'race_w2', 'race_w3'), label = 'Race') %>% 
  ggplot(aes(x = Wave, y = n, fill = Race)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_discrete(labels = c('White', 'Black', 'Other')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 24000, 2000),
                     limits = c(0, 24000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Race Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Race_Barplot.png')

#### Hispanic Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('hispanic_w1', 'hispanic_w2', 'hispanic_w3'), label = 'Hispanic') %>% 
  ggplot(aes(x = Wave, y = n, fill = Hispanic)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_discrete(labels = c('Hispanic', 'Non-Hispanic')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 28000, 2000),
                     limits = c(0, 28000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Hispanic Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Hispanic_Barplot.png')

#### Education Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('education_w1', 'education_w2', 'education_w3'), 
                    label = 'Education') %>% 
  ggplot(aes(x = Wave, y = n, fill = Education)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_discrete(labels = c("Less than High School", "High School", 
                              "Some College or AA", "Bachelor's or More")) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Education Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Education_Barplot.png', width = 10)

#### Income Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('income_w1', 'income_w2', 'income_w3'), 
                    label = 'Income') %>% 
  ggplot(aes(x = Wave, y = n, fill = Income)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_discrete(labels = c("Less than $25,000", "$25,000 to $49,999",
                                 "$50,000 to $99,999", "More than $100,000")) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 14000, 2000),
                     limits = c(0, 14000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Income Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Income_Barplot.png', width = 8)

#### Sexual Orientation Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('sexual_orientation_w1', 
                          'sexual_orientation_w2', 
                          'sexual_orientation_w3'), 
                    label = 'Sexual_Orientation') %>% 
  ggplot(aes(x = Wave, y = n, fill = Sexual_Orientation)) +
  geom_col(position = position_dodge(),  color = 'black', width = .5) +
  scale_fill_discrete(labels =  c("Lesbian, Gay, Bisexual or Something Else","Straight")) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 32000, 2000),
                     limits = c(0, 32000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Sexual Orientation Distribution Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Sexual_Orientation_Barplot.png', width = 10, height = 6)


#### Psychological Distress Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('psychdist_w1', 
                          'psychdist_w2', 
                          'psychdist_w3'), 
                    label = 'Psychdist') %>% 
  ggplot(aes(x = Wave, y = n, fill = factor(Psychdist))) +
  geom_col(position = position_dodge(),  color = 'black', width = .5) +
  scale_fill_discrete(labels =  c('Not Distressed', 'Distressed')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 24000, 2000),
                     limits = c(0, 24000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Psychological Distress Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Psychological_Distress_Barplot.png')

#### Smoking Status  Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('smoking_status_full_w1', 
                          'smoking_status_full_w2', 
                          'smoking_status_full_w3'), 
                    label = 'Smoking_Status') %>% 
  ggplot(aes(x = Wave, y = n, fill = factor(Smoking_Status))) +
  geom_col(position = position_dodge(),  color = 'black', width = .5) +
  scale_fill_discrete(labels = c('Current Established Smokers',
                                 'Current Experimental Smokers',
                                 'Former Established Smokers', 
                                 'Current Non-Established Smokers',
                                 'Never Smokers')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Smoking Status (5 Categories') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Smoking_Status_Barplot.png', width = 12)


#### Current Smoking Frequency Barplot (Panel 3) ####
adult_panel %>% 
  prep_panel_3_data(x = c('cigarette_current_freq_w1', 
                          'cigarette_current_freq_w2', 
                          'cigarette_current_freq_w3'), 
                    label = 'Cigarette_Current_Freq') %>% 
  ggplot(aes(x = Wave, y = n, fill = Cigarette_Current_Freq)) +
  geom_col(position = position_dodge(),  color = 'black', width = .5) +
  scale_fill_discrete(labels = c('Every day',
                                 'Some days', 
                                 'Not at all')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Smoking Frequency Across Waves 1 to 3') +
  xlab(NULL) 
ggsave('Figures/Panel_3_Smoking_Frequency_Barplot.png', width = 8)


#### Abstinence Length Barplot (Panel 3) ####

#Waves 2 Abstinence Length
adult_panel %>% 
      group_by(abst_w2) %>% 
      count
   
#I am only getting 477 Smokers who have quit for more than a year 
adult_panel %>%  filter(R02_AC1009_NN >= 365 & as.numeric(R02_AC1009_UN) ==1) %>%  nrow
adult_panel %>%  filter(R02_AC1009_NN >= 12 & as.numeric(R02_AC1009_UN) ==2) %>%  nrow

#Waves 3 Abstinence Length
adult_panel %>% 
  group_by(abs_w3) %>% 
  count


####Quit Rate (W2)  ####
adult_panel %>% 
  group_by(quit_w2) %>% 
  count %>% 
  drop_na(quit_w2) %>% 
  ggplot(aes(x = quit_w2, y = n, fill = quit_w2)) +
    geom_col(color = 'black',  width = 0.5) + 
    ggtitle('Quit Status of Wave 1 Smokers at Wave 2') +
    xlab('Quit') +
    my_theme_wave
ggsave('Figures/W2_Quit_Rate_Barplot.png')

# Quit Rate Categories (W2)
adult_panel %>% 
  group_by(quit_cat_w2) %>% 
  count %>% 
  drop_na(quit_cat_w2) %>% 
  ggplot(aes(x = quit_cat_w2, y = n, fill = quit_cat_w2)) +
    geom_col(color = 'black') +
  scale_x_discrete(labels = c('Yes*', 'No*', 'No Change**')) +
  ggtitle('Wave 2: Quit Categories for Wave 1 Smokers and Non-Smokers') +
  xlab('*Quit Status for Wave 1 Smokers at Wave 2. 
       \n**Wave 1 Non-Smokers who were still non-smokers at Wave 2') +
  theme_wave_quit 
ggsave('Figures/W2_Quit_Categories_Barplot.png', width = 8)

# Quit Rate (w3)
adult_panel %>% 
  group_by(quit_w3) %>% 
  count %>% 
  drop_na(quit_w3) %>% 
  ggplot(aes(x = quit_w3, y = n, fill = quit_w3)) +
    geom_col(color = 'black', width = 0.5) + 
    ggtitle('Quit Status of Wave 1 Smokers at Wave 3') +
    xlab('Quit') +
    my_theme_wave +
ggsave('Figures/W3_Quit_Rate_Barplot.png')

# Quit Rate Categories (w3)
adult_panel %>% 
  group_by(quit_cat_w3) %>% 
  count %>% 
  drop_na(quit_cat_w3) %>% 
  ggplot(aes(x = quit_cat_w3, y = n, fill = quit_cat_w3)) +
    geom_col(color = 'black') +
    scale_x_discrete(labels = c('Yes*', 'No*', 'No Change**')) +
    ggtitle('Wave 3: Quit Categories for Wave 1 Smokers and Non-Smokers') +
    xlab('*Quit Status for Wave 1 Smokers at Wave 3. 
          \n**Wave 1 Non-Smokers who were still non-smokers at Wave 3') +
    theme_wave_quit 
ggsave('Figures/W3_Quit_Categories_Barplot.png', width = 8)


#### CREATE TABLES ####

 tab <- adult_panel %>% 
  prep_panel_3_data(x = c('smoking_status_full_w1', 
                          'smoking_status_full_w2', 
                          'smoking_status_full_w3'), 
                    label = 'Smoking_Status') 
tab  %>%  
  split(f = tab$Wave) %>% 
  bind_cols %>% 
  select(-Wave, -Wave1, -Wave2, -Smoking_Status1, -Smoking_Status2) %>% 
  rename('Wave_1' = n ,
        'Wave_2' = n1,
        'Wave_3' =  n2) %>% 
  write.csv(file = 'Smoking_Status_Full_W123.csv')


