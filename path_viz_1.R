#These are Visualizations of Wave 1 and Wave 2
#Data corresponds to 'Descriptive' Sheet of Excel File


source('source.R')

#### GENDER BARPLOT (PANEL) ####
adult_panel %>% 
  select(gender_w1, gender_w2) %>% 
  gather(key = Wave, value = Gender) %>% 
  group_by(Wave, Gender) %>% 
  count %>% 
  filter(!is.na(Gender)) %>% 
  ggplot(aes(x = Wave, y = n, fill = Gender)) +
  geom_col(position = position_dodge(), width = .5, color = 'black') +
  scale_fill_manual(labels = c('Male', 'Female'),
                    values = c(gg_blue, gg_red)) +
  scale_x_discrete(labels = c('Wave 1', 'Wave 2')) +
  scale_y_continuous(breaks = seq(0, 18000, 2000),
                     limits = c(0, 18000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Gender Composition') +
  xlab(NULL) 
ggsave('Figures/Panel_2_Gender_Barplot.png')
  

#### AGE BARPLOT (PANEL)  (####
adult_panel %>% 
  select(age_w1, age_w2) %>% 
  gather(key = Wave, value = Age) %>%  
  group_by(Wave, Age) %>% 
  count() %>% 
  filter(!is.na(Age)) %>% 
  ggplot(aes(x = Age, y = n, fill = Age)) +
  geom_col(color = 'black', width = 0.4) +
  facet_wrap(~Wave, 
             labeller = labeller(Wave = c(age_w1 ='Wave 1', age_w2 ='Wave 2'))) +
  ggtitle('Age Distribution') + 
  scale_x_discrete(labels = c('18 to 34',
                              '35 to 64',
                              '65 and older')) +
  scale_y_continuous(breaks = seq(0, 16000, 2000),
                     limits = c(0, 16000),
                     expand = c(0, 0)) +
  xlab('') +
  my_theme_wave
ggsave('Figures/Panel_2_Age_Barplot.png', width = 8)


#### RACE BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'race_w1', w2 = 'race_w2', label = 'Race') %>% 
  ggplot(aes(x = Race, y = n, fill = Race)) +
  geom_col(color = 'black', width = 0.4) +
  facet_wrap(~Wave, 
             labeller = labeller(Wave = c(wave_1 ='Wave 1', wave_2 ='Wave 2'))) +
  ggtitle('Race of Respondents') +
  scale_x_discrete(labels = c('White', 'Black', 'Other')) +
  scale_y_continuous(breaks = seq(0, 24000, 2000),
                   limits = c(0, 24000),
                   expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave
ggsave('Figures/Panel_2_Race_Barplot.png')

#### HISPANIC BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'hispanic_w1', w2 = 'hispanic_w2', label = 'Hispanic') %>% 
  ggplot(aes(x = Hispanic, y = n, fill = Wave)) +
  geom_col(color = 'black', width = 0.7, position = position_dodge()) +
  ggtitle("Hispanic") +
  scale_x_discrete(labels= c('Hispanic', 'Not Hispanic')) +
  scale_y_continuous(breaks = seq(0, 28000, 2000),
                     limits = c(0, 28000),
                     expand = c(0, 0)) +
  scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
  xlab(NULL) +
  my_theme_panel
ggsave('Figures/Panel_2_Hispanic_Barplot.png')

#### EDUCATION BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'education_w1', 
                  w2 = 'education_w2', 
                  label = 'Education') %>% 
  ggplot(aes(x = Education, y = n, fill = Wave)) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  ggtitle("Education Across Waves 1 and 2") +
  scale_x_discrete(labels = c("Less than High School", "High School", 
                              "Some College or AA", "Bachelor's or More")) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
  xlab(NULL) +
  my_theme_panel
ggsave('Figures/Panel_2_Education_Barplot.png', width = 8)

#### INCOME BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'income_w1', 
                  w2 = 'income_w2', 
                  label = 'Income') %>% 
  ggplot(aes(x = Income, y = n, fill = Wave)) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  ggtitle("Income Distribution Across Waves 1 and 2") +
  scale_x_discrete(labels = c("Less than $25,000", "$25,000 to $49,999",
                              "$50,000 to $99,999", "More than $100,000")) +
  scale_y_continuous(breaks = seq(0, 14000, 2000),
                     limits = c(0, 14000),
                     expand = c(0, 0)) +
  scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
  xlab(NULL) +
  my_theme_panel
ggsave('Figures/Panel_2_Income_Barplot.png', width = 8)


#### SEXUAL ORIENTATION BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'sexual_orientation_w1', 
                  w2 = 'sexual_orientation_w2', 
                  label = 'Sexual_Orientation') %>% 
  ggplot(aes(x = Sexual_Orientation, y = n, fill = Wave)) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  ggtitle("Sexual Orientation Across Waves 1 and 2") +
  scale_x_discrete(labels = c("Lesbian, Gay, Bisexual or Something Else",
                              "Straight")) +
  scale_y_continuous(breaks = seq(0, 30000, 2000),
                     limits = c(0, 30000),
                     expand = c(0, 0))  +
    scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
    xlab(NULL) +
    my_theme_panel
ggsave('Figures/Panel_2_Sexual_Orientation_Barplot.png', width = 6)

#### SMOKING STATUS BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'smoking_status_w1', 
                  w2 = 'smoking_status_w2', 
                  label = 'Smoking_Status') %>% 
  ggplot(aes(x = Smoking_Status, y = n, fill = Wave)) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  scale_x_discrete(labels = c('Established Smokers', 
                              'Former Smokers', 
                              'Never Smokers')) +
  scale_y_continuous(expand = c(0, 0),
                    breaks=seq(0,12000,2000), 
                    limits = c(0, 12000)) +
  scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
  ggtitle("Smoking Status Across Waves") +
  xlab(NULL) +
  my_theme_panel
ggsave('Figures/Panel_2_Smoking_Status_Barplot.png', width = 6)


#### FULL SMOKING STATUS BARPLOT  (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'smoking_status_full_w1', 
                  w2 = 'smoking_status_full_w2', 
                  label = 'Smoking_Status') %>% 
  ggplot(aes(x = Smoking_Status, y = n, fill = Wave)) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  scale_x_discrete(labels = c('Current Established Smokers', 
                              'Former Established Smokers', 
                              'Current Experimental Smokers',
                              'Former Experimental Smokers', 
                              'Never Smokers')) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,12000,2000), 
                     limits = c(0, 12000)) +
  scale_fill_discrete(labels = c('Wave 1', 'Wave 2') ) +
  ggtitle("Smoking Status Across Waves") +
  xlab(NULL) +
  my_theme_panel


#### PSYCHOLOGICAL DISTRESS BARPLOT (PANEL) ####
adult_panel %>% 
  prep_panel_data(w1 = 'psychdist_w1', 
                  w2 = 'psychdist_w2', 
                  label = 'Psych_Dist') %>% 
  ggplot(aes(x = Wave, y = n, fill = as.factor(Psych_Dist))) +
  geom_col(color = 'black', 
           width = 0.4,
           position = position_dodge()) +
  scale_x_discrete(labels = c('Wave 1', 
                              'Wave 2')) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,24000,2000), 
                     limits = c(0, 24000)) +
  scale_fill_discrete(labels = c('Not Distressed', 'Distressed') ) +
  ggtitle("Psychological Distress* Across Waves 1 & 2") +
  xlab('*Felt Sad and/or Anxious in Past Month') +
  my_theme_panel +
  theme(axis.title.x = element_text(face = 'italic',size = 6))
ggsave('Figures/Panel_2_Psychological_Distress_Barplot.png')

#### POVERTY BARPLOT (W1) ####
adult_w1 %>% 
  drop_na(poverty_w1) %>% 
  group_by(poverty_w1) %>% 
  count() %>% 
  ggplot(aes(x = poverty_w1, y = n, fill = poverty_w1)) +
  geom_col(color = 'black', width = .3) +
  ggtitle("Poverty Status (Wave 1)") +
  scale_x_discrete(labels = c('Below Poverty Level',
                              'Above Poverty Level')) +
  scale_y_continuous(breaks = seq(0, 20000, 2000),
                     limits = c(0,20000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave
ggsave('Figures/W1_Poverty_Barplot.png', width = 4)

#### REGION BARPLOT (W1)  #####
adult_w1 %>% 
  drop_na(region_w1) %>% 
  group_by(region_w1) %>% 
  count() %>% 
  ggplot(aes(x = region_w1, y = n, fill = region_w1)) +
  geom_col(color = 'black', width = .75) +
  ggtitle("Census Region (Wave 1)") +
  scale_x_discrete(labels = c('North', 'Midwest', 'South', 'West')) +
  scale_y_continuous(breaks = seq(0, 14000, 2000),
                     limits = c(0, 14000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave
ggsave('Figures/W1_Region_Barplot.png')

#### Wave 1 Barplots ####

# SMOKING STATUS BARPLOT (W1) 
smoking_status_table  %>%  
  drop_na(smoking_status) %>% 
  ggplot(aes(x = smoking_status, y = n, fill= smoking_status)) +
  geom_col(color = 'black') +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,12000,2000), 
                     limits = c(0, 12000)) +
  scale_x_discrete(labels = c('Established Smokers', 'Former Smokers', 'Never Smokers')) +
  my_theme +
  xlab(NULL) +
  ggtitle('Smoking Status')

# GENDER BARPLOT (W1)
adult_w1 %>% 
  group_by(gender_w1) %>% 
  count() %>%  
  drop_na(gender_w1) %>% 
  ggplot(aes(x = gender_w1, y = n, fill= gender_w1)) +
    geom_col(color = 'black') +
    ggtitle('Gender') + 
    scale_fill_manual(values = c('blue', 'red')) +
    scale_x_discrete(labels = c('Male', 'Female')) +
    scale_y_continuous(breaks = seq(0, 18000, 2000),
                      limits = c(0, 20000),
                      expand = c(0, 0)) +
    xlab(NULL) +
    my_theme_wave

# AGE BARPLOT (W1)
adult_w1 %>% 
  drop_na(age_w1) %>% 
  ggplot(aes(x = age_w1, fill = age_w1)) +
  geom_bar(color = 'black') +
  ggtitle('Age (Wave 1)') + 
  scale_x_discrete(labels = c('18 to 34',
                              '35 to 64',
                              '65 and older')) +
  scale_y_continuous(breaks = seq(0, 16000, 2000),
                     limits = c(0, 16000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

# RACE BARPLOT (W1) 
adult_w1 %>% 
  drop_na(race) %>% 
  ggplot(aes(x = race_W1, fill = race_w1)) +
  geom_bar(color = 'black') +
  ggtitle("Race of Respondents (Wave 1)") +
  scale_y_continuous(breaks = seq(0, 24000, 2000),
                     limits = c(0, 24000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

# HISPANIC BARPLOT (W1) 
adult_w1 %>% 
  drop_na(hispanic_w1) %>% 
  ggplot(aes(x = hispanic_w1, fill = hispanic_w1)) +
  geom_bar(color = 'black') +
  ggtitle("Hispanic (Wave 1)") +
  scale_y_continuous(breaks = seq(0, 28000, 2000),
                     limits = c(0, 28000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

# EDUCATION BARPLOT (W1) 
adult_w1 %>% 
  drop_na(education_w1) %>% 
  ggplot(aes(x = education_w1, fill = education_w1)) +
  geom_bar(color = 'black') +
  ggtitle("Education (Wave 1)") +
  scale_x_discrete(labels = c("Less than High School", "High School", 
                              "Some College or AA", "Bachelor's or More")) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

# INCOME BARPLOT (W1) 
adult_w1 %>% 
  drop_na(income_w1) %>% 
  ggplot(aes(x = income_w1, fill = income_w1)) +
  geom_bar(color = 'black') +
  ggtitle("Income (Wave 1)") +
  scale_x_discrete(labels = c("Less than $25,000", "$25,000 to $49,999",
                              "$50,000 to $99,999", "More than $100,000")) +
  scale_y_continuous(breaks = seq(0, 14000, 2000),
                     limits = c(0, 14000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

# SEXUAL ORIENTATION BARPLOT (W1) 
adult_w1 %>% 
  drop_na(sexual_orientation_w1) %>% 
  ggplot(aes(x = sexual_orientation_w1, fill = sexual_orientation_w1)) +
  geom_bar(color = 'black ') +
  ggtitle("Sexual Orientation (Wave 1)") +
  scale_x_discrete(labels = c("Lesbian, Gay, Bisexual or Something Else",
                              "Straight")) +
  scale_y_continuous(breaks = seq(0, 30000, 2000),
                     limits = c(0,30000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme_wave

#### WAVE 2 Barplots  #####

#Smoking Status Barplot (W2)
adult_w2  %>%  
  drop_na(smoking_status_w2) %>% 
  ggplot(aes(x = smoking_status_w2, fill = smoking_status_w2)) +
  geom_bar(color = 'black') +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,10000,2000), 
                     limits = c(0, 10000)) +
  scale_x_discrete(labels = c('Established Smokers', 'Former Smokers', 'Never Smokers')) +
  my_theme +
  xlab(NULL) +
  ggtitle('Smoking Status (Wave 2)')


#GENDER BARPLOT (W2) 
adult_w2 %>% 
  drop_na(gender) %>% 
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar(color = 'black') +
  ggtitle('Gender (Wave 2') + 
  scale_fill_manual(values = c('blue', 'red')) +
  scale_x_discrete(labels = c('Male', 'Female')) +
  scale_y_continuous(breaks = seq(0, 16000, 2000),
                     limits = c(0, 16000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme

#AGE BARPLOT (W2) 
adult_w2 %>% 
  drop_na(age) %>% 
  ggplot(aes(x = age, fill = age)) +
  geom_bar(color = 'black') +
  ggtitle('Age (Wave 2)') + 
  scale_x_discrete(labels = c('18 to 34',
                              '35 to 64',
                              '65 and older')) +
  scale_y_continuous(breaks = seq(0, 16000, 2000),
                     limits = c(0, 16000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme

# RACE BARPLOT (W2)
adult_w2 %>% 
  drop_na(race) %>% 
  ggplot(aes(x = race, fill = race )) +
  geom_bar(color = 'black') +
  ggtitle("Race of Respondents (Wave 2)") +
  scale_y_continuous(breaks = seq(0, 22000, 2000),
                     limits = c(0, 22000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme


# HISPANIC BARPLOT (W2)
adult_w2 %>% 
  drop_na(hispanic) %>% 
  ggplot(aes(x = hispanic, fill = hispanic )) +
  geom_bar(color = 'black') +
  ggtitle("Hispanic (Wave 2)") +
  scale_y_continuous(breaks = seq(0, 24000, 2000),
                     limits = c(0, 24000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme

# EDUCATION BARPLOT (W2)
adult_w2 %>% 
  drop_na(education) %>% 
  ggplot(aes(x = education, fill = education )) +
  geom_bar(color = 'black') +
  ggtitle("Education (Wave 2)") +
  scale_x_discrete(labels = c("Less than High School", "High School", 
                              "Some College or AA", "Bachelor's or More")) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme



# INCOME BARPLOT (W2)
adult_w2 %>% 
  drop_na(income) %>% 
  ggplot(aes(x = income, fill = income)) +
  geom_bar(color = 'black') +
  ggtitle("Income (Wave 2)") +
  scale_x_discrete(labels = c("Less than $25,000", "$25,000 to $49,999",
                              "$50,000 to $99,999", "More than $100,000")) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme


# SEXUAL ORIENTATION BARPLOT (W2) 
adult_w2 %>% 
  drop_na(sexual_orientation) %>% 
  ggplot(aes(x = sexual_orientation, fill = sexual_orientation)) +
  geom_bar(color = 'black ') +
  ggtitle("Sexual Orientation (Wave 2)") +
  scale_x_discrete(labels = c("Lesbian, Gay, Bisexual or Something Else",
                              "Straight")) +
  scale_y_continuous(breaks = seq(0, 28000, 2000),
                     limits = c(0,28000),
                     expand = c(0, 0)) +
  xlab(NULL) +
  my_theme