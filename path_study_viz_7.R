require(tidyverse)
#Wave 1-3: Descriptives

#Panel 3 Barplots
#panel 3 = across waves 1, 2, 3

#Hex Codes for Default ggplot colors
gg_blue <- '#619CFF'
gg_red <- '#F8766D'

#Theme For Single Wave Plots
theme_wave_quit <-  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = 'italic', 
                                    size = 8, 
                                    vjust = 0.2,
                                    hjust = 0)
)



prep_panel_3_data <- function(data, x, label){
  
  #Data is the data.frame where vectors are located
  #W1, W2, W3 are character objects representing the column names of the vectors
  #Label is the Grand Vector; general the name of the variable without wave tag at end
  
  require(tidyr)
  require(dplyr)
  
  #Store data from 3 Waves into separate vectors
  wave_1 <- data[, x[1]]
  wave_2 <- data[, x[2]]
  wave_3 <- data[, x[3]]
  
  #Store three vectors into single data.frame
  d <- data.frame(wave_1, wave_2, wave_3) 
  
  #Convert from wide to Long; Then Create a summary table of
  d_table <- d %>%
    gather(key = Wave, value = val) %>%
    group_by(Wave, val) %>%
    count()
  
  #Remove Missing Values
  d_table <-  d_table %>% filter(!is.na(val))
  
  #Replace 'val'  with  Label Name 
  names(d_table)[2] <- label
  
  return(d_table)
}


#### DEFINE THEMES ####
my_theme_panel <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

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
  prep_panel_3_data(x = c('smoking_status_w1', 
                          'smoking_status_w2', 
                          'smoking_status_w3'), 
                    label = 'Smoking_Status') %>% 
  ggplot(aes(x = Wave, y = n, fill = factor(Smoking_Status))) +
  geom_col(position = position_dodge(),  color = 'black', width = .5) +
  scale_fill_discrete(labels = c('Current Established Smokers',
                                 'Current Non-Established Smokers',
                                 'Former Established Smokers', 
                                 'Never Smokers')) +
  scale_x_discrete(labels = panel_3_wave_labs) +
  scale_y_continuous(breaks = seq(0, 12000, 2000),
                     limits = c(0, 12000),
                     expand = c(0, 0)) +
  my_theme_panel +
  ggtitle('Smoking Status Across Waves 1 to 3') +
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
  mutate(days_quit_cigs_w2 = case_when(
            as.numeric(R02_AC1009_UN)==1 & R02_AC1009_NN>=0 ~ R02_AC1009_NN,
            as.numeric(R02_AC1009_UN)==2 & R02_AC1009_NN>=0 ~ R02_AC1009_NN * 30.4375,
            as.numeric(R02_AC1009_UN)==3 & R02_AC1009_NN>=0 ~  R02_AC1009_NN * 365.25), 
        abst_w2 = case_when(
              days_quit_cigs_w2 == 1 ~ 'One Day',
              days_quit_cigs_w2 >= 2 &  days_quit_cigs_w2 <= 6 ~ 'Two to Six Days',
              days_quit_cigs_w2 >= 7 &  days_quit_cigs_w2 < 30 ~ 'More than 7 Days',
              days_quit_cigs_w2 >= 30 &  days_quit_cigs_w2 <= 90 ~ 'One Month',
              days_quit_cigs_w2 > 90 &  days_quit_cigs_w2 <= 180 ~ 'Three Months',
              days_quit_cigs_w2 > 180 &  days_quit_cigs_w2 < 365 ~ 'Six Months',
              days_quit_cigs_w2 >= 365 ~ 'One Year'),
         abst_w2 = factor(abst_w2, levels = c('One Day', 'Two to Six Days',  
                                              'More than 7 Days', 'One Month',  
                                              'Three Months', 'Six Months','One Year'))) %>% 
      group_by(abst_w2) %>% 
      count
   
#I am only getting 477 Smokers who have quit for more than a year 
adult_panel %>%  filter(R02_AC1009_NN >= 365 & as.numeric(R02_AC1009_UN) ==1) %>%  nrow
adult_panel %>%  filter(R02_AC1009_NN >= 12 & as.numeric(R02_AC1009_UN) ==2) %>%  nrow

#Waves 3 Abstinence Length
adult_panel %>% 
  mutate(days_quit_cigs_w3 = case_when(
    as.numeric(R03_AC1009_UN)==1 & R03_AC1009_NN>=0 ~ R03_AC1009_NN,
    as.numeric(R03_AC1009_UN)==2 & R03_AC1009_NN>=0 ~ R03_AC1009_NN * 30.4375,
    as.numeric(R03_AC1009_UN)==3 & R03_AC1009_NN>=0 ~  R03_AC1009_NN * 365.25), 
    abs_w3 = case_when(
      days_quit_cigs_w3 == 1 ~ 'One Day',
      days_quit_cigs_w3 >= 2 &  days_quit_cigs_w3 <= 6 ~ 'Two to Six Days',
      days_quit_cigs_w3 >= 7 &  days_quit_cigs_w3 < 30 ~ 'More than 7 Days',
      days_quit_cigs_w3 >= 30 &  days_quit_cigs_w3 <= 90 ~ 'One Month',
      days_quit_cigs_w3 > 90 &  days_quit_cigs_w3 <= 180 ~ 'Three Months',
      days_quit_cigs_w3 > 180 &  days_quit_cigs_w3 < 365 ~ 'Six Months',
      days_quit_cigs_w3 >= 365 ~ 'One Year'),
    abs_w3 = factor(abs_w3, levels = c('One Day', 'Two to Six Days',  
                                         'More than 7 Days', 'One Month',  
                                         'Three Months', 'Six Months','One Year'))) %>% 
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

