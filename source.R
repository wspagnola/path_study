require(tidyverse)
require(ggplot2)
require(survey)

#### FUNCTIONS ####

recode_binary <- function(x) {
  
  #Recode Yes/No as 1/0
  recode(x, '(1) 1 = Yes' = 1, '(2) 2 = No' = 0 )
}


recode_multi_choice <- function(x) {
  
  #Recode Marked/Not Marked as 1/0 for multiple choice questions
  recode(x, '(1) 1 = Marked' = 1, '(2) 2 = Not Marked' = 0 )
}

prep_panel_data <- function(data, w1, w2, label){
  require(tidyr)
  require(dplyr)
  wave_1 <- data[, w1]
  wave_2 <- data[, w2]
  d <- data.frame(wave_1, wave_2) 
  d_table <- d %>%
    select(wave_1, wave_2) %>%
    gather(key = Wave, value = val) %>%
    group_by(Wave, val) %>%
    count() %>%
    filter(!is.na(val))
  names(d_table)[2] <- label
  return(d_table)
}


#### THEMES ####

#Theme for Multiple Waves
my_theme_panel <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

#Theme for Singl Waves
my_theme_wave <-  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) 


#Hex Codes for Default ggplot colors
gg_blue <- '#619CFF'
gg_red <- '#F8766D'

