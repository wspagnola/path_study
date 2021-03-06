

library(tidyverse)
library(ggplot2)
library(survey)
library(nnet) #For multinomial logistic regression (unweighted)

#### FUNCTIONS ####

recode_binary <- function(x){
    abs(x -2) 
}



recode_multi_choice <- function(x) {
  
  #Recode Marked/Not Marked as 1/0 for multiple choice questions
  recode(x, '(1) 1 = Marked' = 1, '(2) 2 = Not Marked' = 0 )
}

prep_panel_data <- function(data, w1, w2, label){
  
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



#### THEMES ####

#Theme for Multiple Waves
my_theme_panel <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) 

#Theme for Single Waves
my_theme_wave <-  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
) 

#Theme For Quit Plots
theme_wave_quit <-  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = 'italic', 
                                    size = 8, 
                                    vjust = 0.2,
                                    hjust = 0)
)


#Theme For 'Mosaic' Transition Plots
theme_wave_mosaic <-  theme_bw() +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 8, face = "italic"),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 8, face = "italic", hjust = 0))

#Hex Codes for Default ggplot colors
gg_blue <- '#619CFF'
gg_red <- '#F8766D'




plot_sankey <- function( df, w1, w2, w3, sep = '_', useNA = T) {
  require(tidyr)
  require(dplyr)
  
  
  
  
  #Inputs data frame and 1 grouping variable for each of the 3 waves
  w1 <- enquo(w1)
  w2 <- enquo(w2)
  w3 <- enquo(w3)
  
  if(useNA == F){
    df <- df %>% 
      drop_na(!!w1, !!w2, !!w3)
    
  }
  
  #Create Data.frame of step 1 transitions(wave 1 to wave 2) 
  step_1 <- df %>%
    count(!!w1, !!w2 )%>%
    rename(before = !!w1,
           after = !!w2 ,
           flow = n) %>%
    ungroup %>% 
    mutate(step_from = 'wave 1',
           step_to = 'wave 2',
           before =  paste('w1', before, sep = sep),
           after = paste('w2', after, sep = sep))
  
  #Create Data.frame of step 1 transitions(wave 1 to wave 2)
  step_2 <- df %>%
    count(!!w2, !!w3) %>%
    rename(before = !!w2,
           after =!!w3,
           flow = n) %>%
    ungroup %>% 
    mutate(step_from = 'wave 2',
           step_to = 'wave 3',
           before =  paste('w2', before, sep = sep),
           after = paste('w3', after, sep = sep))
  
  #Bind rows of step 1 and step 2 data.frames
  sankey_df <- rbind(step_1, step_2)
  return( sankey_df)
  
  
}



#### Old Code ####

# This package may have been removed from CRAN
#library(srvyr) #Can use dplyr function with survey objects


# recode_binary <- function(x) {
#   
#   #Recode Yes/No as 1/0
#   recode(x, '(1) 1 = Yes' = 1, '(2) 2 = No' = 0 )
# }
