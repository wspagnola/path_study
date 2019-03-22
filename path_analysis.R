#Regression 

cur_est_smokers_w1 <- adult_panel %>% 
  filter(current_est_smoker_w1==1)

mod2 <- glm(factor(smoking_status_full_w3) ~ race_ethnicity_w3 +
              gender_w3 + sexual_orientation_w3 +
              education_w3 + poverty_w1 + region_w1 + 
              age_w1 + psychdist_w1,
            data = cur_est_smokers_w1,
            family =binomial)
summary(mod2)
table_star <- cbind(summary(mod2)$coefficients[, 1] %>%  exp, 
                    summary(mod2)$coefficients[, 2] %>%  exp, 
                    summary(mod2)$coefficients[, 3:4]) 

table_star <- as.data.frame(table_star)
table_star$Sig <- as.character(ifelse(summary(mod2)$coefficients[, 4] < .05, '*', '0'))
names(table_star)[1:2] <- c('OR', 'Std_Err')
write.csv(table_star, 
          'Logistic_Regression_Smoking_Status_W3_Among_W1_Cur_Est_Smokers.csv')
#### Multinomial ####
mod4 <- multinom(factor(smoking_status_full_w3) ~ race_ethnicity_w3 +
                   gender_w3 + sexual_orientation_w3 +
                   education_w3 + poverty_w1 + region_w1 + 
                   age_w1 + psychdist_w1 + factor(smoking_status_full_w1),
                 data = adult_panel)
summary(mod4)

cur_est_smokers_w1$smoking_status_full_w3 %>%  as.factor %>%  levels
levels(adult_w3$ education_w3)

factor(cur_est_smokers_w1$smoking_status_full_w3)

multinom(q)