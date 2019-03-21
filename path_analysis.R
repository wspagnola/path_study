#Regression 

install.packages("devtools")
library(devtools)
install_github("carlganz/svrepmisc")
library(svrepmisc)
svymultinom(cig_use_ever_w1 ~ income_w1, design = survey_w1)

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



mod4 <- multinom(factor(smoking_status_full_w3) ~ race_ethnicity_w3 +
                   gender_w3 + sexual_orientation_w3 +
                   education_w3 + poverty_w1 + region_w1 + 
                   age_w1 + psychdist_w1 + factor(smoking_status_full_w1),
                 data = adult_panel)
summary(mod4)
0.9633552 *1.112336^2
1.0045955* 1.108123^(-2)
1.2743052*1.119375^(-2)
cur_est_smokers_w1$smoking_status_full_w3 %>%  as.factor %>%  levels
levels(adult_w3$ education_w3)

factor(cur_est_smokers_w1$smoking_status_full_w3)
multinom(q)