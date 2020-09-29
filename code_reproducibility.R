# This R script includes all necessary code reproducibility to be done for STA 440 Case Study 2 (Detecting Stress)
# This R script contains information on:
#               1) classification accuracy for the first goal
#               2) classification accuracy for the fourth goal
#               3) quantification of heterogeneity across individuals for the fifth goal

#### SET UP ####
# Validate that all necessary packaged have been downloaded, install otherwise or throw err package DNE
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,repos = "http://cran.r-project.org", dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Installing packages 
pkgTest("tidyverse")
pkgTest("knitr")
pkgTest("kableExtra")
pkgTest("caret")
pkgTest("randomForest")
pkgTest("MASS")
pkgTest("cvAUC")
pkgTest("car")
pkgTest("broom")
pkgTest("lme4")
pkgTest("cAIC4")
pkgTest("boot")
pkgTest("cvms")
pkgTest("grid")
pkgTest("gridExtra")

library(tidyverse)
library(knitr)
library(kableExtra)
library(caret)
library(randomForest)
library(MASS)
library(cvAUC)
library(car)
library(broom)
library(lme4)
library(cAIC4)
library(boot)
library(cvms)
library(grid)
library(gridExtra)

# Load Data

survey <- read_csv("survey.csv")
final_data <- readRDS("final_data.Rds")
sub_data <- readRDS("subj_df.Rds")
survey <- survey %>%
  rename(label = Label)
sub_data <- sub_data %>%
  rename(participant = Participant) %>% 
  mutate(`Dominant hand` = as.factor(`Dominant hand`),
         Gender = as.factor(Gender),
         `Height (cm)` = as.double(`Height (cm)`),
         `Weight (kg)` = as.double(`Weight (kg)`))
all_data <- final_data %>% 
  left_join(survey, by=c("participant", "label")) %>% 
  left_join(sub_data, by=c("participant")) %>% 
  filter(label != 1) %>% 
  mutate(label = case_when(
    label ==  2 ~ 1,
    label == 3 ~ 0
  )) %>% 
  mutate(label = as.factor(label)) %>% 
  mutate(participant = fct_relevel(participant,  c('S10', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17')))

# Train Test

round_perc = function(x) {
  if (is.double(x)) {
    round(x*100, digits = 2) 
  }
}
calc_metrics = function(model, testing_data, folds) {
  list = list()
  acc_list = c()
  f1_list = c()
  AUC_list = c()
  
  # metrics for cross validated data
  for (i in 1:folds) {
    fold <- model$pred %>% pull(Resample) %>% unique() %>% .[i]
    
    fold_df <- model$pred %>% 
      filter(Resample == fold)
    
    # accuracy
    acc <- mean(fold_df$pred == fold_df$obs)
    acc_list[i] <- acc
    
    # F1 score
    # precision <- posPredValue(fold_df$pred, fold_df$obs, positive="1")
    # recall <- sensitivity(fold_df$pred, fold_df$obs, positive="1")
    # F1 <- (2 * precision * recall) / (precision + recall)
    f1_list[i] <- confusionMatrix(fold_df$pred, fold_df$obs)$byClass["F1"]
    
    # AUC
    AUC_list[i] <- cvAUC::AUC(fold_df$pred %>% as.double(), fold_df$obs %>% as.double())
  }
  
  acc_mean = acc_list %>% mean(na.rm=T) %>% round_perc()
  acc_sd = acc_list %>% sd(na.rm=T) %>% round_perc()
  f1_mean = f1_list %>% mean(na.rm=T)  %>% round_perc()
  f1_sd = f1_list %>% sd(na.rm=T)  %>% round_perc()
  AUC_mean = AUC_list %>% mean(na.rm=T)  %>% round_perc()
  AUC_sd = AUC_list %>% sd(na.rm=T)  %>% round_perc()
  
  # metrics for testing data
  pred <- predict(model, newdata = testing_data)
  test_acc <- mean(pred == testing_data %>% pull(label))
  
  cm <- confusionMatrix(as.factor(pred), testing_data$label)
  
  # precision <- posPredValue(pred, testing_data$label, positive="1")
  # recall <- sensitivity(pred, testing_data$label, positive="1")
  test_F1 <- cm$byClass["F1"]
  
  test_AUC <-  AUC(pred %>% as.double(), testing_data$label %>% as.double())
  
  cv_df <- tibble(
    Accuracy = paste0(acc_mean, "% ± \n", acc_sd, "%"),
    F1 = paste(f1_mean, "± \n", f1_sd),
    AUC = paste(AUC_mean, "± \n", AUC_sd)
  )
  
  test_df <- tibble(
    Accuracy = paste0(test_acc %>% round_perc(), "%"), 
    F1 = test_F1 %>% round_perc(),
    AUC = test_AUC %>% round_perc()
  )
  return(bind_cols(cv_df, test_df))
}

lr_model_final_wrist <- train(label ~  sd_EDA_wrist + sd_ACC_wrist_mag + sd_heart_rate_variability_wrist_BVP + 
                                sd_EDA_wrist*sd_ACC_wrist_mag + sd_EDA_wrist*sd_heart_rate_variability_wrist_BVP + 
                                sd_ACC_wrist_mag*sd_heart_rate_variability_wrist_BVP + 
                                participant + participant:sd_ACC_wrist_mag,
                              data = train, trControl = train_control, 
                              method = "glm",
                              family ="binomial") 

lr_model_final_wrist_chest <- train(label ~ sd_EDA + sd_EDA_wrist + sd_Temp_chest + 
                                      min_Temp_wrist + min_EMG + sd_inhale + sd_exhale + max_breath + 
                                      i_e_ratio + mean_ACC_chest_mag + sd_ACC_chest_mag + sd_ACC_wrist_mag + 
                                      sd_heart_rate_wrist_BVP + sd_heart_rate_variability_wrist_BVP + rms_heart_rate_variability_wrist_BVP + 
                                      sd_heart_rate_chest_ECG + rms_heart_rate_variability_chest_ECG  + 
                                      sd_EDA_wrist * min_Temp_wrist + participant +
                                      rms_heart_rate_variability_wrist_BVP*participant + sd_heart_rate_chest_ECG*participant, # try different interactions 
                                    data = train, trControl = train_control, 
                                    method = "glm",
                                    family ="binomial")

lr_model_final_wrist_chest