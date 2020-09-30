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
pkgTest("caret")
pkgTest("MASS")
pkgTest("cvAUC")
pkgTest("broom")

library(tidyverse)
library(caret)
library(MASS)
library(cvAUC)
library(broom)

# Load Data

final_data <- readRDS("/hpc/group/sta440-f20/es321/final_data.Rds")

all_data <- final_data %>% 
  filter(label != 1) %>% 
  mutate(label = case_when(
    label ==  2 ~ 1,
    label == 3 ~ 0
  )) %>% 
  mutate(label = as.factor(label)) %>% 
  mutate(participant = fct_relevel(participant,  c('S10', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17')))

# Train Test

set.seed(20200922)
idx = createDataPartition(all_data$label, p = 0.8, list = FALSE)
train = all_data[idx, ]
test = all_data[-idx, ]
train_control <- trainControl(method="repeatedcv", number = 5, savePredictions = T) # this is the cross validation

# Metrics 
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
  
  test_F1 <- cm$byClass["F1"]
  
  test_AUC <-  cvAUC::AUC(pred %>% as.double(), testing_data$label %>% as.double())
  
  cv_df <- tibble(
    Accuracy = paste0(acc_mean, "% ± ", acc_sd, "%"),
    F1 = paste(f1_mean, "±", f1_sd),
    AUC = paste(AUC_mean, "±", AUC_sd)
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
goal_colnames <- c("Train Accuracy",
                   "Train F1 Score",
                   "Train AUC",
                   "Test Accuracy",
                   "Test F1 Score",
                   "Test AUC", 
                   "Model")
final_wrist_lr_model_df <- calc_metrics(lr_model_final_wrist, test, 5)  %>% 
  mutate(Model = "Wrist Only") 
colnames(final_wrist_lr_model_df) <- goal_colnames

# rename("Train Accuracy"= `Accuracy...1`,
#        "Train F1 Score" = `F1...2`,
#        "Train AUC"  = `AUC...3`,
#        "Test Accuracy" = `Accuracy...4`,
#        "Test F1 Score" = `F1...5`,
#        "Test AUC" = `AUC...6`)
final_wrist_chest_lr_model_df <- calc_metrics(lr_model_final_wrist_chest, test, 5) %>% 
  mutate(Model = "Chest & Wrist")
colnames(final_wrist_chest_lr_model_df) <- goal_colnames


print("These are the values for the 1st and 4th goals. These values can be found in Table 2, under section 5, Results, in the final report.")
print("The first row outputs metrics for the chest & wrist model, and the second row outputs metrics for the wrist model. These metrics were created by performing 5-fold Cross Validation on the training data, which was created using a 80/20 split.")
bind_rows(final_wrist_chest_lr_model_df, final_wrist_lr_model_df) %>% 
  dplyr::select(Model, "Train Accuracy", "Train F1 Score","Train AUC")

print("Similarly, the first row outputs metrics for the chest & wrist model, and the second row outputs metrics for the wrist model. These metrics were generated by predicting our model on the testing data.")
bind_rows(final_wrist_chest_lr_model_df, final_wrist_lr_model_df) %>% 
  dplyr::select(Model, "Test Accuracy", "Test F1 Score","Test AUC")

print("These tables quantify heterogeneity among the individuals (5th goal).")
lr_model_final_wrist_coef_tidy_df <- lr_model_final_wrist$finalModel %>% 
  tidy() %>%
  dplyr::select(-statistic)  

print("These are the coefficients from the wrist model. These are the log odds and odds for the participant predictor, and interaction term of participant and standard deviation of the magnitude of wrist ACC. Certain odds, such as sd_ACC_wrist_mag:S7, which has a value of 49185, are very large.")
lr_model_final_wrist_coef_tidy_df %>% 
  filter(term %in% lr_model_final_wrist_coef_tidy_df$term[str_detect(lr_model_final_wrist_coef_tidy_df$term, "participant")] ) %>% 
  mutate(`Log Odds` = estimate,
         Odds = exp(`Log Odds`)) %>% 
  dplyr::select(term, `Log Odds`, Odds) %>% 
  mutate(term = str_remove_all(term, "participant"),
         term = str_remove_all(term, "`")) %>% 
  print(as_tibble(), n = 100)

lr_model_final_wrist_chest_coef_tidy_df <- lr_model_final_wrist_chest$finalModel %>% 
  tidy() %>%
  dplyr::select(-statistic)  

print("These are the coefficients from the chest & wrist model. These are the log odds and odds for the participant predictor, the interaction term of participant and standard deviation of chest HR, and the interaction term of participant and root mean squared of wrist HRV. Certain odds, such as sd_heart_rate_chest_ECG:S11, which has a value of 529302, are very large.")

lr_model_final_wrist_chest_coef_tidy_df %>% 
  filter(term %in% lr_model_final_wrist_chest_coef_tidy_df$term[str_detect(lr_model_final_wrist_chest_coef_tidy_df$term, "participant")] ) %>% 
  mutate(`Log Odds` = estimate,
         Odds = exp(`Log Odds`)) %>% 
  dplyr::select(term, `Log Odds`, Odds) %>% 
  mutate(term = str_remove_all(term, "participant"),
         term = str_remove_all(term, "`")) %>% 
  print(as_tibble(), n = 100)





