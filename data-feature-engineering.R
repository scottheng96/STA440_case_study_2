library(tidyverse)
library(zoo)

chest_wrist <- readRDS("chest_and_wrist.Rds")

EDA_chest_list = list()
EDA_wrist_list = list()
EMG_list = list()
Temp_chest_list = list()
Temp_wrist_list = list()

for (i in 1:(chest_wrist$Participant %>% unique() %>% length())) {
  p <- chest_wrist$Participant %>% unique() %>% .[i]
  subject_p <- chest_wrist %>% filter(Participant == p)
  
  label <- rollapply(subject_p$Label, 240, first)
  # chest EDA
  mean_EDA <- rollapply(subject_p$EDA, 240, mean)
  sd_EDA <- rollapply(subject_p$EDA, 240, sd)
  max_EDA <- rollapply(subject_p$EDA, 240, max)
  min_EDA <- rollapply(subject_p$EDA, 240, min)
  
  EDA_chest_df <- tibble(
    participant = p,
    label = label,
    # chest EDA
    mean_EDA = mean_EDA, 
    sd_EDA = sd_EDA,
    min_EDA = min_EDA,
    max_EDA = max_EDA,
    range_EDA = max_EDA - min_EDA
  )
  # wrist EDA
  mean_EDA_wrist <- rollapply(subject_p$EDA_wrist, 240, mean)
  sd_EDA_wrist <- rollapply(subject_p$EDA_wrist, 240, sd)
  max_EDA_wrist <- rollapply(subject_p$EDA_wrist, 240, max)
  min_EDA_wrist <- rollapply(subject_p$EDA_wrist, 240, min)
  
  EDA_wrist_df <- tibble(
    participant = p,
    label = label,
    # wrist EDA
    mean_EDA_wrist = mean_EDA_wrist,
    sd_EDA_wrist = sd_EDA_wrist,
    min_EDA_wrist = min_EDA_wrist,
    max_EDA_wrist = max_EDA_wrist,
    range_EDA_wrist = max_EDA_wrist - min_EDA_wrist
  )
  
  # chest EMG
  mean_EMG <- rollapply(subject_p$EMG, 240, mean)
  sd_EMG <- rollapply(subject_p$EMG, 240, sd)
  max_EMG <- rollapply(subject_p$EMG, 240, max)
  min_EMG <- rollapply(subject_p$EMG, 240, min)
  
  EMG_df <- tibble(
    participant = p,
    label = label,
    mean_EMG = mean_EMG, 
    sd_EMG = sd_EMG,
    range_EMG = max_EMG - min_EMG
  )
  
  # wrist Temp
  mean_Temp_wrist <- rollapply(subject_p$TEMP_wrist, 240, mean)
  sd_Temp_wrist <- rollapply(subject_p$TEMP_wrist, 240, sd)
  max_Temp_wrist <- rollapply(subject_p$TEMP_wrist, 240, max)
  min_Temp_wrist <- rollapply(subject_p$TEMP_wrist, 240, min)
  
  Temp_wrist_df <- tibble(
    participant = p,
    label = label,
    # chest EDA
    mean_Temp_wrist = mean_Temp_wrist, 
    sd_Temp_wrist = sd_Temp_wrist,
    min_Temp_wrist = min_Temp_wrist,
    max_Temp_wrist = max_Temp_wrist,
    range_Temp_wrist = max_Temp_wrist - min_Temp_wrist
  )
  
  # chest Temp
  mean_Temp_chest <- rollapply(subject_p$Temp, 240, mean)
  sd_Temp_chest <- rollapply(subject_p$Temp, 240, sd)
  max_Temp_chest <- rollapply(subject_p$Temp, 240, max)
  min_Temp_chest <- rollapply(subject_p$Temp, 240, min)
  
  Temp_chest_df <- tibble(
    participant = p,
    label = label,
    # chest EDA
    mean_Temp_chest = mean_Temp_chest, 
    sd_Temp_chest = sd_Temp_chest,
    min_Temp_chest = min_Temp_chest,
    max_Temp_chest = max_Temp_chest,
    range_Temp_chest = max_Temp_chest - min_Temp_chest
  )
  
  # adding dataframes to list
  EDA_chest_list[[i]] <- EDA_chest_df
  EDA_wrist_list[[i]] <- EDA_wrist_df
  EMG_list[[i]] <- EMG_df
  Temp_chest_list[[i]] <- Temp_chest_df
  Temp_wrist_list[[i]] <- Temp_wrist_df
}

# bind rows for all dataframes (each dataframe is a subject)
EDA_chest_eng <- do.call(bind_rows, EDA_chest_list)
EDA_wrist_eng <- do.call(bind_rows, EDA_wrist_list)
EMG_eng <- do.call(bind_rows, EMG_list)
Temp_chest_eng <- do.call(bind_rows, Temp_chest_list)
Temp_wrist_eng <- do.call(bind_rows, Temp_wrist_list)

# add all of our engineered values/dataframes into here 
final_data <- bind_cols(EDA_chest_eng, 
                        EDA_wrist_eng %>% select(-participant, -label), 
                        EMG_eng %>% select(-participant, -label), 
                        Temp_chest_eng %>% select(-participant, -label),
                        Temp_wrist_eng %>% select(-participant, -label))

#saveRDS(final_data, file = "final_data.Rds")
 