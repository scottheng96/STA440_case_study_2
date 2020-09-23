library(tidyverse)
library(zoo)

chest_wrist <- readRDS("chest_and_wrist.Rds")

chest_wrist <- chest_wrist %>%
  mutate(inhale = case_when(Resp > 0 ~ Resp,
                            TRUE ~ NA_real_),
         exhale = case_when(Resp < 0 ~ Resp,
                            TRUE ~ NA_real_))

EDA_chest_list = list()
EDA_wrist_list = list()
EMG_list = list()
Temp_chest_list = list()
Temp_wrist_list = list()
inhale_list = list()
exhale_list = list()
resp_list = list()
breath_list = list()

for (i in 1:(chest_wrist$Participant %>% unique() %>% length())) {
  p <- chest_wrist$Participant %>% unique() %>% .[i]
  subject_p <- chest_wrist %>% filter(Participant == p)
  
  subject_p$cycle <- cumsum(c(-1,diff(sign(subject_p$Resp))) > 0)
  subject_p <- subject_p %>%
    mutate(resp_cycle = case_when(is.na(cycle) ~ 0.0,
                                  TRUE ~ as.numeric(cycle)))
  cycle_int = subject_p %>%
    group_by(resp_cycle) %>%
    count()
  
  subject_p <- full_join(subject_p, cycle_int)
  subject_p <- subject_p %>%
    mutate(breath_rate = n/4)
  
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

    mean_Temp_chest = mean_Temp_chest, 
    sd_Temp_chest = sd_Temp_chest,
    min_Temp_chest = min_Temp_chest,
    max_Temp_chest = max_Temp_chest,
    range_Temp_chest = max_Temp_chest - min_Temp_chest
  )
  
  # inhale
  mean_inhale <- rollapply(subject_p$inhale, 240, mean, na.rm = TRUE)
  sd_inhale <- rollapply(subject_p$inhale, 240, sd, na.rm = TRUE)
  
  inhale_df <- tibble(
    participant = p,
    label = label,
 
    mean_inhale = mean_inhale, 
    sd_inhale = sd_inhale
  )
  
  # exhale
  mean_exhale <- rollapply(subject_p$exhale, 240, mean, na.rm = TRUE)
  sd_exhale <- rollapply(subject_p$exhale, 240, sd, na.rm = TRUE)
  
  exhale_df <- tibble(
    participant = p,
    label = label,
    # chest EDA
    mean_exhale = mean_exhale, 
    sd_exhale = sd_exhale
  )
  
  # resp
  min_resp <- rollapply(subject_p$exhale, 240, min, na.rm = TRUE)
  max_resp <- rollapply(subject_p$exhale, 240, max, na.rm = TRUE)
  
  resp_df <- tibble(
    participant = p,
    label = label,

    min_resp = min_resp,
    max_resp = max_resp,
    range_resp = max_resp - min_resp
  )
  
  # breath rate
  mean_breath <- rollapply(subject_p$breath_rate, 240, mean, na.rm = TRUE)
  sd_breath <- rollapply(subject_p$breath_rate, 240, sd, na.rm = TRUE)
  max_breath <- rollapply(subject_p$breath_rate, 240, max, na.rm = TRUE)
  min_breath <- rollapply(subject_p$breath_rate, 240, min, na.rm = TRUE)
  
  breath_df <- tibble(
    participant = p,
    label = label,
    
    mean_breath = mean_breath, 
    sd_breath = sd_breath,
    min_breath = min_breath,
    max_breath = max_breath,
    range_breath = max_breath - min_breath
  )
  
  # adding dataframes to list
  EDA_chest_list[[i]] <- EDA_chest_df
  EDA_wrist_list[[i]] <- EDA_wrist_df
  EMG_list[[i]] <- EMG_df
  Temp_chest_list[[i]] <- Temp_chest_df
  Temp_wrist_list[[i]] <- Temp_wrist_df
  inhale_list[[i]] <- inhale_df
  exhale_list[[i]] <- exhale_df
  resp_list[[i]] <- resp_df
  breath_list[[i]] <- breath_df
}

# bind rows for all dataframes (each dataframe is a subject)
EDA_chest_eng <- do.call(bind_rows, EDA_chest_list)
EDA_wrist_eng <- do.call(bind_rows, EDA_wrist_list)
EMG_eng <- do.call(bind_rows, EMG_list)
Temp_chest_eng <- do.call(bind_rows, Temp_chest_list)
Temp_wrist_eng <- do.call(bind_rows, Temp_wrist_list)
inhale_eng <- do.call(bind_rows, inhale_list)
exhale_eng <- do.call(bind_rows, exhale_list)
resp_eng <- do.call(bind_rows, resp_list)
breath_eng <- do.call(bind_rows, breath_list)

# add all of our engineered values/dataframes into here 
final_data <- bind_cols(EDA_chest_eng, 
                        EDA_wrist_eng %>% select(-participant, -label), 
                        EMG_eng %>% select(-participant, -label), 
                        Temp_chest_eng %>% select(-participant, -label),
                        Temp_wrist_eng %>% select(-participant, -label),
                        inhale_eng %>% select(-participant, -label),
                        exhale_eng %>% select(-participant, -label),
                        resp_eng %>% select(-participant, -label),
                        breath_eng %>% select(-participant, -label))

final_data <- final_data %>%
  mutate(i_e_ratio = abs(mean_inhale/mean_exhale))
#saveRDS(final_data, file = "final_data.Rds")
 