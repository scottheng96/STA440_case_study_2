library(tidyverse)
library(zoo)
library(peakPick)

chest_wrist <- readRDS("chest_and_wrist.Rds")

chest_wrist <- chest_wrist %>%
  mutate(inhale = case_when(Resp > 0 ~ Resp,
                            TRUE ~ NA_real_),
         exhale = case_when(Resp < 0 ~ Resp,
                            TRUE ~ NA_real_),
         ACC_chest_mag = sqrt(ACC_chest_X**2 + ACC_chest_Y**2 + ACC_chest_Z**2),
         ACC_wrist_mag = sqrt(ACC_wrist_X**2 + ACC_wrist_Y**2 + ACC_wrist_Z**2)
         )

EDA_chest_list = list()
EDA_wrist_list = list()
EMG_list = list()
Temp_chest_list = list()
Temp_wrist_list = list()
inhale_list = list()
exhale_list = list()
resp_list = list()
breath_list = list()
ACC_chest_list = list()
ACC_wrist_list = list()
BVP_wrist_list = list()
ECG_chest_list = list()

#calculate heart rate variability 
hrv_mean <- function(x) {
  index = which(x==1)
  d = diff(index)
  return(mean(d))
}

hrv_sd <- function(x) {
  index = c(which(x==1))
  d = diff(index)
  return(sd(d))
}

hrv_rms <- function(x) {
  index = which(x==1)
  d = diff(index)
  return(sqrt(sum(d^2)))
}


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
  
  #labels
  label <- rollapply(subject_p$Label, 240, first)
  label_5hz <- rollapply(subject_p$Label, 20, first)
  
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
  
  # wrist BVP
  heart_peak <- peakpick(subject_p$BVP_wrist, 1,  deriv.lim = 4, peak.min.sd = 0.5)
  heart_peak1 <- ifelse(heart_peak == TRUE, 1, 0)
  mean_heart_rate_wrist_BVP <- rollapply(heart_peak1, 240, sum)
  sd_heart_rate_wrist_BVP <- rollapply(heart_peak1, 240, sd)
  mean_heart_rate_variability_wrist_BVP <- rollapply(heart_peak1,240, hrv_mean)
  sd_heart_rate_variability_wrist_BVP <- rollapply(heart_peak1,240,hrv_sd)
  rms_heart_rate_variability_wrist_BVP <- rollapply(heart_peak1,240, hrv_rms)
  
  BVP_wrist_df <- tibble(
    participant = p,
    label = label,
    mean_heart_rate_wrist_BVP = mean_heart_rate_wrist_BVP,
    sd_heart_rate_wrist_BVP = sd_heart_rate_wrist_BVP,
    mean_heart_rate_variability_wrist_BVP = mean_heart_rate_variability_wrist_BVP,
    sd_heart_rate_variability_wrist_BVP = sd_heart_rate_variability_wrist_BVP,
    rms_heart_rate_variability_wrist_BVP = rms_heart_rate_variability_wrist_BVP
  )
  
  # chest ECG
  heart_peaks <- peakpick(subject_p$ECG, 1)
  heart_peaks1 <- ifelse(heart_peaks == TRUE, 1, 0)
  mean_heart_rate_chest_ECG <- rollapply(heart_peaks1, 240, sum)
  sd_heart_rate_chest_ECG <- rollapply(heart_peaks1, 240, sd)
  mean_heart_rate_variability_chest_ECG <- rollapply(heart_peaks1,240, hrv_mean)
  sd_heart_rate_variability_chest_ECG <- rollapply(heart_peaks1,240,hrv_sd)
  rms_heart_rate_variability_chest_ECG <- rollapply(heart_peaks1,240, hrv_rms)
  
  
  ECG_chest_df <- tibble(
    participant = p,
    label = label,
    mean_heart_rate_chest_ECG = mean_heart_rate_chest_ECG,
    sd_heart_rate_chest_ECG = sd_heart_rate_chest_ECG,
    mean_heart_rate_variability_chest_ECG = mean_heart_rate_variability_chest_ECG,
    sd_heart_rate_variability_chest_ECG = sd_heart_rate_variability_chest_ECG,
    rms_heart_rate_variability_chest_ECG = rms_heart_rate_variability_chest_ECG
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
  BVP_wrist_list[[i]] <- BVP_wrist_df
  ECG_chest_list[[i]] <- ECG_chest_df
}

for (i in 1:(chest_wrist$Participant %>% unique() %>% length())) {
  p <- chest_wrist$Participant %>% unique() %>% .[i]
  subject_p <- chest_wrist %>% filter(Participant == p)
  label_5hz <- rollapply(subject_p$Label, 20, first)
  
  # chest ACC
  mean_chest_ACC_X <- rollapply(subject_p$ACC_chest_X, 20, mean)
  mean_chest_ACC_Y <- rollapply(subject_p$ACC_chest_Y, 20, mean)
  mean_chest_ACC_Z <- rollapply(subject_p$ACC_chest_Z, 20, mean)
  mean_chest_ACC_sum <-  mean_chest_ACC_X + mean_chest_ACC_Y + mean_chest_ACC_Z
  
  sd_chest_ACC_X <- rollapply(subject_p$ACC_chest_X, 20, sd)
  sd_chest_ACC_Y <- rollapply(subject_p$ACC_chest_Y, 20, sd)
  sd_chest_ACC_Z <- rollapply(subject_p$ACC_chest_Z, 20, sd)
  sd_chest_ACC_sum <- sd_chest_ACC_X + sd_chest_ACC_Y + sd_chest_ACC_Z
  
  peak_chest_ACC_X <- rollapply(subject_p$ACC_chest_X, 20, max)
  peak_chest_ACC_Y <- rollapply(subject_p$ACC_chest_Y, 20, max)
  peak_chest_ACC_Z <- rollapply(subject_p$ACC_chest_Z, 20, max)
  
  # chest ACC magnitude
  mean_ACC_chest_mag <- rollapply(subject_p$ACC_chest_mag, 20, mean)
  sd_ACC_chest_mag <- rollapply(subject_p$ACC_chest_mag, 20, sd)
  
  ACC_chest_df <- tibble(
    participant = p,
    label = label_5hz,
    mean_chest_ACC_X = mean_chest_ACC_X,
    mean_chest_ACC_Y = mean_chest_ACC_Y,
    mean_chest_ACC_Z = mean_chest_ACC_Z,
    mean_chest_ACC_sum = mean_chest_ACC_sum,
    sd_chest_ACC_X = sd_chest_ACC_X,
    sd_chest_ACC_Y = sd_chest_ACC_Y,
    sd_chest_ACC_Z = sd_chest_ACC_Z,
    sd_chest_ACC_sum = sd_chest_ACC_sum,
    peak_chest_ACC_X = peak_chest_ACC_X,
    peak_chest_ACC_Y = peak_chest_ACC_Y,
    peak_chest_ACC_Z = peak_chest_ACC_Z,
    mean_ACC_chest_mag = mean_ACC_chest_mag,
    sd_ACC_chest_mag = sd_ACC_chest_mag
  )
  
  # wrist ACC
  mean_wrist_ACC_X <- rollapply(subject_p$ACC_wrist_X, 20, mean)
  mean_wrist_ACC_Y <- rollapply(subject_p$ACC_wrist_Y, 20, mean)
  mean_wrist_ACC_Z <- rollapply(subject_p$ACC_wrist_Z, 20, mean)
  mean_wrist_ACC_sum <-  mean_wrist_ACC_X + mean_wrist_ACC_Y + mean_wrist_ACC_Z
  
  sd_wrist_ACC_X <- rollapply(subject_p$ACC_wrist_X, 20, sd)
  sd_wrist_ACC_Y <- rollapply(subject_p$ACC_wrist_Y, 20, sd)
  sd_wrist_ACC_Z <- rollapply(subject_p$ACC_wrist_Z, 20, sd)
  sd_wrist_ACC_sum <- sd_wrist_ACC_X + sd_wrist_ACC_Y + sd_wrist_ACC_Z
  
  peak_wrist_ACC_X <- rollapply(subject_p$ACC_wrist_X, 20, max)
  peak_wrist_ACC_Y <- rollapply(subject_p$ACC_wrist_Y, 20, max)
  peak_wrist_ACC_Z <- rollapply(subject_p$ACC_wrist_Z, 20, max)
  
  # wrist ACC magnitude
  mean_ACC_wrist_mag <- rollapply(subject_p$ACC_wrist_mag, 20, mean)
  sd_ACC_wrist_mag <- rollapply(subject_p$ACC_wrist_mag, 20, sd)
  
  ACC_wrist_df <- tibble(
    participant = p,
    label = label_5hz,
    mean_wrist_ACC_X = mean_wrist_ACC_X,
    mean_wrist_ACC_Y = mean_wrist_ACC_Y,
    mean_wrist_ACC_Z = mean_wrist_ACC_Z,
    mean_wrist_ACC_sum = mean_wrist_ACC_sum,
    sd_wrist_ACC_X = sd_wrist_ACC_X,
    sd_wrist_ACC_Y = sd_wrist_ACC_Y,
    sd_wrist_ACC_Z = sd_wrist_ACC_Z,
    sd_wrist_ACC_sum = sd_wrist_ACC_sum,
    peak_wrist_ACC_X = peak_wrist_ACC_X,
    peak_wrist_ACC_Y = peak_wrist_ACC_Y,
    peak_wrist_ACC_Z = peak_wrist_ACC_Z,
    mean_ACC_wrist_mag = mean_ACC_wrist_mag,
    sd_ACC_wrist_mag = sd_ACC_wrist_mag
  )
  
  ACC_chest_list[[i]] <- ACC_chest_df
  ACC_wrist_list[[i]] <- ACC_wrist_df
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
BVP_wrist_eng <- do.call(bind_rows, BVP_wrist_list)
ECG_chest_eng <- do.call(bind_rows, ECG_chest_list)

ACC_chest_eng <- do.call(bind_rows, ACC_chest_list)
ACC_wrist_eng <- do.call(bind_rows, ACC_wrist_list)

# add all of our engineered values/dataframes into here 
final_data <- bind_cols(EDA_chest_eng, 
                        EDA_wrist_eng %>% select(-participant, -label), 
                        EMG_eng %>% select(-participant, -label), 
                        Temp_chest_eng %>% select(-participant, -label),
                        Temp_wrist_eng %>% select(-participant, -label),
                        inhale_eng %>% select(-participant, -label),
                        exhale_eng %>% select(-participant, -label),
                        resp_eng %>% select(-participant, -label),
                        breath_eng %>% select(-participant, -label),
#                        ACC_chest_eng %>% select(-participant, -label),
#                        ACC_wrist_eng %>% select(-participant, -label),
                        BVP_wrist_eng %>% select(-participant, -label),
                        ECG_chest_eng %>% select(-participant, -label))

final_data_acc <- bind_cols(ACC_chest_eng,
                            ACC_wrist_eng)

final_data <- final_data %>%
  mutate(i_e_ratio = abs(mean_inhale/mean_exhale))
saveRDS(final_data, file = "final_data.Rds")
 
#only for ACC
saveRDS(final_data_acc, file= "final_data_acc.Rds")
