library(tidyverse)

id <- c('S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17')
chest_files <- paste0("./output_data/",id, "_chest.csv")
wrist_files <- paste0("./output_data/",id, "_wrist.csv")

all_data = data_frame() # had total of 347372 samples, after downsampling to 4Hz, still kept all labels 0-7
chest_data = data_frame()
wrist_data = data_frame()

for (i in 1:length(id)) {
  chest <- read_csv(chest_files[i])
  wrist <- read_csv(wrist_files[i]) 
  subject <- bind_cols(chest, wrist %>% select(-X1, -Label, -Participant))
  
  all_data <- bind_rows(all_data, subject)
  chest_data <- bind_rows(chest_data, chest)
  wrist_data <- bind_rows(wrist_data, wrist)
}


all_data_filtered <- all_data %>% 
  filter(Label %in% c(1, 2, 3)) 
chest_data_filtered <- chest_data %>% 
  filter(Label %in% c(1, 2, 3)) 
wrist_data_filtered <- wrist_data %>% 
  filter(Label %in% c(1, 2, 3)) 



saveRDS(all_data_filtered, "chest_and_wrist.Rds")
saveRDS(chest_data_filtered, "chest.Rds")
saveRDS(wrist_data_filtered, "wrist.Rds")

#readRDS("chest_and_wrist.Rds")