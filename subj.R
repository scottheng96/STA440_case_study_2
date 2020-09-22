library(tidyverse)
path = "/hpc/group/sta440-f20/WESAD/WESAD/"
id <- c('S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8', 'S9', 'S10', 'S11', 'S13', 'S14', 'S15', 'S16', 'S17')
subj_info_list = list()
for (i in 1:(id %>% length())) {
  subj_info <- read.delim(paste0(path, id[i], "/", id[i], "_readme.txt"), sep=":", header=FALSE) %>% 
    filter(V2 != "") %>% 
    tidyr::spread(V1, V2) %>% 
    mutate(Participant = id[i])
  
  subj_info_list[[i]] <- subj_info
  
}
subj_df <- do.call(bind_rows, subj_info_list)

saveRDS(subj_df, "/hpc/group/sta440-f20/es321/subj_df.Rds")