
# Set system time
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(data.table)
library(ggnewscale)

# What files are there in the data directory? 
monroe <- list.files("./data/Monroe/",
                     #                     pattern = "*csv", 
                     pattern = "2022_01_22_AC2110_DI06",
                     # pattern = "2023_01_14_KI18",    # debugging
                     full.names = TRUE)

# print the name of each file
for (file in monroe) {
  print(file)
}

length(monroe)

for (file in monroe) {
  
  #---------------------
  # 1. import tag data
  #---------------------
  tagdat <- data.table::fread(file,  
                              header = T,
                              showProgress = T) 
  
  id = tools::file_path_sans_ext(basename(file))
  
  tagdat$id = id
  
  tagdat = tagdat %>% 
    dplyr::select(id, Timestamp, Depth) %>% 
    rename(depth = Depth, date.time = Timestamp)
  
  tagdat$date.time = as.POSIXct(tagdat$date.time, format="%Y/%m/%d %H:%M:%S")
  
  #-----------------------------
  # 2. import video annotations
  #-----------------------------
  labdat <- data.table::fread(paste0('./data/Monroe_OUT/', id,'_OUT.csv'),
                              header = T,
                              showProgress = T) 
  
  # remove columns with no use
  labdat = labdat %>% 
    dplyr::select(PCE, vid, vid_time) 
  
  #----------------------------------
  # 3. merge tag and annotation data
  #----------------------------------
  df_25 = cbind(tagdat, labdat)
  
  #--------------------------------------------------------------  
  # replace behavioral annotations that we are not interested in
  #--------------------------------------------------------------
  # all PCE are labelled 1 or 2: change to binary prey captures
  df_25 = df_25 %>% 
    mutate(PCE_pce = ifelse(PCE == 2, 1, 
                            ifelse(PCE >= 3, 0, 
                                   PCE)))
  sum(df_25$PCE_pce) 
  
  #---------------------------------------------------------------
  # binary dark frames: all dark sections were labelled with a 9
  #---------------------------------------------------------------
  df_25 = df_25 %>% 
    mutate(PCE_dark = ifelse(PCE < 9, 0, 
                             ifelse(PCE == 9, 1, 
                                    PCE)))
  
  # sum prey captures per second, to reduce the data from 25 Hz to 1 Hz (TDR sampling rate)
  PCE_sum = df_25 %>% 
    group_by(date.time) %>%
    summarise(PCE_1hz = sum(PCE_pce),
              darkvid = sum(PCE_dark))
  
  sum(PCE_sum$PCE_1hz) # total PCE
  sum(PCE_sum$darkvid) # 0 = no darkness
  
  
  #--------------------------------------------------------------
  # remove video NA
  #--------------------------------------------------------------
  
sum(df_25$PCE_pce) 
max(df_25$PCE_pce)
  
alldat_pce = df_25 %>%
    filter(PCE_pce == 1)

alldat_pce 
nrow(alldat_pce )
  
  