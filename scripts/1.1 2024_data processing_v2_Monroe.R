
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# April 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# Pre-processing of tag input and annotation data.
# Import all csv files and save plots of dives

#-----------------------------------
# Setup
#-----------------------------------

# Set system time
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(data.table)
library(ggnewscale)

#-----------------------------------
# Data
#-----------------------------------
# Monroe island:
#----------------------
# loop
# 1. import tag data (25 Hz)
# 2. import video annotations (25 Hz - same row sequence as tag data)
# 3. merge tag and annotation data
# 4. cut data to video-recording period (do this the "difficult" way, so as to keep TDR data at 1Hz)
# 5. save for import to dive analysis
#----------------------

# What files are there in the data directory? 
monroe <- list.files("./data/Monroe/",
                      pattern = "*csv", 
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

#==============================================================
# Save data for later sanity check
sanity_d = df_25 %>% 
           drop_na(depth)

saveRDS(sanity_d, paste0('./data_sanitycheck_1hz/', id,'.rds'))
#==============================================================

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

#-------------------------------------
# subset to 1 Hz (TDR sampling rate)
#-------------------------------------
df1 = df_25 %>% 
  drop_na(depth)  

# merge 1Hz summed PCE to this df
df1 = merge(df1, PCE_sum, by = "date.time")
df1

df1_deployment = df1  # keep a copy of the 'full' TDR record to plot later

#---------------------------------------------------
# remove TDR data before and after video period
#---------------------------------------------------
# drop all data not covered by video and 
# first 20 sec. for all deployments (to remove initial 10 to 15 seconds video sections)
temp = df1 %>% 
  drop_na(vid_time) %>%
  slice(31:n())

first = min(temp$vid_time, na.rm = T)
first_date.time = temp$date.time[temp$vid_time %in% first]
first_date.time = first_date.time  

last = max(temp$vid_time, na.rm = T)
last_date.time = temp$date.time[temp$vid_time %in% last]

df1 = df1 %>% 
    filter(date.time >= first_date.time) %>% 
    filter(date.time <= last_date.time)

# remove final video snip from this specific animal (prelim plotting showed this is needed)
if (id == "2023_01_14_KI18") {
      # If this "id", remove the last 11 rows
      df1 <- df1 %>%filter(id != "2023_01_14_KI18"| row_number() <= (n() - 11))}

# sanity check: time between rows (gap duration)
temp$prev_time = lag(temp$vid_time, default = temp$vid_time[1])
temp$timegap = temp$vid_time - temp$prev_time
tst = as.data.frame(table(temp$timegap))

tstplot = ggplot(data = temp, aes(date.time, date.time)) + 
          geom_point(colour = "red") + 
          ggtitle(paste("Max gap (sec) in vid_time = ", max(as.numeric(as.character(tst$Var1)))))

#tstplot + table_grob

ggsave(plot = tstplot# + table_grob
       , bg = 'white',
       filename = paste0("./plots/vid_time_gaps_check/", id, "_cropped.png"), width=8,height=8)

#-------------------
# exploratory plots
#-------------------
# create binary PCE variable to plot
df1$plotPCE = df1$PCE_1hz 
df1$plotPCE[df1$plotPCE > 1] <- 1

# add time between rows as a 'check' (should mostly be 1 second) (1 Hz sampling rate)
df1 = df1 %>%
      mutate(diff = date.time - lag(date.time),
      diff_secs = as.numeric(diff, units = 'secs'))

table(df1$diff)

#----------------------------------------
# plot TDR data only where there is video
#----------------------------------------

fig = ggplot(data = df1,
             aes(x = date.time, y = depth))+
  geom_line() + 
  theme_minimal() + 
  geom_line(aes(y = 0), col = "black")+
  geom_jitter(aes(x = date.time, y = depth, shape = as.factor(plotPCE),
                  color = as.factor(plotPCE)),
              width = 0.1, height = 0.1)+
  scale_color_manual(values = c("transparent", "red"))+
  scale_shape_manual(values = c(1, 1))+
  theme(legend.position = "none") + 
  labs(title = paste(id, ". Sampling =", max(df1$diff, na.rm = T), 'Hz'),
       subtitle = paste("Annotated video PCE (all depths) =", sum(df1$PCE_1hz),
                        "; dark frames = ", sum(df1$darkvid)))

ggsave(plot = fig, bg = 'white',
       filename = paste0("./plots/videotime_PCE/", id, "_cropped.png"), width=8,height=8)

#-------------------------------------------------------------
# plot TDR data only where there is video, and rest of the TDR 
#-------------------------------------------------------------

# where in the foraging trip did we record video?

# first, merge the data sets
merged_df <- merge(df1_deployment, df1, by = "date.time", all.x = TRUE)

# Create a new column indicating whether the id appears in df2
merged_df$Appears_in_df1_deployment <- ifelse(!is.na(merged_df$id.y), TRUE, FALSE)

fig = ggplot() +
  geom_line(data = merged_df, aes(x = date.time, y = depth.x, color = Appears_in_df1_deployment)) + 
  scale_color_manual(values = c("grey51", "black"))+
  theme_minimal() + 
  geom_hline(yintercept = 0, col = "black") +
  new_scale_color() +
    geom_jitter(data = df1, aes(x = date.time, y = depth, shape = as.factor(plotPCE),
                              color = as.factor(plotPCE)),
              width = 0.1, height = 0.1) +
  scale_color_manual(values = c("transparent", "red")) +
  scale_shape_manual(values = c(1, 1)) +
  theme(legend.position = "none") +
  ggtitle(paste(id, ". Sampling =", max(df1$diff, na.rm = T), 'Hz')) +
  scale_x_datetime(limits = c(min(merged_df$date.time), max(merged_df$date.time))) +
  labs(title = paste(id, ". Sampling =", max(df1$diff, na.rm = T), 'Hz'),
       subtitle = paste("Annotated video PCE (all depths) =", sum(df1$PCE_1hz),
                        "; dark frames = ", sum(df1$darkvid)))

ggsave(plot = fig, bg = 'white',
       filename = paste0("./plots/TDR_time_videotime_divePCE/", id, ".png"), width=8,height=8)

# clean up
df1 = df1 %>% 
  dplyr::select(id, date.time, depth,
                vid, vid_time, 
                PCE_1hz, darkvid)
              
saveRDS(df1, paste0('./data_processed_1hz/', id,'.rds'))

} 

