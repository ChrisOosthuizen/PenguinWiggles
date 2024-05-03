
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# April 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# SANITY CHECK! 

#-----------------------------------
# Setup
#-----------------------------------

# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(viridis)
library(patchwork)

#-----------------------------------------------
# 1. import 1Hz TDR data (complete deployment) 
#-----------------------------------------------

# What files are there in the data directory? 
sandat <- list.files("./data_sanitycheck_1hz",
                          pattern = "*rds", 
                        #  pattern = '2022_01_13_AC2109_DIM07',
                          #pattern = '2022_01_07',
                          full.names = TRUE)

# print the name of each file
for (file in sandat) {
  print(file)
}

# how many files
length(sandat)

# initiate df to save summary data to:
summary_df <- data.frame(id = character(), 
                        dark_dives_N = integer(),
                        light_dives_N = integer(),
                        total_dives_N = integer(),
                        total_PCE_N = integer())
                        

perc_of_dive_that_is_dark_df <- data.frame(id = character(),
                                           dive.nr = integer(),
                                           dark_p = integer(),
                                           light_p = integer(),
                                           samples = integer(),
                                           perc_of_dive_that_is_dark =  numeric())

      
for (file in sandat) {
  
#---------------------
# 1. import 1 Hz tag data
#---------------------
tagdat = readRDS(file)
head(tagdat)
dim(tagdat)

tagdat$video.level = ifelse(is.na(tagdat$vid_time), "novideo", "video")

current_id = tools::file_path_sans_ext(basename(file))

#---------------------
# 1. import wiggle data
#---------------------
subdat_Halsey_05m = readRDS("./output/subdat_Halsey_05m_3m.rds")
unique(subdat_Halsey_05m$id)

subdat_Halsey_05m = subdat_Halsey_05m %>%
                    dplyr::filter(id == current_id)

unique(subdat_Halsey_05m$id)
unique(subdat_Halsey_05m$light.level)
unique(subdat_Halsey_05m$video.level)

#----------
# plot
#----------

c1 = ggplot(data = tagdat, aes(x = date.time, y = depth, color = video.level, shape = video.level)) + 
  geom_point(size = 0.5) +
  scale_color_manual(values = c("red", "black"))+ 
  scale_shape_manual(values = c("@", ".")) + 
  geom_hline(yintercept = 0, col = "black") +
  geom_text(data = tagdat, aes(x = date.time[1], y = max(depth), label = 'TDR data'), hjust = "left") + 
  geom_point(size = 0.5, data = subdat_Halsey_05m, aes(x = date.time, y = depth*-1,
                                      color = as.factor(video.level), shape= as.factor(video.level))) +
  geom_text(data = subdat_Halsey_05m, aes(x = date.time[1], y = max(depth)*-1, label = 'wiggle data'), hjust = "left") 
  
#c1

ggsave(plot = c1, bg = 'white',
       filename = paste0("./plots/TDR_time_wiggle_time/", current_id, "_cropped.png"), width=8,height=8)

#-----------------------------------------
# Filter wiggle data to remove non-vid 
#-----------------------------------------
subdat_Halsey_05m_vid_obs = subdat_Halsey_05m %>%
                            filter(video.level == 'video') 
unique(subdat_Halsey_05m_vid_obs$light.level)

c2 = ggplot()+
  geom_point(size = 0.5, data = tagdat, aes(x = date.time, y = depth, color = video.level)) +
  scale_color_manual(values = c("red", "black"))+
  scale_shape_manual(values = '.') +
  geom_hline(yintercept = 0, col = "black") +
  geom_text(data = tagdat, aes(x = date.time[1], y = max(depth), label = 'TDR data'), hjust = "left") +
  new_scale_color() +

  geom_point(size = 0.5, data = subdat_Halsey_05m_vid_obs, aes(x = date.time, y = depth*-1,
                                     color = as.factor(light.level))) +
  geom_text(data = subdat_Halsey_05m, aes(x = date.time[1], y = max(depth)*-1, 
                                          label = 'wiggle data'), hjust = "left") +
  scale_color_manual(values = c("orange", "blue"))

#c2

ggsave(plot = c2, bg = 'white',
       filename = paste0("./plots/cropped_TDR_time_wiggle_time/", current_id, "_cropped.png"), width=8,height=8)

#-----------------------------------------
# Plot only data in analysis 
#-----------------------------------------
head(subdat_Halsey_05m_vid_obs)

table(subdat_Halsey_05m_vid_obs$light.level, subdat_Halsey_05m_vid_obs$darkvid)

# How many light and dark dives are there?
counts <- subdat_Halsey_05m_vid_obs %>%
  group_by(dive.nr) %>%
  summarize(dark_count = sum(light.level == 'dark'),
            light_count = sum(light.level == 'light'))

# Count IDs with 'yes' and 'no' states
num_dark <- sum(counts$dark_count > 0)
num_light <- sum(counts$light_count > 0)
num_dark
num_light
total_dives_N = num_dark+num_light

total_PCE_N = sum(subdat_Halsey_05m_vid_obs$PCE_1hz)

# create binary PCE variable to plot
subdat_Halsey_05m_vid_obs$plotPCE = subdat_Halsey_05m_vid_obs$PCE_1hz 
subdat_Halsey_05m_vid_obs$plotPCE[subdat_Halsey_05m_vid_obs$plotPCE > 1] <- 1

c3 = ggplot()+
  geom_point(size = 0.6, data = subdat_Halsey_05m_vid_obs, 
                         aes(x = date.time, y = depth*-1,
                          color = as.factor(light.level), shape= as.factor(video.level))) +
  geom_text(data = subdat_Halsey_05m, aes(x = date.time[1], y = max(depth)*-1, 
                                     label = 'wiggle data'), hjust = "left") +
  scale_color_manual(values = c("black", "gold2")) + 
  new_scale_color() +
  geom_jitter(data = subdat_Halsey_05m_vid_obs, 
              aes(x = date.time, y = depth*-1,
                  color = as.factor(plotPCE)),
              width = 0.1, height = 0.1, shape = 1)+
  scale_color_manual(values = c("transparent", "red"))+
  
  ggtitle(paste0(subdat_Halsey_05m_vid_obs$id, ".    PCE = ", sum(subdat_Halsey_05m_vid_obs$PCE_1hz)))+
  labs(subtitle = paste0("Dive N with dark segment = ", num_dark,
                         ".  Dive N light = ", num_light))
  
perc_of_dive_that_is_dark <- subdat_Halsey_05m_vid_obs %>%
  group_by(dive.nr) %>%
  summarize(dark_p = sum(darkvid > 12.5),
            light_p = sum(darkvid < 12.5),
            samples =  light_p + dark_p,
            perc_of_dive_that_is_dark = dark_p/samples)

perc_of_dive_that_is_dark

subdat_Halsey_05m_vid_obs = merge(subdat_Halsey_05m_vid_obs, perc_of_dive_that_is_dark, 'dive.nr')

c4 =  ggplot()+
  geom_point(size = 0.6, data = subdat_Halsey_05m_vid_obs, 
             aes(x = date.time, y = depth*-1,
                 color = perc_of_dive_that_is_dark, shape= as.factor(video.level))) +
  geom_text(data = subdat_Halsey_05m, aes(x = date.time[1], y = max(depth)*-1, 
                                          label = 'wiggle data'), hjust = "left") +
  #scale_color_manual(values = c("black", "gold2")) + 
  scale_color_viridis(direction = -1, begin = 0, end = 0.8, limits = c(0, 1),
                      option = "inferno")+
  new_scale_color() +
  geom_jitter(data = subdat_Halsey_05m_vid_obs, 
              aes(x = date.time, y = depth*-1,
                  color = as.factor(plotPCE)),
              width = 0.1, height = 0.1, shape = 1)+
  scale_color_manual(values = c("transparent", "red"))#+
 # ggtitle(paste0(subdat_Halsey_05m_vid_obs$id, ".    PCE = ", sum(subdat_Halsey_05m_vid_obs$PCE_1hz)))+
#  labs(subtitle = paste0("Dive N with dark segment = ", num_dark,
#                         ".  Dive N light = ", num_light))

#c3 / c4

ggsave(plot = c3 / c4, bg = 'white',
       filename = paste0("./plots/cropped_wiggle_time/", current_id, "_cropped.png"), width=8,height=8)


# Now try to not color by 1 single value for % darkness per dive, but by 'darkvid'
# which is the number of samples (25 Hz) that is dark within the 1 Hz time frame.

c5 =  ggplot()+
  geom_point(size = 1, data = subdat_Halsey_05m_vid_obs, 
             aes(x = date.time, y = depth*-1,
                 color = darkvid, shape = as.factor(video.level))) +
  geom_text(data = subdat_Halsey_05m, aes(x = date.time[1], y = max(depth)*-1, 
                                          label = 'wiggle data'), hjust = "left") +
  #scale_color_manual(values = c("black", "gold2")) + 
  scale_color_viridis(direction = -1, begin = 0, end = 0.8, limits = c(0, 25),
                      option = "inferno")+
  new_scale_color() +
  geom_jitter(data = subdat_Halsey_05m_vid_obs, 
              aes(x = date.time, y = depth*-1,
                  color = as.factor(plotPCE)),
              width = 0.1, height = 0.1, shape = 1)+
  scale_color_manual(values = c("transparent", "red"))#+
# ggtitle(paste0(subdat_Halsey_05m_vid_obs$id, ".    PCE = ", sum(subdat_Halsey_05m_vid_obs$PCE_1hz)))+
#  labs(subtitle = paste0("Dive N with dark segment = ", num_dark,
#                         ".  Dive N light = ", num_light))

ggsave(plot = c3 / c5, bg = 'white',
       filename = paste0("./plots/cropped_wiggle_time_darkframes/", current_id, "_cropped.png"), width=8,height=8)

# Append the results to the result data frame
summary_df <- rbind(summary_df, data.frame(id = current_id,
                                         dark_dives_N = num_dark,
                                         light_dives_N = num_light,
                                         total_dives_N = total_dives_N,
                                         total_PCE_N = total_PCE_N))


perc_of_dive_that_is_dark_df <- rbind(perc_of_dive_that_is_dark_df,
                                      data.frame(id = current_id,
                                                 dive.nr = perc_of_dive_that_is_dark$dive.nr,
                                                 dark_p = perc_of_dive_that_is_dark$dark_p,
                                                 light_p = perc_of_dive_that_is_dark$light_p,
                                                 samples = perc_of_dive_that_is_dark$samples,
                                                 perc_of_dive_that_is_dark = perc_of_dive_that_is_dark$perc_of_dive_that_is_dark))


}


saveRDS(summary_df, './data_sanitycheck_1hz/summary_df.rds')

saveRDS(perc_of_dive_that_is_dark_df, './data_sanitycheck_1hz/perc_of_dive_that_is_dark_df.rds')


summary_df %>%
       arrange(light_dives_N)

head(perc_of_dive_that_is_dark_df)

# Exclude
#2022_01_13_AC2109_DIM07
#2022_01_13_AC2115_KIM01
#2023_01_14_KI16
#2022_01_10_AC2107_DI04 


