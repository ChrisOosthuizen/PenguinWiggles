
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# April 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# Calculate PCE per dive from video observations
# Calculate wiggle metrics for inferred PCE per dive

#-----------------------------------
# Setup
#-----------------------------------

# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(lubridate)

# import diveMove dive data (dive summary and 1 Hz depth data)
dive.stats = readRDS('./output/dive_stats.rds')
divedat = readRDS('./output/divedat.rds')

# Exclude animals based on sanity check:
dive.stats = dplyr::filter(dive.stats, 
                            id != '2022_01_13_AC2109_DIM07' &
                            id != '2022_01_13_AC2115_KIM01' &
                            id != '2023_01_14_KI16' &
                            id != '2022_01_10_AC2107_DI04') 

divedat = dplyr::filter(divedat, 
                          id != '2022_01_13_AC2109_DIM07' &
                          id != '2022_01_13_AC2115_KIM01' &
                          id != '2023_01_14_KI16' &
                          id != '2022_01_10_AC2107_DI04')

# Get unique IDs
unique_ids <- unique(dive.stats$id)
length(unique_ids)

# debugging 
 # dive.stats = dplyr::filter(dive.stats, id == "2023_01_14_DI4" |
 #                                id == '2022_01_13_AC2115_KIM01')
 # divedat = dplyr::filter(divedat, id == "2023_01_14_DI4" |
 #                             id == '2022_01_13_AC2115_KIM01')

#======================================
# 3. Calculate prey captures per dive
#======================================

# Loop through each individual, and then loop through each dive in dive.stats
# (loop through each dive deeper than 1 m and longer than 3 sec)

#---------------------------
# Set up df to store results
#---------------------------
# Create results data frame to store summary data: 
result_all <- data.frame(id = character(), 
                        dive.nr = numeric(),
                        video_pce_IN = numeric(),
                        video_pce_OUT = numeric(),
                        wiggleN_Halsey_1m = numeric(),
                        wiggleN_Halsey_2m = numeric(),
                        wiggleN_Simeone = numeric(),
                        wiggleN_Takahashi = numeric(),
                        light.level = character(),
                        video.level = character())


subdat_Halsey_1m_df = NULL
subdat_Halsey_2m_df = NULL
subdat_Simeone_df = NULL
subdat_Takahashi_df = NULL

#----------------------------------------------------------------
# Outer loop: Iterate through unique_ids
#----------------------------------------------------------------
for (current_id in unique_ids) {

  # Create lists to store data with wiggle labels 
subdat_Halsey_1m <- list()
subdat_Halsey_2m <- list()
subdat_Simeone <- list()
subdat_Takahashi <- list()
  
# current_id = '2022_01_13_AC2115_KIM01'
# current_id = '2022_01_13_AC2113_DAGM-UK'
# current_dive = 10
#----------------------------------------------------------------
# Inner loop: Iterate through dive numbers for the current ID
#----------------------------------------------------------------
dives_per_id <- unique(dive.stats$dive.nr[dive.stats$id == current_id])

for (current_dive in dives_per_id) {
  
  # Subset the data for the current ID
  dive.stats_sub <- dive.stats[dive.stats$id == current_id, ]
  
  start_time_PCE <- dive.stats_sub$begdesc[current_dive]   # dive start time
  
# end_time: include the post-dive period here, so that you can sum up the PCE in the video
# otherwise any PCE at the surface is 'lost' and the individual dive PCE does not add up to the total PCE.
    # Check if there is a subsequent dive
  if (current_dive + 1 <= length(dive.stats_sub$begdesc)) {

# the line below is the beginning of the descent of next dive, minus one second (so that that dive is not included)
     end_time_PCE <- dive.stats_sub$begdesc[current_dive + 1] - 1
    # Continue with the rest of your code using start_time and end_time
  } else {
    # Handle the case when there is no subsequent dive (then use the 'end time' of the dive)
    end_time_PCE <- dive.stats_sub$enddive[current_dive]
  }
  
  start_time_PCE  # the is the begdesc time in dives
  end_time_PCE    # this is 1 second less than the start time of the next dive in dives
  
  # Subset data based on start and end times of dive; and restrict to PCE in dives > depth.threshold 
  # PCE still have '9' labels - exclude them here (periods that are dark)
  subdat_PCE = divedat %>% 
    dplyr::filter(id == current_id & 
                    date.time >= start_time_PCE & 
                    date.time <= end_time_PCE) 
# ggplot(data = subdat_PCE, aes(x = date.time, y = depth, color = as.factor(PCE_1hz)))+ geom_point()
# ggplot(data = subdat_PCE, aes(x = date.time, y = depth, color = as.factor(darkvid)))+ geom_point()
  
#---------------------------------------------------------  
# Calculate observed / annotated video PCE in dive:  
#---------------------------------------------------------
  # Actual video PCE in dive using diveMove dive phase definition:  
  video_pce_IN = subdat_PCE %>%
    filter(InDive != 'X') %>%
    summarise(video_pce_IN = sum(PCE_1hz)) 
  video_pce_IN
  
  # Actual video PCE outside dive, using divemove dive phase definitions:  
  video_pce_OUT = subdat_PCE %>%
    filter(InDive == 'X') %>%
    summarise(video_pce_OUT = sum(PCE_1hz)) 
  video_pce_OUT
  
# contains the data for 1 dive (deeper than the threshold) and the post-dive duration

#===================================
# 4. Wiggle analysis 
#===================================
# remove post-dive duration
  start_time_wig <- start_time_PCE    # dive start time
  end_time_wig <- dive.stats_sub$enddive[current_dive]
  
  start_time_wig  # the is the begdesc time in dives
  end_time_wig    
  
  # Subset data based on start and end times of dive
  subdat = divedat %>% 
    dplyr::filter(id == current_id & 
                    date.time >= start_time_wig & 
                    date.time <= end_time_wig) 
  #dive only 
  #ggplot(data = subdat, aes(x = date.time, y = depth, color = as.factor(PCE_1hz)))+ geom_point()
  
  # light.level? (binary classification across entire dive)
  # If more than half of the 25 Hz samples were dark then that 'second' is assumed dark, in the 1 Hz.
  light.level = ifelse(any(subdat$darkvid > 12), 'dark', 'light')
  #light.level = ifelse(any(subdat_PCE$darkvid > 0), 'dark', 'light')  # This would be if ANY one of the 25 Hz samples is dark, then it is dark.
  subdat$light.level = light.level
  
  # Because 'any' is in the code above, you are still labelling the ENTIRE dive as being 'dark', when e.g.
  # only 1 or 3 seconds of the entire dive might have been dark. Why not only cut out the dark sections?
  # This is possible, but then it is difficult to relate e.g. dive depth to PCE.
  
  # video taken? (binary classification across entire dive)
  video.level = ifelse(any(is.na(subdat$vid_time)), "novideo", "video")
  subdat$video.level = video.level
  
  # NON-binary classification of whether there was a dive or not.
  subdat <- subdat %>%
    mutate(video.level_1Hz = ifelse(is.na(vid_time), "novideo", "video"))
  
  #----------------------------------------------------------------------------------
  # Calculate wiggles following Halsey et al. 2007 
  #----------------------------------------------------------------------------------
source("./scripts/3.2 2024_wiggle_function_Halsey.R")
wiggleN_Halsey_1m = Halsey_wiggles(subdat, wiggle_threshold = 1, wiggle_threshold2 = 0)
wiggleN_Halsey_2m = Halsey_wiggles(subdat, wiggle_threshold = 2, wiggle_threshold2 = 0)

source("./scripts/3.3 2024_wiggle_function_Takahashi.R")
wiggleN_Takahashi = Takahashi_wiggles(subdat)

source("./scripts/3.4 2024_wiggle_function_Simeone_Wilson.R")
wiggleN_Simeone = Simeone_wiggles(subdat, wiggle_threshold = 0.3)


  #---------------------------------
  # Save results
  #---------------------------------
  # Append the results to the result data frame
  result_all <- rbind(result_all, data.frame(id = current_id,
                                           dive.nr = current_dive,
                                           video_pce_IN = video_pce_IN,
                                           video_pce_OUT = video_pce_OUT,
                                           wiggleN_Halsey_1m = wiggleN_Halsey_1m$wiggleN_Halsey,
                                           wiggleN_Halsey_2m = wiggleN_Halsey_2m$wiggleN_Halsey,
                                           wiggleN_Simeone = wiggleN_Simeone$wiggleN_Simeone,
                                           wiggleN_Takahashi = wiggleN_Takahashi$wiggleN_Takahashi,
                                           light.level = light.level,
                                           video.level = video.level))

wiggleN_Halsey_1m$subdat_Halsey$dive.nr = current_dive
wiggleN_Halsey_2m$subdat_Halsey$dive.nr = current_dive
wiggleN_Simeone$subdat_Simeone$dive.nr = current_dive
wiggleN_Takahashi$subdat_Takahashi$dive.nr = current_dive

  subdat_Halsey_1m[[current_dive]] = wiggleN_Halsey_1m$subdat_Halsey
  subdat_Halsey_2m[[current_dive]] = wiggleN_Halsey_2m$subdat_Halsey
  subdat_Simeone[[current_dive]] = wiggleN_Simeone$subdat_Simeone
  subdat_Takahashi[[current_dive]] = wiggleN_Takahashi$subdat_Takahashi
  
  subdat_Halsey_1m_df[[current_id]] =  bind_rows(subdat_Halsey_1m)
  subdat_Halsey_2m_df[[current_id]] =  bind_rows(subdat_Halsey_2m)
  subdat_Simeone_df[[current_id]] =  bind_rows(subdat_Simeone)
  subdat_Takahashi_df[[current_id]] =  bind_rows(subdat_Takahashi)
  
 }
  }

#-------------------------------------
# Summarise
#-------------------------------------
subdat_Halsey_1m = bind_rows(subdat_Halsey_1m_df)
unique(subdat_Halsey_1m$id)

subdat_Halsey_2m = bind_rows(subdat_Halsey_2m_df)
unique(subdat_Halsey_2m$id)

subdat_Simeone = bind_rows(subdat_Simeone_df)
unique(subdat_Simeone$id)

subdat_Takahashi = bind_rows(subdat_Takahashi_df)
unique(subdat_Takahashi$id)

unique(subdat_Halsey_1m$video.level)
unique(subdat_Halsey_1m$light.level)

#-------------------
# Save the output
#-------------------
saveRDS(result_all, "./output/result_all_3m.rds")
saveRDS(subdat_Halsey_1m, "./output/subdat_Halsey_1m_3m.rds")
saveRDS(subdat_Halsey_2m, "./output/subdat_Halsey_2m_3m.rds")
saveRDS(subdat_Simeone, "./output/subdat_Simeone_3m.rds")
saveRDS(subdat_Takahashi, "./output/subdat_Takahashi_3m.rds")

