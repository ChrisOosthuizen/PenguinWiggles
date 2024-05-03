
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
library(patchwork)
library(viridis)
library(colorspace)
library(ggridges)

source('./scripts/0.1 ggplot theme.R') 

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
                        wiggleN_Halsey_05m = numeric(),
                        wiggleN_Halsey_2m = numeric(),
                        wiggleN_Simeone = numeric(),
                        wiggleN_Takahashi = numeric(),
                        light.level = character(),
                        video.level = character())


subdat_Halsey_05m_df = NULL
subdat_Halsey_2m_df = NULL
subdat_Simeone_df = NULL
subdat_Takahashi_df = NULL

#----------------------------------------------------------------
# Outer loop: Iterate through unique_ids
#----------------------------------------------------------------
for (current_id in unique_ids) {

  # Create lists to store data with wiggle labels 
subdat_Halsey_05m <- list()
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
source("./scripts/4.1 2024_wiggle_function_Halsey.R")
wiggleN_Halsey_05m = Halsey_wiggles(subdat, wiggle_threshold = 0.5, wiggle_threshold2 = 0)
wiggleN_Halsey_2m = Halsey_wiggles(subdat, wiggle_threshold = 2, wiggle_threshold2 = 0)

source("./scripts/4.2 2024_wiggle_function_Simeone_Wilson.R")
wiggleN_Simeone = Simeone_wiggles(subdat, wiggle_threshold = 0.3)

source("./scripts/4.3 2024_wiggle_function_Takahashi.R")
wiggleN_Takahashi = Takahashi_wiggles(subdat)


  #---------------------------------
  # Save results
  #---------------------------------
  # Append the results to the result data frame
  result_all <- rbind(result_all, data.frame(id = current_id,
                                           dive.nr = current_dive,
                                           video_pce_IN = video_pce_IN,
                                           video_pce_OUT = video_pce_OUT,
                                           wiggleN_Halsey_05m = wiggleN_Halsey_05m$wiggleN_Halsey,
                                           wiggleN_Halsey_2m = wiggleN_Halsey_2m$wiggleN_Halsey,
                                           wiggleN_Simeone = wiggleN_Simeone$wiggleN_Simeone,
                                           wiggleN_Takahashi = wiggleN_Takahashi$wiggleN_Takahashi,
                                           light.level = light.level,
                                           video.level = video.level))

wiggleN_Halsey_05m$subdat_Halsey$dive.nr = current_dive
wiggleN_Halsey_2m$subdat_Halsey$dive.nr = current_dive
wiggleN_Simeone$subdat_Simeone$dive.nr = current_dive
wiggleN_Takahashi$subdat_Takahashi$dive.nr = current_dive

  subdat_Halsey_05m[[current_dive]] = wiggleN_Halsey_05m$subdat_Halsey
  subdat_Halsey_2m[[current_dive]] = wiggleN_Halsey_2m$subdat_Halsey
  subdat_Simeone[[current_dive]] = wiggleN_Simeone$subdat_Simeone
  subdat_Takahashi[[current_dive]] = wiggleN_Takahashi$subdat_Takahashi
  
  subdat_Halsey_05m_df[[current_id]] =  bind_rows(subdat_Halsey_05m)
  subdat_Halsey_2m_df[[current_id]] =  bind_rows(subdat_Halsey_2m)
  subdat_Simeone_df[[current_id]] =  bind_rows(subdat_Simeone)
  subdat_Takahashi_df[[current_id]] =  bind_rows(subdat_Takahashi)
  
 }
  }

#-------------------------------------
# Summarise
#-------------------------------------
subdat_Halsey_05m = bind_rows(subdat_Halsey_05m_df)
unique(subdat_Halsey_05m$id)

subdat_Halsey_2m = bind_rows(subdat_Halsey_2m_df)
unique(subdat_Halsey_2m$id)

subdat_Simeone = bind_rows(subdat_Simeone_df)
unique(subdat_Simeone$id)

subdat_Takahashi = bind_rows(subdat_Takahashi_df)
unique(subdat_Takahashi$id)

unique(subdat_Halsey_05m$video.level)
unique(subdat_Halsey_05m$light.level)

#-------------------
# Save the output
#-------------------
# saveRDS(result_all, "./output/result_all_3m.rds")
# saveRDS(subdat_Halsey_05m, "./output/subdat_Halsey_05m_3m.rds")
# saveRDS(subdat_Halsey_2m, "./output/subdat_Halsey_2m_3m.rds")
# saveRDS(subdat_Simeone, "./output/subdat_Simeone_3m.rds")
# saveRDS(subdat_Takahashi, "./output/subdat_Takahashi_3m.rds")

#-------------------
# Read the output
#-------------------
result_all = readRDS("./output/result_all_3m.rds") 
subdat_Halsey_05m = readRDS("./output/subdat_Halsey_05m_3m.rds")
subdat_Halsey_2m = readRDS("./output/subdat_Halsey_2m_3m.rds")
subdat_Simeone = readRDS("./output/subdat_Simeone_3m.rds")
subdat_Takahashi = readRDS("./output/subdat_Takahashi_3m.rds")

length(unique(result_all$id))

# make a 'general' subdat from any of the above:
nrow(subdat_Halsey_05m)
nrow(subdat_Halsey_2m)
nrow(subdat_Simeone)
nrow(subdat_Takahashi)

subdat_r = subdat_Takahashi %>%
           select(-wig_depth, -wiggles_Takahashi, -dive.nr)
head(subdat_r)

#--------------------
# Data wrangling
#--------------------
# merge with output from diveMove
result_all = merge(result_all, dive.stats, by = c('id', 'dive.nr'))
dim(result_all)  # number of dives (dark and non-video included) = 3039
head(result_all)

#-----------------------------------------
# add ascent speed and year of sampling
#-----------------------------------------
result_all$ascspeed = result_all$begasc_dep / result_all$asctim 
result_all$year = year(result_all$begdesc)

#--------------------------------
# Replace botttim NA with zero:
#--------------------------------
result_all = result_all %>% 
  dplyr::mutate(botttim = replace_na(botttim, 0)) %>% 
  dplyr::mutate(bottdist = replace_na(bottdist, 0)) 


wiggle_summary_all_data = result_all %>%
  group_by(id, video.level, light.level) %>%
  # filter(light.level == light)
  summarise(
    surfacePCE = sum(video_pce_OUT),
    divingPCE = sum(video_pce_IN),
    totalPCE_wiggle = surfacePCE + divingPCE, 
    totalPCE_dive = mean(totalPCE),
    diff_pce = totalPCE_dive - totalPCE_wiggle, 
    wiggle_sum_Halsey_05m = sum(wiggleN_Halsey_05m),
    wiggle_sum_Halsey_2m = sum(wiggleN_Halsey_2m),
    wiggle_sum_Simeone = sum(wiggleN_Simeone),
    wiggle_sum_Takahashi = sum(wiggleN_Takahashi),
    bottdist_sum = sum(bottdist, na.rm = TRUE)) %>%
  ungroup()

wiggle_summary_all_data   # totalPCE calculated here (surfacePCE + divingPCE) match 
# totalPCE in the 2.1 dive analysis script (sum of annotations)

# Some dives included periods too dark to see whether prey captures happened in the video
wiggle_sum_novid = wiggle_summary_all_data %>%
          filter(video.level == 'novideo')

wiggle_sum_vid = wiggle_summary_all_data %>%
         filter(video.level == 'video')

# Work only with light dives, where there are video.
result_df = result_all %>%
  filter(video.level == 'video',
         light.level == 'light') 

#view(result_df)

# how many dives?
n_distinct(result_df$id)
length(result_df$dive.nr)

# what is diveN_annotated? (diveN included ALL dives; some may have been dark or had video gaps)
result_df = result_df %>%
  group_by(id) %>% 
  mutate(diveN_annotated = n())

# ad dcolumns to order data by mean maxdepth later on:
result_df = result_df %>%
   group_by(id) %>%
   mutate(mean_maxdep = mean(maxdep, na.rm = TRUE), 
          max_maxdep = max(maxdep, na.rm = TRUE))

#------------------------------------
# Calculate video sampling duration
#------------------------------------

vid_dur = result_df %>%
    group_by(id) %>%
    summarise(first_dive = min(begdesc),
            last_dive = max(enddive), 
            sample_duration = difftime(last_dive, first_dive, units = "hour"))

vid_dur
#view(vid_dur)

mean(vid_dur$sample_duration)
range(vid_dur$sample_duration)
sum(vid_dur$sample_duration)

#----------------------------------------------
# Calculate and plot mean maxdep for each id
#----------------------------------------------

# pull(id) is required to plot below (remove if you want a table with output)
mean_maxdep = result_df %>%
  group_by(id) %>%
  summarize(year = mean(year),
            mean_maxdep = mean(maxdep, na.rm = TRUE), 
            max_maxdep = max(maxdep, na.rm = TRUE)) %>%
  arrange(desc(year), desc(mean_maxdep)) %>%
  pull(id) 

# order IDs according to mean_maxdep
result_df$id <- factor(result_df$id, levels = mean_maxdep)

maxdep_id = result_df %>% 
  ggplot() + aes(x = maxdep, y = id, fill = after_stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Depth (m)", option = "C") +
  theme_bw() + 
#  theme(axis.text.y = element_blank(),
#        axis.ticks.y = element_blank()) + 
  geom_hline(yintercept = 13.5, linetype = "dashed", color = "red") +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) + 
  scale_y_discrete(labels = rev(seq_along(levels(result_df$id))))+
  ylab("Individual") + 
  xlab("Maximum depth (m)") +
  theme(legend.position = c(0.9, 0.85),  # Adjust legend position
        legend.background = element_blank()) + # Remove legend background  # Adjust legend position
    annotate("text", x = 140, y = 16, label = "2022", color = "red")+  # Add vertical text outside plot
    annotate("text", x = 140, y = 11, label = "2023", color = "red")  # Add vertical text outside plot

maxdep_id

# Save as PNG
ggsave("./plots/maxdep_id.png", maxdep_id, width = 6, height = 6, dpi = 1200)


#------------------------
# calculate dive bouts
#------------------------

# Calculate time difference between end of dive x and start of dive x+1
# Varified that the initial 3 calculations are correct time differences of inter-dive time.

result_df = result_df %>%
          group_by(id) %>%
          arrange(id, dive.nr) %>%
          mutate(dive.gap = difftime(begdesc, 
                               lag(begdesc, 
                               default = begdesc[1]), units = "min")) %>%
          mutate(bout = cumsum(ifelse(dive.gap > 10, 1, 0)))  %>%
  ungroup()
  
# 10 =   how many minnutes for a new bout to start?
# 1 =  number that each bout should 'advance' with if yes
# 0 =  number that each bout should 'advance' with if no 

# make the bouts start at bout 1, not at bout 0.
result_df$bout <- result_df$bout + 1     

head(result_df)

#--------------------------------------
# refine wiggle summary:
#--------------------------------------

wiggle_summary = result_df %>%
  group_by(id, video.level, light.level) %>%
  filter(light.level == 'light',
         video.level == 'video') %>%
  summarise(
    year = mean(year),
    surfacePCE = sum(video_pce_OUT),
    divingPCE = sum(video_pce_IN),
    totalPCE = surfacePCE + divingPCE, 
    totalPCE_dive = mean(totalPCE),
    diff_pce = totalPCE_dive - totalPCE, 
    total_diveN_annotated = mean(diveN_annotated),
    mean_maxdep = mean(mean_maxdep),
    wiggle_sum_Halsey_05m = sum(wiggleN_Halsey_05m),
    wiggle_sum_Halsey_2m = sum(wiggleN_Halsey_2m),
    wiggle_sum_Simeone = sum(wiggleN_Simeone),
    wiggle_sum_Takahashi = sum(wiggleN_Takahashi),
    bottdist_sum = sum(bottdist, na.rm = TRUE)) %>%
  ungroup()

wiggle_summary = wiggle_summary %>%
        dplyr::select(-totalPCE_dive, -diff_pce)

wiggle_summary
#view(wiggle_summary)

wiggle_summary %>%
#      group_by(year) %>%
      summarize(total_surfacePCE = sum(surfacePCE),
                total_divingPCE = sum(divingPCE),
                total_PCE = total_surfacePCE + total_divingPCE,
                perc_surface = (total_surfacePCE / total_PCE)*100)

# calculate percent surface PC by year
wiggle_summary %>%
  group_by(year) %>%
  summarize(total_surfacePCE = sum(surfacePCE),
            total_divingPCE = sum(divingPCE),
            total_PCE = total_surfacePCE + total_divingPCE,
            perc_surface = (total_surfacePCE / total_PCE)*100)

# Summary stats:
# Range of PCE at surface and in dives:
range(wiggle_summary$surfacePCE)
range(wiggle_summary$divingPCE)
range(wiggle_summary$totalPCE)
range(wiggle_summary$total_diveN_annotated)

#---------------------------
# Write summary tables
#---------------------------

# Table 1: PCE and dive stats
# Give animals IDs based on mean max depth (so that the IDs are the same order as the mean max depth figure)
tableS1 = wiggle_summary %>% 
         mutate(Year = substr(id, 1, 4)) %>% 
         arrange(Year, mean_maxdep)%>%
    mutate(ID = row_number()) %>%
    select(id, Year, ID, 
           surfacePCE, divingPCE, totalPCE,   # PCE stats
           total_diveN_annotated, mean_maxdep)  # dive stats 

# Table S1: sampling duration
tableS1 = merge(tableS1, vid_dur, by = "id") # merge so that we can get the same IDs as in other table

#tableS1$sample_dur_min = as.numeric(tableS1$sample_duration)*60
#tableS1$PCE_min = tableS1$totalPCE / tableS1$sample_dur_min

tableS1 = tableS1 %>%
               mutate(Date = as.Date(first_dive),
               Duration = round(as.numeric(sample_duration),2),
               mean_maxdep = round(mean_maxdep,2)) %>%
           select(id, Year, ID, Date, Duration, total_diveN_annotated, mean_maxdep,
                 surfacePCE, divingPCE, totalPCE)
          
mean(tableS1$mean_maxdep)
mean(tableS1$Duration)
mean(tableS1$totalPCE)
sd(tableS1$totalPCE)


#-----------------------------------------------------------
# Dive classification (foraging dive or non-foraging dive)
#-----------------------------------------------------------
# How well does wiggles work to classify dive types? (~ Manco et al.)

# Create a foraging_dive column based on video observations (is this a foraging dive?)
result_df = result_df %>%
  mutate(foraging_dive = ifelse(video_pce_IN == 0, "non-foraging", "foraging"))

# Create a foraging_bout column based on video observations (is this a foraging bout?)
# Here, a foraging bout is if more than 50 % of the dives in the bout are foraging dives
# CO: double checked outside R that this performs as intended;
# should check the 'biological importance' of a limit such as 50 %
result_df = result_df %>%
   group_by(id, bout) %>%
   mutate(foraging_bout = ifelse(mean(foraging_dive == "foraging") < 0.5, "non-foraging", "foraging")) %>%
   ungroup()


# Calculate how many foraging and non-foraging dives there are (video observations)
divetypeN_video <- result_df %>%  
    group_by(id) %>%
    summarise(foraging_video = sum(foraging_dive == "foraging"),
              nonforaging_video = sum(foraging_dive == "non-foraging"),
              total_dives = sum(foraging_video + nonforaging_video))

divetypeN_video

# combine with table Summary

tableS1 = merge(tableS1,divetypeN_video, by = "id")
  
tableS1 =  tableS1 %>% select(-id, - total_dives) %>%
  arrange(ID)

write.table(tableS1, './tables/tableS1.csv', sep = ",", row.names = F)


#-----------------------------------------------------------------------------------------
# Calculate how many foraging and non-foraging dives were predicted by each wiggle analysis
#-----------------------------------------------------------------------------------------

divetype_wiggles <- result_df %>%
     #     dplyr::filter(maxdep > 10) %>%
          group_by(id) %>%
          mutate(
            pred_wiggleN_Halsey_05m = ifelse(wiggleN_Halsey_05m > 0, "foraging", "non-foraging"),
            pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 0, "foraging", "non-foraging"),
            pred_wiggleN_Simeone = ifelse(wiggleN_Simeone > 0, "foraging", "non-foraging"),
            pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 1, "foraging", "non-foraging")) %>%
          summarise(
            foraging_Halsey_05m = sum(pred_wiggleN_Halsey_05m == "foraging"),
            nonforaging_Halsey_05m = sum(pred_wiggleN_Halsey_05m == "non-foraging"),
            
            foraging_Halsey_2m = sum(pred_wiggleN_Halsey_2m == "foraging"),
            nonforaging_Halsey_2m = sum(pred_wiggleN_Halsey_2m == "non-foraging"),
            
            foraging_Simeone = sum(pred_wiggleN_Simeone == "foraging"),
            nonforaging_Simeone = sum(pred_wiggleN_Simeone == "non-foraging"),
            
            foraging_Takahashi = sum(pred_wiggleN_Takahashi == "foraging"),
            nonforaging_Takahashi = sum(pred_wiggleN_Takahashi  == "non-foraging"))

divetype_wiggles 

all_divetype = merge(divetypeN_video, divetype_wiggles , by = 'id')

divetype_summary = all_divetype  %>%
    pivot_longer(cols = starts_with(c("foraging", "non-foraging")),
    names_to = "Wiggle_type",
    values_to = "dives") %>%
    separate(Wiggle_type , into = c("dive_type", "wiggle_metric"), sep = "_", extra = "merge") # %>%
     # pivot_wider(names_from = wiggle_metric, values_from = dives)
  
divetype_summary

divetype_summary$proportion = divetype_summary$dives / divetype_summary$total_dives
divetype_summary

#-------------------------------------------------------------------------------------
# How does max depth correlate with odds of foraging / non-foraging dives with wiggles?
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# Calculate foraging / non-foraging dives for wiggles, and whether these are 
# true positives, false positives, true negatives, etc.
#-------------------------------------------------------------------------------------

wiggledepth <- result_df %>%
  group_by(id) %>%
  mutate(
   # foraging_dive = foraging_dive,
    pred_wiggleN_Halsey_05m = ifelse(wiggleN_Halsey_05m > 0, "foraging", "non-foraging"),
    pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 0, "foraging", "non-foraging"),
    pred_wiggleN_Simeone = ifelse(wiggleN_Simeone > 0, "foraging", "non-foraging"),
    pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 1, "foraging", "non-foraging"))  # exclude the '1' compulsory wiggle per dive

wiggledepth = as.data.frame(wiggledepth)
head(wiggledepth)

wiggledepth <- wiggledepth %>%
    mutate(Halsey_05m = case_when(
         foraging_dive == "foraging" & pred_wiggleN_Halsey_05m == "foraging" ~ "true_pos",
         foraging_dive == "foraging" & pred_wiggleN_Halsey_05m == "non-foraging" ~ "false_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_05m == "non-foraging" ~ "true_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_05m == "foraging" ~ "false_pos"))  %>%
    mutate(Halsey_2m = case_when(
         foraging_dive == "foraging" & pred_wiggleN_Halsey_2m == "foraging" ~ "true_pos",
         foraging_dive == "foraging" & pred_wiggleN_Halsey_2m == "non-foraging" ~ "false_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_2m == "non-foraging" ~ "true_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_2m == "foraging" ~ "false_pos")) %>%

    mutate(Simeone = case_when(
         foraging_dive == "foraging" & pred_wiggleN_Simeone == "foraging" ~ "true_pos",
         foraging_dive == "foraging" & pred_wiggleN_Simeone == "non-foraging" ~ "false_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Simeone == "non-foraging" ~ "true_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Simeone == "foraging" ~ "false_pos"))  %>%
       
    mutate(Takahashi = case_when(
        foraging_dive == "foraging" & pred_wiggleN_Takahashi == "foraging" ~ "true_pos",
        foraging_dive == "foraging" & pred_wiggleN_Takahashi == "non-foraging" ~ "false_neg",
        foraging_dive == "non-foraging" & pred_wiggleN_Takahashi == "non-foraging" ~ "true_neg",
        foraging_dive == "non-foraging" & pred_wiggleN_Takahashi == "foraging" ~ "false_pos"))

head(wiggledepth)
# view(wiggledepth)


#-------------------------------------------------
# Plot confusion matrices
#-------------------------------------------------

# Create summary table
Halsey_05m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_05m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_05m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  # group_by(Reference) %>%
  ungroup() %>%
  mutate(prop = round(Freq/sum(Freq),2))

# Create summary table
Halsey_2m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_2m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_2m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
 # group_by(Reference) %>%
  ungroup() %>%
  mutate(prop = round(Freq/sum(Freq),2))

# Create summary table
Simeone_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Simeone) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Simeone,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  # group_by(Reference) %>%
  ungroup() %>%
  mutate(prop = round(Freq/sum(Freq),2))

# Create summary table
Takahashi_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Takahashi) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Takahashi,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  # group_by(Reference) %>%
  ungroup() %>%
  mutate(prop = round(Freq/sum(Freq),2))


# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Halsey_05m = 
  ggplot(data = Halsey_05m_table, 
         aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = 0.2, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  xlim(levels(Halsey_05m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Halsey et al. 2007 (0.5m)") + 
  xlab("Reference (video)")+
  ylab("Predicted (wiggles)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels
         
#CM_Halsey_05m

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Halsey_2m = 
  ggplot(data = Halsey_2m_table, 
         aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = 0.2, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  xlim(levels(Halsey_2m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Halsey et al. 2007 (2m)") + 
  xlab("Reference (video)")+
  ylab("Predicted (wiggles)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels

#CM_Halsey_2m


# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Simeone = 
  ggplot(data = Simeone_table, 
         aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = 0.2, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  xlim(levels(Simeone_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Simeone and Wilson 2003 (0.3m)") + 
  xlab("Reference (video)")+
  ylab("Predicted (wiggles)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels
#CM_Simeone

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups 
CM_Takahashi = 
  ggplot(data = Takahashi_table, 
         aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = 0.2, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
  xlim(levels(Takahashi_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Takahashi et al. 2004") + 
  xlab("Reference (video)")+
  ylab("Predicted (wiggles)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels  
#CM_Takahashi

Figure4 = cowplot::plot_grid(CM_Simeone, CM_Takahashi, CM_Halsey_05m, CM_Halsey_2m, ncol = 2,
                             align = "h", axis = "tblr") 
                             
Figure4

# # Save as PNG
#ggsave("./plots/CM_Takahashi.png", CM_Takahashi, width = 2.5, height = 2, dpi = 1200)

ggsave("./plots/confusion_matrix.png", Figure4, width = 4, height = 4, dpi = 1200)


#-------------------------------------------
# How can we plot ACCURACY?
#------------------------------------------

TF_plot = wiggledepth %>% 
          #    filter(maxdep > 3) %>%
             ggplot(aes(x = Halsey_05m, y = maxdep)) + 
             geom_jitter(alpha = 0.3,  color = "darkslategray") + 
             gg_theme()+ 
             ylab("Depth (m)") + 
          theme(
            axis.text = element_text(colour = "black", size = 14),
            axis.title = element_text(size=17)) 
TF_plot

# Save as PNG
#ggsave("./plots/TF_plot.png", TF_plot, width = 10, height = 3, dpi = 1500)


wiggledepth %>% 
     # filter(maxdep > 3) %>%
      ggplot(aes(x = Halsey_2m, y = maxdep)) + 
      geom_jitter(alpha = 0.3)

wiggledepth %>% 
     # filter(maxdep > 3) %>%
      ggplot(aes(x = Simeone, y = maxdep)) + 
      geom_jitter(alpha = 0.3)

wiggledepth %>% 
     # filter(maxdep > 3) %>%
      ggplot(aes(x = , y = Takahashi)) + 
      geom_density(alpha = 0.3) + facet_wrap(~ Takahashi, ncol = 2) 

ggplot(wiggledepth, aes(x = maxdep, y = (Halsey_2m))) +
  geom_point() +
  facet_wrap(~ Halsey_2m, ncol = 2) +
 # labs(x = "Continuous Variable", y = "Factor Variable") +
  theme_minimal()

ggplot(data=wiggledepth, aes(x=maxdep, group=Halsey_05m, fill=Halsey_05m)) +
       geom_density(adjust=1.5, position="fill") 

ggplot(data=wiggledepth, aes(x=maxdep, group=Halsey_2m, fill=Halsey_2m)) +
  facet_grid(~Halsey_2m) +
  geom_density(adjust=1.5) +   
  theme(legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()) +
    ylim(0, 0.03)  

# # Convert the data from wide to long format using pivot_longer
# wiggledepth_long <- wiggledepth %>%
#       dplyr::select(maxdep, Halsey_05m, Halsey_2m,  Simeone, Takahashi) %>%
#       pivot_longer(cols = c(Halsey_05m, Halsey_2m,  Simeone, Takahashi), 
#                    names_to = "Outcome", 
#                    values_to = "Count")
# wiggledepth_long

wiggledepth

#---------------------
# More plotting
#---------------------

ggplot(result_df, aes(x = botttim, y = video_pce_IN)) + 
       geom_point()

ggplot(result_df, aes(x = ascspeed, y =  video_pce_IN)) +    
  geom_point()

sort(unique(result_df$asctim)) # accent time is measured in 1 second bins. Too coarse to be useful?
hist(unique(result_df$ascspeed))

#-----------------------------------------------------------------------
# Boxplot: wiggles and video dive types (foraging or non-foraging dive)
#-----------------------------------------------------------------------
# Note: Above, there is an optional filter in:
#         divetype_wiggles <- result_df %>%
#                  dplyr::filter(maxdep > 10) %>%
# This allows you to remove shallow dives.
# Not surprisingly, the wiggles seem to  have better 'predicted' proportions of foraging 
# dives when you exclude shallow dives (the wiggle proportions get closer to the video
# proportion when you remove dives <3m, < 5m, <10m, <20m)

# all dives
divetype_fig = divetype_summary %>%
  filter(dive_type == 'foraging') %>%
  ggplot(aes(x = wiggle_metric, y = proportion, fill = wiggle_metric)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  geom_jitter(color="black", size=0.85, alpha=1, width = 0.1, height = 0) +
  gg_theme()+ 
  theme(legend.position="none",
  plot.title = element_text(size=8)) +
  ggtitle("Proportion of foraging dives predicted by different metrics") +
  xlab("Metric") + 
  ylab("Proportion foraging dives") 

divetype_fig

#---------------------------------------
# Regression: wiggles against video PCE
#---------------------------------------
# fig_halsey_05 = 
#   ggplot(data = result_df,
#               aes(x = video_pce_IN, 
#                   y = wiggleN_Halsey_05m)) + 
#   geom_point(alpha = 0.1, size = 0.95, shape = 21,
#              fill = "aquamarine4", col = "aquamarine4") + 
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#  xlab("Video prey captures") +
#  ylab("No. wiggles")+
#   scale_x_continuous(expand = c(0.02, 0.01)) +
#   scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Halsey_05m), 
#                   label = "Halsey et al. 2007 (0.5m)", size = 4, 
#                   hjust = 1, vjust = 0) +   # Adjust hjust and vjust
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
#         legend.position = "none",
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank())
# 
# fig_halsey_05
# 
# fig_halsey_2 = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Halsey_2m)) + 
#   geom_point(alpha = 0.1, size = 0.95, shape = 21,
#              fill = "aquamarine4", col = "aquamarine4") + 
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_x_continuous(expand = c(0.02, 0.01)) +
#   scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Halsey_2m), 
#            label = "Halsey et al. 2007 (2m)", size = 4, 
#            hjust = 1, vjust = 2) +   # Adjust hjust and vjust
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
#         legend.position = "none",
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank())
# 
#   fig_halsey_2
#   
#   
# fig_simeone = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Simeone)) + 
#   geom_point(alpha = 0.1, size = 0.95, shape = 21,
#              fill = "aquamarine4", col = "aquamarine4") + 
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_y_continuous(expand = c(0.02, 0.01), limits = c(0,20))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Simeone), 
#            label = "Simeone and Wilson 2003 (0.3m)", size = 4, 
#            hjust = 1, vjust = -1) + # Adjust hjust and vjust
#   scale_x_continuous(breaks = seq(0,125, by = 25))
# 
# fig_simeone
# 
# 
# fig_bottdist = result_df %>%
# #  filter(!(is.na(bottdist) | bottdist == 0) & foraging_dive != "non-foraging") %>%
#   ggplot(aes(x = video_pce_IN, 
#                    y = bottdist)) + 
#   geom_point(alpha = 0.1, size = 0.95, shape = 21,
#              fill = "aquamarine4", col = "aquamarine4") + 
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("Bottom distance")+
# #  scale_x_continuous(expand = c(0.02, 0.01)) +
# #  scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$bottdist, na.rm = T), 
#            label = "Bottom distance (diveMove)", size = 4, 
#            hjust = 1, vjust = 1.5) + # Adjust hjust and vjust+
#   scale_x_continuous(breaks = seq(0,125, by = 25))
# 
# fig_bottdist 
# 
# 
# Figure2 =  fig_halsey_05 + fig_halsey_2 + fig_simeone + fig_bottdist + 
#   plot_layout(ncol = 2, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5)))
# 
# Figure2
# 
# # Save as PNG
# ggsave("./plots/Figure2_points.png", Figure2, width = 7, height = 5, dpi = 1500)

#---------------------
# stat_binhex
#---------------------

# result_df %>%
#   filter(video_pce_IN != 0) %>%
#   #  filter(!(is.na(bottdist) | bottdist == 0) & foraging_dive != "non-foraging") %>%
#   ggplot(aes(x = video_pce_IN, 
#             # y = bottdist)) + 
#  # y = wiggleN_Simeone))+
#  y = wiggleN_Halsey_05m)) + 
#   stat_binhex(bins = 50) +
#   scale_fill_viridis_c()
#   #scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 200))

#--------------------
# stat_density_2d
#--------------------

# result_df %>%
#   filter(!(is.na(bottdist) | bottdist == 0) & foraging_dive != "non-foraging") %>%
#   ggplot(aes(x = video_pce_IN, 
#              y = bottdist)) + 
#   gg_theme() +
#   theme(legend.position = "none")+
#   stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#   scale_fill_distiller(palette= "Spectral", direction=1) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme(
#     legend.position='none'
#   )
# 

#----------------------------------------
# Geom_cout of the above figures
#----------------------------------------
# Regression: wiggles against video PCE

# fig_halsey_05 = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Halsey_05m)) + 
#   geom_count(aes(#color = after_stat(n), 
#                  size = after_stat(n))) +
#   guides(color = 'legend')+
#   scale_size(range = c(2, 15))+
#   scale_color_continuous_sequential(palette = "ag_GrnYl", rev = F, alpha = 0.5,
#                                     begin = 0.1, end = 0.9)+
# #  scale_color_continuous(type = "viridis", begin = 0.4, end = 0.9,alpha = 0.4)+
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_x_continuous(breaks = seq(0,125, by = 25))+
#   scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Halsey_05m), 
#            label = "Halsey et al. 2007 (0.5m)", size = 4, 
#            hjust = 1, vjust = -1.3) +
#   theme(
#     axis.text = element_text(colour = "black", size = 14),
#     axis.title = element_text(size=17))
# 
# fig_halsey_05
# 
# fig_halsey_2 = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Halsey_2m)) + 
#   geom_count(aes(#color = after_stat(n),
#                  size = after_stat(n))) +
#   guides(color = 'legend')+
#   scale_size(range = c(2, 15))+
#   scale_color_continuous_sequential(palette = "ag_GrnYl", rev = F, alpha = 0.5,
#                                     begin = 0.1, end = 0.9)+
# #  scale_color_continuous(type = "viridis", begin = 0.4, end = 0.9,alpha = 0.4)+
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_x_continuous(breaks = seq(0,125, by = 25))+
#   scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Halsey_2m), 
#            label = "Halsey et al. 2007 (2m)", size = 4, 
#            hjust = 1, vjust = 1.2) +
#   theme(
#     axis.text = element_text(colour = "black", size = 14),
#     axis.title = element_text(size=17))
# 
# fig_halsey_2
# 
# 
# 
# fig_simeone = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Simeone)) + 
#   geom_count(aes(#color = after_stat(n),
#                  size = after_stat(n))) +
#   guides(color = 'legend')+
#   scale_size(range = c(2, 15))+
#   scale_color_continuous_sequential(palette = "ag_GrnYl", rev = F, alpha = 0.5,
#                                     begin = 0.1, end = 0.9)+
#  # scale_color_continuous(type = "viridis", begin = 0.4, end = 0.9,alpha = 0.4)+
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_x_continuous(breaks = seq(0,125, by = 25))+
#   scale_y_continuous(expand = c(0.02, 0.01), limits = c(0,20))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Simeone), 
#            label = "Simeone and Wilson 2003 (0.3m)", size = 4, 
#            hjust = 1, vjust = -2.3) + # Adjust hjust and vjust
#     # Adjust hjust and vjust
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
#         #  legend.position = c(1, 0),  # Adjust the position of the legend inside the plotting area
#         #   legend.justification = c(1, 0),  # Justify the legend to the top right corner
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank()) +
#   theme(
#     axis.text = element_text(colour = "black", size = 14),
#     axis.title = element_text(size=17))# +
# #   theme(legend.position = c(1, 0),  # Adjust the position of the legend inside the plotting area
# #         legend.justification = c(1, 0))
# 
# 
# fig_simeone
# 
# 
# fig_takahashi = 
#   ggplot(data = result_df,
#          aes(x = video_pce_IN, 
#              y = wiggleN_Takahashi)) + 
#   geom_count(aes(#color = after_stat(n), 
#                  alpha = 0.1, size = after_stat(n))) +
#   guides(color = 'legend')+
#   scale_size(range = c(2, 15))+
#   scale_color_continuous_sequential(palette = "ag_GrnYl", rev = F, alpha = 0.5,
#                                     begin = 0.1, end = 0.9)+
#   # scale_color_continuous(type = "viridis", begin = 0.4, end = 0.9,alpha = 0.4)+
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("No. wiggles")+
#   scale_x_continuous(breaks = seq(0,125, by = 25))+
#   scale_y_continuous(expand = c(0.02, 0.01), limits = c(0,20))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$wiggleN_Takahashi), 
#            label = "Takahashi et al. 2004", size = 4, 
#            hjust = 1, vjust = -0.5)  +   # Adjust hjust and vjust
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
#         #  legend.position = c(1, 0),  # Adjust the position of the legend inside the plotting area
#         #  legend.justification = c(1, 0),  # Justify the legend to the top right corner
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank())+
#   theme(
#     axis.text = element_text(colour = "black", size = 14),
#     axis.title = element_text(size=17))
# # +
# #   theme(legend.position = c(1, 0),  # Adjust the position of the legend inside the plotting area
# #         legend.justification = c(1, 0))
# 
# 
# fig_takahashi
# 
# 
# fig_bottdist = result_df %>%
#    ggplot(aes(x = video_pce_IN, 
#              y = bottdist)) + 
#   geom_count(aes(#color = after_stat(n), 
#                  size = after_stat(n))) +
#   guides(color = 'legend')+
#   scale_size(range = c(2, 15))+
#   scale_color_continuous_sequential(palette = "ag_GrnYl", rev = F, alpha = 0.5,
#                                     begin = 0.1, end = 0.9)+
# #  scale_color_continuous(type = "viridis", begin = 0.4, end = 0.9,alpha = 0.4)+
#   gg_theme() +
#   theme(legend.position = "none")+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
#   geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
#   xlab("Video prey captures") +
#   ylab("Bottom distance")+
#   scale_x_continuous(breaks = seq(0,125, by = 25))+
#   scale_y_continuous(expand = c(0.02, 0.01))+
#   annotate("text", x = max(result_df$video_pce_IN), y = max(result_df$bottdist, na.rm = T), 
#            label = "Bottom distance (diveMove)", size = 4, 
#            hjust = 1, vjust = 1.4)  +
#     theme(
#     axis.text = element_text(colour = "black", size = 14),
#     axis.title = element_text(size=17))
# 
# # +
# #   theme(legend.position = c(1, 0),  # Adjust the position of the legend inside the plotting area
# #         legend.justification = c(1, 0))
# 
# fig_bottdist 
# 
# 
# Figure2 =  fig_simeone + fig_takahashi + fig_halsey_05 + fig_halsey_2 +  
#   plot_layout(ncol = 2, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5)))
# 
# #Figure2
# 
# # Save as PNG
# ggsave("./plots/Figure2_geom_count.png", Figure2, width = 20, height = 15, dpi = 1500)
# 
# ggsave("./plots/Figure3_bottdist_diveMove.png", fig_bottdist , width = 5.7, height = 4.85, dpi = 1500)
# 

#-------------------------------------------
# Plot video and wiggles against each other 
#-------------------------------------------

#https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/

get_Density <- function(x, y, ...) {
  Density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, Density_out$x)
  int_y <- findInterval(y, Density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(Density_out$z[comb_int])
}

Density_map_bottdist <- result_df %>% 
  select(video_pce_IN, bottdist) %>% 
  mutate(Density = get_Density(video_pce_IN, bottdist, n = 100))

Density_map_wiggleN_Halsey_05m <-result_df %>% 
  select(video_pce_IN, wiggleN_Halsey_05m) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Halsey_05m, n = 100))

Density_map_wiggleN_Halsey_2m <-result_df %>% 
  select(video_pce_IN, wiggleN_Halsey_2m) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Halsey_2m, n = 100))

Density_map_wiggleN_Simeone <-result_df %>% 
  select(video_pce_IN, wiggleN_Simeone) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Simeone, n = 100))

Density_map_wiggleN_Takahashi <-result_df %>% 
  select(video_pce_IN, wiggleN_Takahashi) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Takahashi, n = 100))


fig2_bottdist = Density_map_bottdist %>% 
  ggplot(aes(x =  video_pce_IN , y = bottdist , color = Density)) +
  geom_point(alpha = 0.3) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1) +
  gg_theme() +
  theme(
    legend.position = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right") +       # Align legend to the right of the box
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Video prey captures") +
  ylab("Bottom distance") +
  scale_x_continuous(breaks = seq(0,125, by = 25))+
  scale_y_continuous(limits = c(0, 60)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Bottom distance (diveMove)", size = 4, 
           hjust = 1.1, vjust = 1.6)
fig2_bottdist


fig2_Simeone = Density_map_wiggleN_Simeone %>% 
  ggplot(aes(x =  video_pce_IN , y = wiggleN_Simeone , color = Density)) +
 #geom_point(alpha = 0.3) +
 geom_jitter(alpha = 0.3, width = 0, height = 0.1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right"       # Align legend to the right of the box
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Video prey captures") +
  ylab("No. wiggles") +
  scale_x_continuous(breaks = seq(0,125, by = 25))+
  scale_y_continuous(limits = c(0, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Simeone and Wilson 2003 (0.3m)", size = 4,  
           hjust = 1.1, vjust = 1.6)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) 

fig2_Simeone

fig2_Takahashi = Density_map_wiggleN_Takahashi %>% 
  ggplot(aes(x =  video_pce_IN , y = wiggleN_Takahashi , color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0, height = 0.1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right")+       # Align legend to the right of the box
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Video prey captures") +
  ylab("No. wiggles") +
  scale_x_continuous(breaks = seq(0,125, by = 25))+
  scale_y_continuous(limits = c(0, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Takahashi et al. 2004", size = 4, 
           hjust = 1.1, vjust = 1.6)+
  # Adjust hjust and vjust
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) 
fig2_Takahashi


fig2_Halsey_05m = Density_map_wiggleN_Halsey_05m %>% 
  ggplot(aes(x =  video_pce_IN , y = wiggleN_Halsey_05m , color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0, height = 0.1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right") +       # Align legend to the right of the box
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Video prey captures") +
  ylab("No. wiggles") +
  scale_x_continuous(breaks = seq(0,125, by = 25))+
  scale_y_continuous(limits = c(0, 14)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Halsey et al. 2007 (0.5m)", size = 4, 
           hjust = 1.1, vjust = 1.6)
fig2_Halsey_05m

fig2_Halsey_2m = Density_map_wiggleN_Halsey_2m %>% 
  ggplot(aes(x =  video_pce_IN , y = wiggleN_Halsey_2m , color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0, height = 0.1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right") +      # Align legend to the right of the box
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Video prey captures") +
  ylab("No. wiggles") +
  scale_x_continuous(breaks = seq(0,125, by = 25))+
  scale_y_continuous(limits = c(0, 8)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Halsey et al. 2007 (2m)", size = 4, 
           hjust = 1.1, vjust = 1.6)
fig2_Halsey_2m


figure2 =  fig2_Simeone + fig2_Takahashi + 
  fig2_Halsey_05m + fig2_Halsey_2m + 
  plot_layout(ncol = 2, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5))) 
figure2

# Save as PNG
ggsave("./plots/Figure2_points_density.png", figure2, width = 10, height = 8, dpi = 1500)

# Save as PNG
ggsave("./plots/Figure3_bottist.png", fig2_bottdist, width = 5.7, height = 4.85, dpi = 1500)


#-----------------------------------------------------------
# Save zoomed out prey captures plots for ALL individuals (loop)
#-----------------------------------------------------------

all_individuals = unique(divedat$id)

# Define your own colors for different dive phases (diveMove)
my_colors <- c("D" = "slateblue", 
               "DB" = "slateblue",
               "B" = "darkgreen",
               "BA" = "darkgreen", 
               "A" = "cadetblue",
               "DA" = "cadetblue",
               "X" = "azure3")

# example_animal = "2022_01_07_AC2002_HPM11"

for (example_animal in all_individuals) {
 
dive_timeseries = divedat %>% 
            dplyr::filter(id == example_animal) %>%
            dplyr::filter(!is.na(vid_time)) %>% 
            arrange(date.time) %>%
            slice(20:(n()-20))
  
# remove the first and last 19/20 rows (this should get rid of the 'initial video', and 
# perhaps an inconsequential amount of 'dive' data

#---------------------------
# Plot video prey captures
#---------------------------

Fig4A = ggplot(data = dive_timeseries, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data=subset(dive_timeseries, PCE_1hz > 0),
             aes(x = date.time, y = depth*-1), 
             fill = "red", col = "azure", shape = 21, size = 2) +
  gg_theme() + 
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "1 hour")+
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_timeseries$date.time), 
           y = min(dive_timeseries$depth*-1),
           label = "Prey capture (video)", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_timeseries$date.time)-220, 
           y = min(dive_timeseries$depth*-1),
           size = 2.5, 
           fill = "red", col = "azure", shape = 21)+
  theme(legend.position = "none") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())


#Fig4A

#-------------------------------------
# Plot Halsey 2m wiggle prey captures
#-------------------------------------

x = subdat_Halsey_2m %>% 
  dplyr::filter(id == example_animal &
                  wiggle ==  1 &
                   date.time >= min(dive_timeseries$date.time) &
                   date.time <= max(dive_timeseries$date.time))

Fig4B = ggplot(data = dive_timeseries, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = x, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "azure", shape = 21, alpha = 1, size  = 2) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "30 min")+
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_timeseries$date.time), 
           y = min(dive_timeseries$depth*-1), label = "Wiggle: Halsey et al. 2007 (2m)", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_timeseries$date.time)-220, 
           y = min(dive_timeseries$depth*-1), size = 2.5, 
           fill = "red", col = "azure", shape = 21)+
  theme(legend.position = "none") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())


#-----------------------------------------------
# Plot Simeone and Wilson wiggle prey captures
#-----------------------------------------------
y = subdat_Simeone %>% 
  dplyr::filter(id == example_animal &
                  undulations ==  1 &
                  date.time >= min(dive_timeseries$date.time) &
                  date.time <= max(dive_timeseries$date.time))

Fig4C = ggplot(data = dive_timeseries, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = y, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "azure", shape = 21, alpha = 1, size  = 2) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "30 min")+
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_timeseries$date.time),  y = min(dive_timeseries$depth*-1),
           label = "Wiggle: Simeone and Wilson 2003 (0.3m)", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_timeseries$date.time)-220,  y = min(dive_timeseries$depth*-1),
           size = 2.5, 
           fill = "red", col = "azure", shape = 21)+
  theme(legend.position = "none") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())


#-----------------------------------------------
# Plot Halsey 0.5m wiggle prey captures
#-----------------------------------------------

z = subdat_Halsey_05m %>% 
  dplyr::filter(id == example_animal &
                  wiggle ==  1 &
                  date.time >= min(dive_timeseries$date.time) &
                  date.time <= max(dive_timeseries$date.time))

Fig4D = ggplot(data = dive_timeseries, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = z, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "azure", shape = 21, alpha = 1, size  = 2) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "30 min")+
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_timeseries$date.time),  y = min(dive_timeseries$depth*-1),
           label = "Wiggle: Halsey et al. 2007 (0.5m)", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_timeseries$date.time)-220,  y = min(dive_timeseries$depth*-1),
           size = 2.5, 
           fill = "red", col = "azure", shape = 21)+
  theme(legend.position = "none") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())



#-----------------------------------------------
# Plot Takahashi wiggle prey captures
#-----------------------------------------------

w = subdat_Takahashi %>% 
  dplyr::filter(id == example_animal &
                  wiggles_Takahashi ==  1 &
                  date.time >= min(dive_timeseries$date.time) &
                  date.time <= max(dive_timeseries$date.time))

Fig4E = ggplot(data = dive_timeseries, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = w, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "azure", shape = 21, alpha = 1, size  = 2) +
  scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "30 min")+
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_timeseries$date.time),  y = min(dive_timeseries$depth*-1),
           label = "Wiggle: Takahashi et al. 2004", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_timeseries$date.time)-220,  y = min(dive_timeseries$depth*-1),
           size = 2.5, 
           fill = "red", col = "azure", shape = 21)+
  theme(legend.position = "none") 


video_vs_wiggles = Fig4A / Fig4D / Fig4C / Fig4E

# Save as PNG
ggsave(paste0("./plots/dives_and_PCE/", example_animal, "_video_wiggles.png"), 
       video_vs_wiggles, width = 5, height = 10, dpi = 400)

 
TDR = dive_timeseries %>% 
      dplyr::select(date.time, depth, PCE_1hz) %>%
      mutate(depth = depth * -1)

saveRDS(TDR, paste0("./output/TDR_", example_animal, ".rds") )


} 


#--------------------
# Prey captures plot 
#--------------------

# SUBSET 1
example_animal = "2022_01_10_AC2101_DI02"
#subset data
dive_example = divedat %>%
  dplyr::filter(id == example_animal) %>%
  dplyr::filter(!is.na(vid_time)) %>%
  arrange(date.time) %>%
  dplyr::filter(date.time > '2022-01-10 18:12:00') %>%
  dplyr::filter(date.time < '2022-01-10 18:25:30')

## SUBSET 2
# example_animal = "2023_01_14_DI4"
# dive_example = divedat %>% 
#   dplyr::filter(id == example_animal) %>%
#   dplyr::filter(!is.na(vid_time)) %>% 
#   arrange(date.time) %>%
#   # dplyr::filter(date.time > '2023-01-14 19:50:00') %>% 
#   # dplyr::filter(date.time < '2023-01-14 20:04:10') 
#    dplyr::filter(date.time > '2023-01-14 19:54:00') %>% 
#    dplyr::filter(date.time < '2023-01-14 20:04:10') 
  

#SUBSET 3
# example_animal = "2022_01_07_AC2104_HPM03"
# #subset data
# dive_example = divedat %>%
#   dplyr::filter(id == example_animal) %>%
#   dplyr::filter(!is.na(vid_time)) %>%
#   arrange(date.time) %>%
#   dplyr::filter(date.time > '2022-01-07 16:19:00') %>%
#   dplyr::filter(date.time < '2022-01-07 16:25:30')

# plot video data (PCE)

Fig5A = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data=subset(dive_example, PCE_1hz > 0),
             aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, size = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  
  annotate("text", x = max(dive_example$date.time)-370,  y = -16, label = "Video prey captures", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("point", x = max(dive_example$date.time)-390,  y = -15.7, size = 2.5, 
           fill = "red", col = "navy", shape = 21)  +
  theme(legend.position = "none") +
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.title = element_text(size=15))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
    theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

# plot Halsey_05m (wiggles)
x = subdat_Halsey_05m %>% 
  dplyr::filter(id == example_animal &
                  wiggle ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

Fig5B = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = x, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = max(dive_example$date.time)-450,  y = -16, label = "Wiggle:", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("text", x = max(dive_example$date.time)-450,  y = -18, label = "Halsey et al. 2007 (0.5m)", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("point", x = max(dive_example$date.time)-470,  y = -17.7, size = 2.5, 
           fill = "red", col = "navy", shape = 21)+
  theme(legend.position = "none") +
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.title = element_text(size=15)) + 
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )

# plot Simeone (wiggles)
y = subdat_Simeone %>% 
  dplyr::filter(id == example_animal &
                  undulations ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

Fig5C = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = y, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = max(dive_example$date.time)-395,  y = -16, label = "Wiggle:", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("text", x = max(dive_example$date.time)-395,  y = -18, label = "Simeone and Wilson 2003 (0.3m)", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("point", x = max(dive_example$date.time)-415,  y = -17.7, size = 2.5, 
           fill = "red", col = "navy", shape = 21)+
  theme(legend.position = "none") +
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.title = element_text(size=15))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

z = subdat_Takahashi %>% 
  dplyr::filter(id == example_animal &
                  wiggles_Takahashi ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

# plot Takahashi (wiggles)
Fig5D = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = z, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = max(dive_example$date.time)-240,  y = -16, label = "Wiggle:", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("text", x = max(dive_example$date.time)-240,  y = -18, label = "Takahashi et al. 2004", 
           size = 5, color = "black", hjust = 0, vjust = 0)+
  annotate("point", x = max(dive_example$date.time)-260,  y = -17.7, size = 2.5, 
           fill = "red", col = "navy", shape = 21)+
  theme(legend.position = "none") +
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.title = element_text(size=15))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

Figure5_example  = Fig5A / Fig5C / Fig5D / Fig5B 
Figure5_example

# Save as PNG
ggsave("./plots/Figure5_example.png", Figure5_example, width = 8, height = 9, dpi = 1000)

# Save as PNG
#ggsave("./plots/Figure5.png", Figure5, width =5, height = 6, dpi = 2000)
# Save as PNG
#ggsave("./plots/Fig5A.png", Fig5A, width = 5, height = 3, dpi = 1000)
# Save as PNG
#ggsave("./plots/Fig5B.png", Fig5B, width = 5, height = 3, dpi = 1000)



#-------------------------
# Catch per unit effort 
#-------------------------

# Attempts of catch per unit effort (ACPUE) 
# ACPUE = # Total number of wiggles / Total time in bottom duration (Riaz et al. 2020, 2021)

# CPUE is very variable on the 'dive' scale. Riaz et al. summed CPUE across dives (per hour)
# Here, I sum CPUE across dive bouts where there is at least 5 dives in the bout. Cut out
# bouts with less dives as these are again highly variable. 

# Test to see that you are summing the PCE and bottom time correctly.

# tst = result_df %>% 
#   select(id, bout, video_pce_IN, botttim) %>%
#   group_by(id, bout) %>%
#   mutate(CPUE_video = sum(video_pce_IN) / sum(botttim))
# 
# tst %>% 
#   filter(id == '2022_01_16_AC2113_HPM03' & 
#          bout == 4) 

# No calcualte the sum of PCE and botttim across IDs and bouts:
result_df = result_df %>% 
            group_by(id, bout) %>%
            mutate(CPUE_video = sum(video_pce_IN) / sum(botttim),
            ACPUE_Halsey_05m = sum(wiggleN_Halsey_05m) / sum(botttim),
            ACPUE_Halsey_2m = sum(wiggleN_Halsey_2m) / sum(botttim),
            ACPUE_Simeone = sum(wiggleN_Simeone) / sum(botttim),
            ACPUE_Takahashi = sum(wiggleN_Takahashi) / sum(botttim),
            ACPUE_bottdist = sum(bottdist) / sum(botttim))

# result_df %>% ggplot + 
#   geom_point(aes(x = CPUE_video, y = ACPUE_Halsey_05m),
#        alpha = 0.3, size = 0.95, shape = 21, fill = "navy", col = "black") + 
#   geom_point(aes(x = CPUE_video, y = ACPUE_Halsey_2m),
#        alpha = 0.3, size = 0.95, shape = 21, fill = "red", col = "black")+  
#   geom_point(aes(x = CPUE_video, y = ACPUE_Simeone),
#        alpha = 0.3, size = 0.95, shape = 21, fill = "darkgreen", col = "black")+  
#   geom_point(aes(x = CPUE_video, y = ACPUE_Takahashi),
#              alpha = 0.3, size = 0.95, shape = 21, fill = "black", col = "black")+  
#   geom_point(aes(x = CPUE_video, y = ACPUE_bottdist),
#              alpha = 0.3, size = 0.95, shape = 21, fill = "grey", col = "black")+
#   gg_theme() +
#   theme(legend.position = "none") +
#   xlab("CPUE_video") +
#   ylab("ACPUE")+
#   scale_x_continuous(expand = c(0.02, 0.01), limits = c(0,7)) +
#   scale_y_continuous(expand = c(0.02, 0.01), limits = c(0,0.52))

#========================
# CPUE boxplots
#========================
cpue =  result_df %>%
  select(id, bout, year, CPUE_video,
         ACPUE_bottdist, ACPUE_Halsey_05m, ACPUE_Halsey_2m, ACPUE_Simeone, ACPUE_Takahashi) %>%
  group_by(id, bout) %>% 
  mutate(diveN_per_bout = n()) %>%
  filter(diveN_per_bout > 4) %>% 
  pivot_longer(cols = starts_with(c("CPUE", "ACPUE")),
               names_to = "CPUE_metric",
               values_to = "CPUE_bout") 

# Is is difficult to compare, directly, bottom distance.
# Consedered scaling the values, but not sure that this is useful or correct. 

# Function to standardize a vector
standardize <- function(x) {
    (x - min(x)) / (max(x) - min(x))}

cpue = cpue %>% 
  group_by(CPUE_metric) %>%
  mutate(CPUE_bout.std = standardize(as.numeric(CPUE_bout)))

cpue %>%
  group_by(CPUE_metric) %>%
  summarise(mn = mean(CPUE_bout.std),
            sdev = sd(CPUE_bout.std))

cpue %>%
  ggplot(aes(x = as.factor(CPUE_metric), y = CPUE_bout.std)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4", alpha = 0.5) + 
  gg_theme() +
  xlab("CPUE metric") + 
  ylab("STD sum(catch per unit effort) / bout") +
  theme(axis.text = element_text(colour = "black", size = 11),
        axis.title = element_text(size=11)) 

cpue %>%
  filter(CPUE_metric != 'ACPUE_bottdist') %>%
  ggplot(aes(x = as.factor(CPUE_metric), y =CPUE_bout)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4", alpha = 0.5) + 
  gg_theme() +
  xlab("CPUE metric") + 
  ylab("STD sum(catch per unit effort) / bout") +
  theme(axis.text = element_text(colour = "black", size = 11),
        axis.title = element_text(size=11)) 


# # 2022
# result_df %>%
#  filter( begdesc < "2022-03-07 18:06:40") %>%
#   ggplot(aes(x =  begdesc, y = maxdep * -1)) + 
#   geom_point(shape = ".")+
#   gg_theme()
#   
# # 2023
# result_df %>%
#   filter( begdesc > "2022-03-07 18:06:40") %>%
#   ggplot(aes(x =  begdesc, y = maxdep * -1)) + 
#   geom_point(shape = ".")+
#   gg_theme()
# 

#------------------------------------------------
# Can we see differences between 2022 and 2023?
#------------------------------------------------
# in video, and in wiggles...

# Attempts of catch per unit effort (ACPUE) 
# ACPUE = # Total number of wiggles / Total time in bottom duration (Riaz et al. 2020, 2021)
head(result_df)


cpue_bottdist = 
  result_df %>%
  subset(maxdep > 3) %>%
  group_by(year) %>%
  select(year, ACPUE_bottdist) %>%
  drop_na() %>%
  #summarise(mean_CPUE = mean(ACPUE_bottdist)) %>%
  ggplot(aes(x = as.factor(year), y = ACPUE_bottdist)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4", alpha = 0.7) + 
  gg_theme() +
  xlab("Year") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(size=15)) + 
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
  #     axis.title.x=element_blank(),
  #     axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x = 2.5, y = 0.1, 
           label = "CPUE bottom distance (diveMove)", size = 4.5, 
           hjust = 1, vjust = 0)    # Adjust hjust and vjust

cpue_bottdist 


cpue_bottdist = 
result_df %>%
  subset(maxdep > 3) %>%
  group_by(id, year) %>%
  select(year, ACPUE_bottdist) %>%
  drop_na() %>%
  summarise(mean_CPUE = mean(ACPUE_bottdist)) %>%
  ggplot(aes(x = as.factor(year), y = mean_CPUE)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4" ) + 
  gg_theme() +
  xlab("Year") + 
  ylab("Catch per unit effort") +
    theme(axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(size=15)) + 
    # theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
    #     axis.title.x=element_blank(),
    #     axis.text.x=element_blank()) +
   theme(axis.text.y=element_blank()) +
    annotate("text", x = 2.5, y = 0.1, 
           label = "CPUE bottom distance (diveMove)", size = 4.5, 
           hjust = 1, vjust = 0)    # Adjust hjust and vjust

cpue_bottdist 



cpue_CPUE_video = 
  result_df %>%
  subset(maxdep > 3) %>%
  group_by(id, year) %>%
  select(year, CPUE_video) %>%
  drop_na() %>%
  summarise(mean_CPUE = mean(CPUE_video)) %>%
  ggplot(aes(x = as.factor(year), y = mean_CPUE)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4" ) + 
  gg_theme() +
  xlab("Year") + 
  ylab("Catch per unit effort") +
  ylim(0, 0.75) + 
  theme(axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(size=15)) + 
   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
       axis.title.x=element_blank(),
       axis.text.x=element_blank()) + 
  theme(axis.text.y=element_blank()) +
  annotate("text", x = 2.5, y = 0.7, 
           label = "CPUE video prey captures", size = 4.5, 
           hjust = 1, vjust = 0)    # Adjust hjust and vjust

cpue_CPUE_video


names(result_df)


cpue_ACPUE_Halsey_05m = 
  result_df %>%
  subset(maxdep > 3) %>%
  group_by(id, year) %>%
  select(year, ACPUE_Halsey_05m) %>%
  drop_na() %>%
  summarise(mean_CPUE = mean(ACPUE_Halsey_05m)) %>%
  ggplot(aes(x = as.factor(year), y = mean_CPUE)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4" ) + 
  gg_theme() +
  xlab("Year") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(size=15)) + 
   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
       axis.title.x=element_blank(),
       axis.text.x=element_blank()) + 
  theme(axis.text.y=element_blank()) +
  annotate("text", x = 2.5, y = 0.01, 
           label = "CPUE Halsey et al. 2007 (0.5m)", size = 4.5, 
           hjust = 1, vjust = 0)    # Adjust hjust and vjust

cpue_ACPUE_Halsey_05m


cpue_fig = cpue_CPUE_video / cpue_ACPUE_Halsey_05m / cpue_bottdist 
cpue_fig

# Save as PNG
ggsave("./plots/cpue_figs.png", cpue_fig, width = 4, height = 7, dpi = 1500)



# why in subdat is it 2020? Well, that would contain all 'dive' PCE and also surface PCE?
# remove outside dive X PCE and you get what is in wiggle summary
# check whether DARK and LIGHT is INCLUDED or EXCLUDED! Split in the table.
# dark might be filtered out here.
# subdat_Halsey_05m %>%
#   group_by(id) %>%
#   filter(InDive != "X") %>%
#   summarize(PCE = sum(PCE_1hz))


# DATA FOR GITHUB

sub = dive_timeseries %>% 
      select(date.time, depth, PCE_1hz)
head(sub)

ggplot(sub, aes(date.time, depth*-1, color = as.factor(PCE_1hz))) +
   geom_point()





#--------------------
# Prey captures plot
#--------------------

dive_example = dplyr::filter(divedat, id == "2023_01_14_DI4")
dive_example = dive_example[15850:17400,]

# Define your own colors
my_colors <- c("D" = "slateblue", 
               "DB" = "slateblue",
               "B" = "darkgreen",
               "BA" = "darkgreen", 
               "A" = "cadetblue",
               "DA" = "cadetblue",
               "X" = "azure3")

Fig3A = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data=subset(dive_example, PCE_1hz > 0),
             aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, size = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  
  annotate("text", x = max(dive_example$date.time)-100, y = - 95, label = "Prey capture", 
           size = 3, color = "black")+
  annotate("point", x = max(dive_example$date.time)-220, y = - 95,size = 2.5, 
           fill = "red", col = "navy", shape = 21)

Fig3A 

x = subdat_Halsey_2m %>% 
  dplyr::filter(id == "2023_01_14_DI4" &
                  wiggle ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

Fig3B = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = x, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = max(dive_example$date.time)-100, y = - 95, label = "Wiggle Halsey", 
           size = 3, color = "black")+
  annotate("point", x = max(dive_example$date.time)-220, y = - 95,size = 2.5, 
           fill = "red", col = "navy", shape = 21)

Fig3B

Fig3A / Fig3B

#--------------------
# Prey captures plot 2
#--------------------
#-----------------------------------------------------
# This is an example where the wiggles are PCE occur.
#-----------------------------------------------------
# dive_example2 = 
#   dplyr::filter(divedat, id == example_animal)

# dive_example2 = dive_example2 %>% 
#   dplyr::filter(date.time > "2023-01-14 14:29:00" & 
#                   date.time <  "2023-01-14 15:00:00")

# dive_example2 = 
#   dplyr::filter(divedat, id == "2023_01_14_DI2")
# 
# dive_example2 = dive_example2 %>% 
#   dplyr::filter(date.time > "2023-01-14 16:00:00" & 
#                   date.time < "2023-01-14 17:00:00")


example_animal = "2023_01_14_DI2"

dive_example = divedat %>% 
  dplyr::filter(id == example_animal) %>%
  dplyr::filter(!is.na(vid_time)) %>% 
  arrange(date.time) %>%
  dplyr::filter(date.time > '2023-01-14 16:08:00') %>% 
  dplyr::filter(date.time < '2023-01-14 16:48:00') 

Fig3A = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data=subset(dive_example, PCE_1hz > 0),
             aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, size = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  
  annotate("text", x = min(dive_example$date.time)-100,  y = min(dive_example$depth*-1), label = "Prey capture", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_example$date.time)-220,  y = min(dive_example$depth*-1), size = 2.5, 
           fill = "red", col = "navy", shape = 21)

x = subdat_Halsey_2m %>% 
  dplyr::filter(id == example_animal &
                  wiggle ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

Fig3B = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.2, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = x, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
  annotate("text", x = min(dive_example$date.time)-100,  y = min(dive_example$depth*-1), label = "Wiggle Halsey", 
           size = 3, color = "black", hjust = 0)+
  annotate("point", x = min(dive_example$date.time)-220,  y = min(dive_example$depth*-1), size = 2.5, 
           fill = "red", col = "navy", shape = 21)

Fig3A / Fig3B


