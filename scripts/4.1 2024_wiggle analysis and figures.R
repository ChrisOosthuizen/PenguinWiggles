
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# April 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# Analysis of wiggle metrics 

#-----------------------------------
# Setup
#-----------------------------------

# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(patchwork)
library(caret)
library(RColorBrewer)

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

#----------------------------------
# Read the wiggle analysis output
#----------------------------------
result_all = readRDS("./output/result_all_3m.rds") 
subdat_Halsey_1m = readRDS("./output/subdat_Halsey_1m_3m.rds")
subdat_Halsey_2m = readRDS("./output/subdat_Halsey_2m_3m.rds")
subdat_Simeone = readRDS("./output/subdat_Simeone_3m.rds")
subdat_Takahashi = readRDS("./output/subdat_Takahashi_3m.rds")

length(unique(result_all$id))

# make a 'general' subdat from any of the above:
nrow(subdat_Halsey_1m)
nrow(subdat_Halsey_2m)
nrow(subdat_Simeone)
nrow(subdat_Takahashi)

subdat_r = subdat_Takahashi %>%
           dplyr::select(-wig_depth, -wiggles_Takahashi)
head(subdat_r)

#-----------------------------------------------------------------------
#  Figure 2 - plot time series of dives and video observed prey captures
#-----------------------------------------------------------------------

# Density function 
# https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
get_Density <- function(x, y, ...) {
  Density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, Density_out$x)
  int_y <- findInterval(y, Density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(Density_out$z[comb_int])
}


# To plot VIDEO captures, take ANY of the subdat (they are the same dimensions)
pcedat = subdat_r %>% 
  # clean up   
  filter(light.level == 'light'&
           video.level == 'video') %>%
  mutate(year = year(date.time)) %>%
  # Create the new column with day and month
  mutate(day_month = format(date.time, "%d %b"))

head(pcedat)
table(pcedat$InDive)

# plot simple graph of all dive data
# pcedat %>% 
#   arrange(date.time) %>%
#   ggplot(aes(x = seq(1:length(depth)), y = depth*-1, color = as.factor(PCE_1hz))) +
#   geom_point(alpha = 0.2, shape = ".")


# create x variable to plot (simple sequence; works much better than 'date')
pcedat = pcedat %>% 
  arrange(date.time) %>%
  # group_by(year) %>%
  mutate(xseq = row_number()) %>%
  ungroup()

pcedat %>%
  group_by(year) %>%
  summarize(minx = min(xseq),
            maxx = max(xseq),
            mind = min(day_month),
            maxd = max(day_month)) 

# create data frame to plot PCE 
pcecapt = pcedat %>% 
  filter(PCE_1hz > 0)

# Duplicate rows based on the PCE_1hz column
# So, if PCE_1hz = 2, then it creates an extra PCE_1hz row
pcecapt_dup <- pcecapt[rep(row.names(pcecapt), pcecapt$PCE_1hz), ]

# should be the same 
sum(pcecapt$PCE_1hz)
nrow(pcecapt_dup)

density_map <- pcedat %>% 
   dplyr::select(xseq, depth, year, day_month) %>% 
  mutate(Density = get_Density(xseq, depth, n = 100))

x.scale = c(1, 25000, 50000, 75000, 92000, 102000, 112000, 122000, 132000)

x.dates = pcedat %>% 
  filter(xseq == 1 |       xseq == 25000 |
           xseq == 50000 | xseq == 75000|
           xseq == 92000|  xseq == 102000|
           xseq == 112000| xseq == 122000|
           xseq == 132000) %>%
  distinct(date.time, .keep_all = TRUE) %>% 
  dplyr::select(day_month, xseq, date.time)
x.scale
x.dates

dive_fig = ggplot() + 
  # plot depth heatplot
  geom_point(data = density_map, aes(x =xseq, y = depth*-1, color = Density),
             alpha = 1, shape = 16, size = 0.5) +
  facet_wrap(~year, scales = "free_x") + 
  scale_color_viridis_c(option = 'viridis', end = 1, begin = 0.2) +
  gg_theme() + 
  # add prey captures
  geom_jitter(data = pcecapt_dup, aes(x =xseq, y = depth*-1), color = "red", size = 0.3, shape = 16) + 
  # make plot pretty
  xlab("Chronological order of dives") + 
  ylab("Depth (m)") +
  scale_x_continuous(breaks = x.scale,  
                     labels = x.dates$day_month) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "inside", legend.position.inside =c(0.92, 0.22))+
  theme(plot.margin = margin(b = 20)) + 
  theme(axis.title.x = element_text(vjust=-1.8))

# Create a data frame for annotation
annotation_text <- data.frame(year = 2022, x = 1870, y = -130, label = "Prey captures")
annotation_point <- data.frame(year = 2022, x = 150, y = -130)

# Add annotation to a specific facet
dive_fig = dive_fig + 
  geom_text(data = annotation_text %>% filter(year == 2022), aes(x=x,y=y,label = label), hjust = 0)+
  geom_point(data = annotation_point %>% filter(year == 2022), aes(x=x,y=y),shape = 16, color = "red")

dive_fig 

# Save the ggplot as a TIFF file
ggsave("./figures/Figure2.tiff", plot = dive_fig, width = 8, height = 5, units = "in", dpi = 500)

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
    wiggle_sum_Halsey_1m = sum(wiggleN_Halsey_1m),
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

#----------------------------------------------
# Calculate and plot mean maxdep for each id
# ALL DIVES! (light and dark / video and no video)
#----------------------------------------------

# pull(id) is required to plot below (remove if you want a table with output)
mean_maxdep_all = result_all %>%
  group_by(id) %>%
  summarize(year = mean(year),
            mean_maxdep = mean(maxdep, na.rm = TRUE), 
            max_maxdep = max(maxdep, na.rm = TRUE)) %>%
  arrange(desc(year), desc(mean_maxdep)) %>%
  pull(id) 

# order IDs according to mean_maxdep
result_all$id <- factor(result_all$id, levels = mean_maxdep_all)

maxdep_id_all = result_all %>% 
  filter(video.level == 'video') %>%                                   # REMOVE NON-
  ggplot() + aes(x = maxdep, y = id, fill = after_stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Depth (m)", option = "C") +
  theme_bw() + 
  #  theme(axis.text.y = element_blank(),
  #        axis.ticks.y = element_blank()) + 
  geom_hline(yintercept = 13.5, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 150, by = 25)) + 
  scale_y_discrete(labels = rev(seq_along(levels(result_all$id))))+
  ylab("Individual") + 
  xlab("Maximum depth (m)") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),  # Adjust legend position
        legend.background = element_blank()) + # Remove legend background  # Adjust legend position
  annotate("text", x = 140, y = 16, label = "2022", color = "black")+  # Add vertical text outside plot
  annotate("text", x = 140, y = 11, label = "2023", color = "black")  # Add vertical text outside plot

#maxdep_id_all

# Save as PNG
#ggsave("./plots/maxdep_id_ALLDIVES.png", maxdep_id, width = 6, height = 6, dpi = 1200)


#--------------------------------------------------------------
# Work only with light dives, where there are video.
#--------------------------------------------------------------

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

# add columns to order data by mean maxdepth later on:
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
  geom_hline(yintercept = 13.5, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 150, by = 25)) + 
  scale_y_discrete(labels = rev(seq_along(levels(result_df$id))))+
  ylab("Individual") + 
  xlab("Maximum depth (m)") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),  # Adjust legend position
        legend.background = element_blank()) + # Remove legend background  # Adjust legend position
    annotate("text", x = 140, y = 16, label = "2022", color = "black")+  # Add vertical text outside plot
    annotate("text", x = 140, y = 11, label = "2023", color = "black")  # Add vertical text outside plot

#maxdep_id

# Save as PNG
#ggsave("./plots/maxdep_id.png", maxdep_id, width = 6, height = 6, dpi = 1200)

Supp_fig21 = cowplot::plot_grid(maxdep_id_all, maxdep_id , ncol = 2, labels = c("A", "B"),
                   label_size = 12, label_x = 0, label_y = 1,
                   align = "h", axis = "tblr")
Supp_fig21 

# Save as PNG
ggsave("./supplement/FigureS2_1.png", Supp_fig21, width = 8, height = 8, dpi = 1200)


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
  
# 10 =   how many minutes for a new bout to start?
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
    wiggle_sum_Halsey_1m = sum(wiggleN_Halsey_1m),
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
    dplyr::select(id, Year, ID, 
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
           dplyr::select(id, Year, ID, Date, Duration, total_diveN_annotated, mean_maxdep,
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

#--Supplement ------------------------------------------------------------------------------------------
# Create a foraging_dive column based on video observations (is this a foraging dive?)
 #result_df = result_df %>%
#             mutate(foraging_dive = ifelse(maxdep >= 10 & video_pce_IN >=5, "foraging", "non-foraging"))

# tst = result_df %>%
# 
# view(tst)

# saveRDS(result_df, "./output/supplement_result_df_strict.rds")

#---------------------------------------------------------------------------------------------

# Create a foraging_bout column based on video observations (is this a foraging bout?)
# Here, a foraging bout is if more than 40 % of the dives in the bout are foraging dives
# CO: double checked outside R that this performs as intended;
# should check the 'biological importance' of a limit such as 40 %
result_df = result_df %>%
   group_by(id, bout) %>%
   mutate(foraging_bout = ifelse(mean(foraging_dive == "foraging") < 0.4, "non-foraging", "foraging")) %>%
   ungroup()


# Calculate how many foraging and non-foraging dives there are (video observations)
divetypeN_video <- result_df %>%  
    group_by(id) %>%
    summarise(foraging_video = sum(foraging_dive == "foraging"),
              nonforaging_video = sum(foraging_dive == "non-foraging"),
              total_dives = sum(foraging_video + nonforaging_video))

divetypeN_video
sum(divetypeN_video$foraging_video)
sum(divetypeN_video$nonforaging_video)
sum(divetypeN_video$total_dives)
        
# combine with table Summary

tableS1 = merge(tableS1,divetypeN_video, by = "id")
  
tableS1 =  tableS1 %>% 
           dplyr::select(-id, - total_dives) %>%
           arrange(ID)

write.table(tableS1, './supplement/TableS1.csv', sep = ",", row.names = F)

sum(tableS1$foraging_video)
sum(tableS1$nonforaging_video)
sum(tableS1$total_diveN_annotated)

#-----------------------------------------------------------------------------------------
# Calculate how many foraging and non-foraging dives were predicted by each wiggle analysis
#-----------------------------------------------------------------------------------------

divetype_wiggles <- result_df %>%
     #     dplyr::filter(maxdep > 10) %>%
          group_by(id) %>%
          mutate(
            pred_wiggleN_Halsey_1m = ifelse(wiggleN_Halsey_1m > 0, "foraging", "non-foraging"),
            pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 0, "foraging", "non-foraging"),
            pred_wiggleN_Simeone = ifelse(wiggleN_Simeone > 0, "foraging", "non-foraging"),
            pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 1, "foraging", "non-foraging")) %>%
          summarise(
            foraging_Halsey_1m = sum(pred_wiggleN_Halsey_1m == "foraging"),
            nonforaging_Halsey_1m = sum(pred_wiggleN_Halsey_1m == "non-foraging"),
            
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
    values_to = "Nforagingdives") %>%
    separate(Wiggle_type , into = c("dive_type", "wiggle_metric"), sep = "_", extra = "merge") # %>%
     # pivot_wider(names_from = wiggle_metric, values_from = dives)
  
divetype_summary$proportion = divetype_summary$Nforagingdives / divetype_summary$total_dives
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
    pred_wiggleN_Halsey_1m = ifelse(wiggleN_Halsey_1m > 0, "foraging", "non-foraging"),
    pred_wiggleN_Halsey_2m = ifelse(wiggleN_Halsey_2m > 0, "foraging", "non-foraging"),
    pred_wiggleN_Simeone = ifelse(wiggleN_Simeone > 0, "foraging", "non-foraging"),
    pred_wiggleN_Takahashi = ifelse(wiggleN_Takahashi > 1, "foraging", "non-foraging"))  # exclude the '1' compulsory wiggle per dive


wiggledepth = as.data.frame(wiggledepth)
head(wiggledepth)

wiggledepth <- wiggledepth %>%
    mutate(Halsey_1m = case_when(
         foraging_dive == "foraging" & pred_wiggleN_Halsey_1m == "foraging" ~ "true_pos",
         foraging_dive == "foraging" & pred_wiggleN_Halsey_1m == "non-foraging" ~ "false_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_1m == "non-foraging" ~ "true_neg",
         foraging_dive == "non-foraging" & pred_wiggleN_Halsey_1m == "foraging" ~ "false_pos"))  %>%
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

# OTHER PLOTS

#-------------------------------------------------
# Plot confusion matrices - Approach 1
#-------------------------------------------------

# Here, I 'group_by' Reference, and divide each row element by the sum of the 
# entire row. This leads to normalization of the confusion table.

# Note that the confusion table is in the orientation as Python 
# https://medium.com/analytics-vidhya/visually-interpreting-the-confusion-matrix-787a70b65678

# Since each row represents the total number of actual values for each class label, 
# the final normalized matrix will show us the percentage. 
# ie. out of all true labels for a particular class, 
# what was the % prediction of each class made by our model for that specific true label.

# Create summary table
Halsey_1m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_1m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_1m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Halsey_2m_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Halsey_2m) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Halsey_2m,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Simeone_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Simeone) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Simeone,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

# Create summary table
Takahashi_table <- wiggledepth %>%
  group_by(foraging_dive, pred_wiggleN_Takahashi) %>%
  summarize(Freq = n()) %>%
  rename(Prediction = pred_wiggleN_Takahashi,
         Reference = foraging_dive) %>%
  mutate(Reference = factor(Reference, levels = c("foraging", "non-foraging"))) %>%
  mutate(Prediction = factor(Prediction, levels = c("non-foraging", "foraging"))) %>%
  mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
  ungroup() %>%
  group_by(Reference) %>%
  mutate(prop = round(Freq/sum(Freq),3))

#-------------------------
# ggplot confusion matrices
#-------------------------
# Python way round:
# https://medium.com/analytics-vidhya/visually-interpreting-the-confusion-matrix-787a70b65678
# this is the format of the very popular Python library for ML: sklearn. 

CM_Halsey_1m = 
  ggplot(data = Halsey_1m_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
#  xlim(levels(Halsey_1m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Halsey et al. 2007 (1m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels
         
#CM_Halsey_1m

CM_Halsey_2m = 
  ggplot(data = Halsey_2m_table, 
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
 # xlim(levels(Halsey_2m_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Halsey et al. 2007 (2m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
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
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
#  xlim(levels(Simeone_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Simeone and Wilson 2003 (0.3m)") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
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
         aes(x = Prediction, y = Reference,  fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0("(",prop*100,"%)")), vjust = -1.7, fontface  = "bold", alpha = 1, size = 2.4) +
  geom_text(aes(label = Freq), vjust = 0.8, fontface  = "bold", alpha = 1, size = 3.1) +
  scale_fill_manual(values = c(good = "aquamarine4", bad = "brown3")) +
  theme_bw() +
 # xlim(levels(Takahashi_table$Reference)) + 
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 1.83, label = "True\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 1.83, label = "False\nPositive", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 1, y = 0.83, label = "False\nNegative", size = 2, hjust = 0.5, vjust = 1) +
  annotate("text", x = 2, y = 0.83, label = "True\nPositive", size = 2, hjust = 0.5, vjust = 1) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  ggtitle("Takahashi et al. 2004") + 
  xlab("Predicted (wiggles)")+
  ylab("Reference (video)")+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = -2),
        axis.text = element_text(size = 6),  # Reduce size of tick labels
        axis.title = element_text(size = 7),  # Reduce size of axis labels
        axis.ticks = element_blank(), # Remove ticks
        panel.grid = element_blank(),
        axis.text.x = element_text(margin = margin(t = -1)),  # Adjust margin for x-axis tick labels
        axis.text.y = element_text(margin = margin(r = -1)))   # Adjust margin for y-axis tick labels  
#CM_Takahashi

Figure3_pyth = cowplot::plot_grid(CM_Simeone, CM_Takahashi, CM_Halsey_1m, CM_Halsey_2m, ncol = 2,
                             align = "h", axis = "tblr") 
                             
Figure3_pyth

# # Save as PNG
#ggsave("./plots/CM_Takahashi.png", CM_Takahashi, width = 2.5, height = 2, dpi = 1200)

ggsave("./figures/Figure3_confusion_matrix_pyth.png", Figure3_pyth, width = 4, height = 4, dpi = 1200)

#-------------------------------------------------
# Plot confusion matrices - Approach 2 and stats
#-------------------------------------------------

#library(cvms) plots reference and prediction cells on the opposite axis than python.
# this is not wrong, but I used the python orientation.
# We can still use the cvmr measures of accuracy, etc.

#library(cvms)
# plot_confusion_matrix(Halsey_1m_table, 
#                       target_col = "Reference", 
#                       prediction_col = "Prediction",
#                       counts_col = "Freq") 

cm_Halsey_1m = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Halsey_1m), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Halsey_1m$byClass

cm_Halsey_2m = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Halsey_2m), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Halsey_2m$byClass

cm_Simeone = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Simeone), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Simeone$byClass

cm_Takahashi = confusionMatrix(
  data = as.factor(wiggledepth$pred_wiggleN_Takahashi), # a factor of predicted classes
  reference = as.factor(wiggledepth$foraging_dive), # a factor of classes to be used as the true results
  positive = "foraging") 
cm_Takahashi$byClass

metrics = c('Sensitivity',
            'Specificity',
            'Pos Pred Value',
            'Neg Pred Value',
            'Precision',
            'Recall',
            'F1',
            'Prevalence',
            'Detection Rate',
            'Detection Prevalence',
            'Balanced Accuracy')

tableS2 = cbind(metrics, 
                round(as.data.frame(cm_Simeone$byClass),2),
                round(as.data.frame(cm_Takahashi$byClass),2),
                round(as.data.frame(cm_Halsey_1m$byClass),2),
                round(as.data.frame(cm_Halsey_2m$byClass),2))

names(tableS2) = c("Performance metrics", "Simeone and Wilson 2003 (0.3m)", "Takahashi et al. 2004",
                   "Halsey et al. 2007 (1m)", "Halsey et al. 2007 (2m)")
tableS2

write.table(tableS2, './supplement/tableS2.csv', sep = ",", row.names = F)

#---------------aside----------------------------
# By hand
TP = 1143
FP = 468
FN = 191
TN = 669

# sensitivity = recall: measures the proportion of positive responses correctly identified
# (The ability of the classifier to find all the positive samples) 
(sensitivity = TP / (TP + FN)) # also known as recall

# Specificity: measures the proportion of the negative responses correctly identified
(specificity = TN / (FP + TN))

# Precision: The ability of the classifier not to label a negative sample as positive.
(precision = TP / (TP + FP))

(accuracy = (TP + TN) / (TP + FP + TN + FN))

#---------------aside ends----------------------------

#-----------------------------------------------------------------------
# Boxplot of the propostion of foraging dives across metrics
#-----------------------------------------------------------------------
# Each point in the boxplot is an individual's proportion.
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


#-------------------------------------------
# Plot video and wiggles against each other 
#-------------------------------------------
#https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/

# get_Density <- function(x, y, ...) {
#   Density_out <- MASS::kde2d(x, y, ...)
#   int_x <- findInterval(x, Density_out$x)
#   int_y <- findInterval(y, Density_out$y)
#   comb_int <- cbind(int_x, int_y)
#   return(Density_out$z[comb_int])
# }

Density_map_bottdist <- result_df %>% 
  dplyr::select(id, video_pce_IN, bottdist) %>% 
  mutate(Density = get_Density(video_pce_IN, bottdist, n = 100))

Density_map_wiggleN_Halsey_1m <-result_df %>% 
  dplyr::select(id, video_pce_IN, wiggleN_Halsey_1m) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Halsey_1m, n = 100))

Density_map_wiggleN_Halsey_2m <-result_df %>% 
  dplyr::select(id, video_pce_IN, wiggleN_Halsey_2m) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Halsey_2m, n = 100))

Density_map_wiggleN_Simeone <-result_df %>% 
  dplyr::select(id, video_pce_IN, wiggleN_Simeone) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Simeone, n = 100))

Density_map_wiggleN_Takahashi <-result_df %>% 
  dplyr::select(id, video_pce_IN, wiggleN_Takahashi) %>% 
  mutate(Density = get_Density(video_pce_IN, wiggleN_Takahashi, n = 100))


fig5_bottdist = Density_map_bottdist %>% 
  ggplot(aes(x = bottdist, y =video_pce_IN , color = Density)) +
  geom_point(alpha = 0.3) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1) +
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, 0.01),  # Adjust legend position
    legend.justification = c(1, 0),  # Justify legend to bottom-right
    legend.box.just = "right") +       # Align legend to the right of the box
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("Bottom distance") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25))+
  scale_x_continuous(limits = c(0, 60)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Bottom distance (diveMove)", size = 4, 
           hjust = 1.05, vjust = 1.6) + 
  theme(#legend.text=element_text(size = 8),
    #           legend.title=element_text(size = 10),
    legend.key.size=unit(0.8, "lines"))

fig5_bottdist


fig5_Simeone = Density_map_wiggleN_Simeone %>% 
  ggplot(aes(x = wiggleN_Simeone, y = video_pce_IN, color = Density)) +
 #geom_point(alpha = 0.3) +
 geom_jitter(alpha = 0.3, width = 0.1, height = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.01, 0.99),  # Adjust legend position
    legend.justification = c(0, 1),  # Justify legend to bottom-right
    legend.box.just = "right"       # Align legend to the right of the box
  ) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("No. wiggles") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Simeone and Wilson 2003 (0.3m)", size = 4,  
           hjust = 1.05, vjust = 1.6)+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  theme(#legend.text=element_text(size = 8),
    #           legend.title=element_text(size = 10),
    legend.key.size=unit(0.8, "lines")) #+ coord_flip()

fig5_Simeone

fig5_Takahashi = Density_map_wiggleN_Takahashi %>% 
  ggplot(aes(x = wiggleN_Takahashi, y = video_pce_IN, color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.01, 0.99),  # Adjust legend position
    legend.justification = c(0,1),  # Justify legend to bottom-right
    legend.box.just = "right")+       # Align legend to the right of the box
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("No. wiggles") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Takahashi et al. 2004", size = 4, 
           hjust = 1.1, vjust = 1.6)+
  # Adjust hjust and vjust
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(#legend.text=element_text(size = 8),
    #           legend.title=element_text(size = 10),
    legend.key.size=unit(0.8, "lines"))

fig5_Takahashi


fig5_Halsey_1m = Density_map_wiggleN_Halsey_1m %>% 
  ggplot(aes(x = wiggleN_Halsey_1m, y = video_pce_IN , color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.01, 0.99),  # Adjust legend position
    legend.justification = c(0,1),  # Justify legend to bottom-right
    legend.box.just = "right") +       # Align legend to the right of the box
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("No. wiggles") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Halsey et al. 2007 (1m)", size = 4, 
           hjust = 1.1, vjust = 1.6)+ 
  theme(#legend.text=element_text(size = 8),
    #           legend.title=element_text(size = 10),
    legend.key.size=unit(0.8, "lines"))

fig5_Halsey_1m

fig5_Halsey_2m = Density_map_wiggleN_Halsey_2m %>% 
  ggplot(aes(x = wiggleN_Halsey_2m, y = video_pce_IN, color = Density)) +
  #geom_point(alpha = 0.3) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.01, 0.99),  # Adjust legend position
    legend.justification = c(0,1),  # Justify legend to bottom-right
    legend.box.just = "right") +      # Align legend to the right of the box
  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  #  geom_smooth(method = "lm", se = F, col = "aquamarine4")+  
  xlab("No. wiggles") +
  ylab("Video prey captures") +
  scale_y_continuous(breaks = seq(0,125, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  annotate("text",
           x = Inf, y = Inf, 
           label = "Halsey et al. 2007 (2m)", size = 4, 
           hjust = 1.1, vjust = 1.6) + 
   theme(#legend.text=element_text(size = 8),
          #           legend.title=element_text(size = 10),
                     legend.key.size=unit(0.8, "lines"))
fig5_Halsey_2m


fig5_Takahashi2 = fig5_Takahashi + 
theme(axis.title.y=element_blank(),
axis.text.y=element_blank())

fig5_Halsey_2m2 = fig5_Halsey_2m + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())

figure5 =  fig5_Simeone + fig5_Takahashi2 + 
  fig5_Halsey_1m + fig5_Halsey_2m2 + 
  plot_layout(ncol = 2, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5))) 
figure5

# Save as PNG
#ggsave("./figures/Figure5_points_density.png", figure5, width = 8, height = 6, dpi = 1500)

# Save as PNG
#ggsave("./figures/Figure5_bottist.png", fig5_bottdist, width = 4.2, height = 3.35, dpi = 1500)


#----------------------------------
# Figure 6 - examples
#----------------------------------

figure6_example_ids = Density_map_wiggleN_Halsey_2m %>% 
  filter(id == "2022_01_13_AC2105_HPM09" | 
           id == "2022_01_16_AC2105_DI06" | 
           id == "2022_01_22_AC2110_DI06" | 
           id == "2022_01_10_AC2003_DI09") %>%
  ggplot(aes(x = wiggleN_Halsey_2m, y =video_pce_IN, color = Density)) +
  geom_jitter(alpha = 0.4, size = 1.9, height = 0, width = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 6.1)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 2) + 
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, 0.99),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right") + 
  theme(legend.key.size=unit(0.6, "lines")) 

figure6_example_ids

# Save as PNG
ggsave("./figures/Figure6.png", figure6_example_ids, width = 4, height = 4, dpi = 1000)


#----------------------------------
# plot every individual in a facet
#----------------------------------
fig5_Halsey_1m_ind = Density_map_wiggleN_Halsey_1m %>% 
  ggplot(aes(x = wiggleN_Halsey_1m, y =video_pce_IN, color = Density)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles (Halsey et al. 2007 (1m))") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 4) 

#fig5_Halsey_1m_ind

# Save as PNG
ggsave("./supplement/FigureS3_1_Halsey_1m_ind.png",fig5_Halsey_1m_ind, width = 6, height = 8, dpi = 1000)


fig5_Halsey_2m_ind = Density_map_wiggleN_Halsey_2m %>% 
  ggplot(aes(x = wiggleN_Halsey_2m, y =video_pce_IN, color = Density)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles (Halsey et al. 2007 (2m))") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 4) 

#fig5_Halsey_2m_ind
# Save as PNG
ggsave("./supplement/FigureS3_1_Halsey_2m_ind.png",fig5_Halsey_2m_ind, width = 6, height = 8, dpi = 1000)


fig5_Takahashi_ind = Density_map_wiggleN_Takahashi %>% 
  ggplot(aes(x = wiggleN_Takahashi, y = video_pce_IN, color = Density)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles (Takahashi et al. 2004)") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 4) 

#fig5_Takahashi_ind
# Save as PNG
ggsave("./supplement/FigureS3_1_Takahashi_ind.png",fig5_Takahashi_ind, width = 6, height = 8, dpi = 1000)


fig5_Simeone_ind = Density_map_wiggleN_Simeone %>% 
  ggplot(aes(x = wiggleN_Simeone, y = video_pce_IN, color = Density)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles (Simeone and Wilson 2003 (0.3m))") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 16)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 4) 

#fig5_Simeone_ind 
# Save as PNG
ggsave("./supplement/FigureS3_1_Simeone_ind.png",fig5_Simeone_ind , width = 6, height = 8, dpi = 1000)


fig5_bottdist_ind = Density_map_bottdist %>% 
  ggplot(aes(x = bottdist, y =video_pce_IN, color = Density)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("Bottom distance") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 40)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 4) 

#fig5_bottdist_ind

# Save as PNG
ggsave("./supplement/FigureS3_1_bottdist_ind.png",fig5_bottdist_ind, width = 6, height = 8, dpi = 1000)


#--------------------------------------------
# Calculate cumulative PCE (by dive number)
#--------------------------------------------
# idea from Allegue et al. Movement Ecology (2023) 11:3

result_df = result_df %>% 
  group_by(id) %>%
  mutate(video_cumulativePCE = cumsum(video_pce_IN),
         cumulativeHalsey05 = cumsum(wiggleN_Halsey_1m),
         cumulativeHalsey2 = cumsum(wiggleN_Halsey_2m),
         cumulativeSimeone = cumsum(wiggleN_Simeone),
         cumulativeTakahashi = cumsum(wiggleN_Takahashi),
         cumulativebottdist = cumsum(bottdist))

# individual variation (PCE against dive nr)
result_df %>%
  ggplot(aes(x = dive.nr, y = video_cumulativePCE, color = id)) +
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "none")

#-----------------------------------------
# cumulative PCE vs cumulative wiggles:  
#-----------------------------------------
cumulativeHalsey05 =
  result_df %>%
  ggplot(aes(x = cumulativeHalsey05, y = video_cumulativePCE, color = id)) +
  geom_line() +
  gg_theme() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  ylab("Cumulative captures") +
  xlab("Cumulative wiggles") +
  xlim(0,600) +
  annotate("text",
           x = Inf, y = Inf,
           label = "Halsey et al. 2007 (1m)", size = 4,
           hjust = 1.1, vjust = 1.6)


cumulativeHalsey2 =
  result_df %>%
  ggplot(aes(x = cumulativeHalsey2, y = video_cumulativePCE, color = id)) +
  geom_line() +
  gg_theme() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  ylab("Cumulative captures") +
  xlab("Cumulative wiggles") +
  xlim(0,600) +
  annotate("text",
           x = Inf, y = Inf,
           label = "Halsey et al. 2007 (2m)", size = 4,
           hjust = 1.1, vjust = 1.6)

cumulativeSimeone =
  result_df %>%
  ggplot(aes(x = cumulativeSimeone, y = video_cumulativePCE, color = id)) +
  geom_line() +
  gg_theme() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  ylab("Cumulative captures") +
  xlab("Cumulative wiggles") +
  xlim(0,600) +
  annotate("text",
           x = Inf, y = Inf,
           label = "Simeone and Wilson 2003 (0.3m)", size = 4,
           hjust = 1.05, vjust = 1.6)

cumulativeTakahashi =
  result_df %>%
  ggplot(aes(x = cumulativeTakahashi, y = video_cumulativePCE, color = id)) +
  geom_line() +
  gg_theme() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  ylab("Cumulative captures") +
  xlab("Cumulative wiggles") +
  xlim(0,600) +
  annotate("text",
           x = Inf, y = Inf,
           label = "Takahashi et al. 2004", size = 4,
           hjust = 1.1, vjust = 1.6)

cumulativebottdist =
  result_df %>%
  ggplot(aes(x = cumulativebottdist , y =video_cumulativePCE, color = id)) +
  geom_line() +
  gg_theme() +
  theme(legend.position = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")+
  ylab("Cumulative captures") +
  xlab("Cumulative bottom distance") +
  annotate("text",
           x = Inf, y = Inf,
           label = "Bottom distance (diveMove)", size = 4,
           hjust = 1.05, vjust = 1.6)

cumulativePCE = cowplot::plot_grid(
  (cumulativeSimeone+cumulativeTakahashi+ cumulativeHalsey05) /
    (cumulativeHalsey2+cumulativebottdist  + plot_spacer()) +
    plot_annotation(tag_levels = 'A'))

cumulativePCE

#------------------------------------------------
# Repeat above plots, but plot with facet~wrap 
#------------------------------------------------
cumulativePCE_d = result_df %>%
  dplyr::select(id, 
                video_cumulativePCE,
                cumulativeHalsey05,
                cumulativeHalsey2,
                cumulativeSimeone,
                cumulativeTakahashi,
                cumulativebottdist) %>%
  pivot_longer(cols = starts_with("cumulative"),
               names_to = "metric",
               values_to = "wigglecumulativePCE")

new_labels <- c("cumulativeHalsey05" = "Halsey et al. 2007 (1m)", 
                "cumulativeHalsey2" = "Halsey et al. 2007 (2m)", 
                "cumulativeSimeone" = "Simeone and Wilson 2003 (0.3m)", 
                "cumulativeTakahashi" = "Takahashi et al. 2004",
                "cumulativebottdist" = "Bottom distance")

# Define the desired order of facets
desired_order <- c("cumulativeSimeone",
                   "cumulativeTakahashi", 
                   "cumulativeHalsey05", 
                   "cumulativeHalsey2", 
                   "cumulativebottdist")

# Reorder the levels of 'metric' according to the desired order
cumulativePCE_d$metric <- factor(cumulativePCE_d$metric, levels = desired_order)

cumulative_plot = cumulativePCE_d %>%
  ggplot(aes(x = wigglecumulativePCE, y = video_cumulativePCE, color = id)) +
  geom_line() + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_abline(slope = 1, intercept = 0) + 
  ylab("Cumulative prey captures") + 
  xlab("Cumulative wiggles") + 
  facet_wrap(~ metric, 
             ncol = 2,
             #  scales = 'free_y',
             labeller = labeller(metric = new_labels))+
  theme(strip.text = element_text(size = 12))  

cumulative_plot

# Save as PNG
# ggsave("./plots/Supp_fig_cumulativePCE.png", cumulative_plot, width = 6, height = 8, dpi = 1200)


#------------------------------------------
# plot PCE and cumulative PCE together
#------------------------------------------
no_xaxis = theme(plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "mm"), 
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank())+ 
  theme(axis.text = element_text(size = 10)) 

sml_legend = theme(legend.text=element_text(size = 8),
                   legend.title=element_text(size = 10),
                   legend.key.size=unit(0.5, "lines")) +
    theme(axis.text = element_text(size = 10)) 

# Combine the plots into a list
plots <- list(fig5_Simeone + sml_legend , 
              cumulativeSimeone + no_xaxis , 
              fig5_Takahashi + sml_legend ,
              cumulativeTakahashi + no_xaxis, 
              fig5_Halsey_1m + sml_legend + no_xaxis ,
              cumulativeHalsey05 + no_xaxis,
              fig5_Halsey_2m + sml_legend ,
              cumulativeHalsey2  +  theme(axis.text = element_text(size = 10)) ,
              fig5_bottdist + sml_legend,
              cumulativebottdist +  theme(axis.text = element_text(size = 10)))

# Arrange the plots in a 2x5 grid using patchwork
PCE_cumulative = wrap_plots(plots, ncol = 2, nrow = 5)
PCE_cumulative 

# Save as PNG
ggsave("./figures/Figure5.png", 
       PCE_cumulative, width = 8, height = 10, dpi = 1200)


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

z = subdat_Halsey_1m %>% 
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
           label = "Wiggle: Halsey et al. 2007 (1m)", 
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

Fig6A = ggplot(data = dive_example, 
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
  
  annotate("text", x = max(dive_example$date.time),  y = -18, label = "Video prey captures", 
           size = 5, color = "black", hjust = 1, vjust = 0)+
 # annotate("point", x = max(dive_example$date.time)-265,  y = -18, size = 2.5, 
#           fill = "red", col = "navy", shape = 21)  +
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

# plot Halsey_1m (wiggles)
x = subdat_Halsey_1m %>% 
  dplyr::filter(id == example_animal &
                  wiggle ==  1 &
                  date.time >= min(dive_example$date.time) &
                  date.time <= max(dive_example$date.time))

Fig6B = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = x, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
#  annotate("text", x = max(dive_example$date.time)-450,  y = -16, label = "Wiggle:", 
#           size = 5, color = "black", hjust = 1, vjust = 0)+
  annotate("text", x = max(dive_example$date.time),  y = -18, label = "Halsey et al. 2007 (1m)", 
           size = 5, color = "black", hjust = 1, vjust = 0)+
 # annotate("point", x = max(dive_example$date.time)-260,  y = -17.7, size = 2.5, 
 #          fill = "red", col = "navy", shape = 21)+
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

Fig6C = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = y, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
#  annotate("text", x = max(dive_example$date.time)-395,  y = -16, label = "Wiggle:", 
#           size = 5, color = "black", hjust = 1, vjust = 0)+
  annotate("text", x = max(dive_example$date.time),  y = -18, label = "Simeone and Wilson 2003 (0.3m)", 
           size = 5, color = "black", hjust = 1, vjust = 0)+
#  annotate("point", x = max(dive_example$date.time)-375,  y = -17.7, size = 2.5, 
#           fill = "red", col = "navy", shape = 21)+
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
Fig6D = ggplot(data = dive_example, 
               aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15)+
  scale_color_manual(values = my_colors)+
  geom_point(data = z, aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)")+
 # annotate("text", x = max(dive_example$date.time),  y = -16, label = "Wiggle:", 
 #           size = 5, color = "black", hjust = 1, vjust = 0)+
  annotate("text", x = max(dive_example$date.time),  y = -18, label = "Takahashi et al. 2004", 
           size = 5, color = "black", hjust = 1, vjust = 0)+
 # annotate("point", x = max(dive_example$date.time)-265,  y = -17.7, size = 2.5, 
#           fill = "red", col = "navy", shape = 21)+
  theme(legend.position = "none") +
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.title = element_text(size=15))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

Figure6_example  = Fig6A / Fig6C / Fig6D / Fig6B 
Figure6_example

# Save as PNG
ggsave("./figures/Figure6_example.png", Figure6_example, width = 8, height = 9, dpi = 1000)


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


#-----------------------------------------------
# Histograms of PCE per dive (all individuals)
#-----------------------------------------------

alldives_hist = 
# Are PCE zero inflated? (yes) 
result_df %>%
#  filter(video_pce_IN > 0) %>%
  ggplot() + 
  geom_histogram(aes(video_pce_IN, fill = foraging_dive), binwidth = 1)+
  scale_fill_manual(values = c("non-foraging" = "black", "foraging" = "red"))+
  scale_x_continuous(breaks = c(0, 25,50,75,100,125)) + 
  gg_theme() + 
  theme(legend.position = "inside",
    legend.position.inside = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "transparent", color = NA, linewidth = 0.5))+
  labs(x = "Prey captures per dive",
       y = "Count",
       fill = "Dive type")

allPCdives_hist = result_df %>%
    filter(video_pce_IN > 0) %>%
    ggplot() + 
    geom_histogram(aes(video_pce_IN), binwidth = 1, fill = "red") + 
  labs(x = "Prey captures per dive",
       y = "Count") +
  scale_x_continuous(breaks = c(0, 25,50,75,100,125))+
  gg_theme() 

alldives_hist + allPCdives_hist

# Save as PNG
ggsave("./supplement/Supp_PCE_per_dive_histogram.png", 
       alldives_hist + allPCdives_hist, 
       width = 8, height = 4, dpi = 1500)

#-----------------------------------------------
# Histograms of PCE per dive (by individual)
#-----------------------------------------------
PC_dive_id = 
  result_df %>% 
  # filter(video_pce_IN > 0) %>%
  ggplot() + 
  aes(x = video_pce_IN, y = id, fill = after_stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Prey captures per dive", option = "C") +
  theme_bw() + 
  geom_hline(yintercept = 13.5, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 40, by = 10)) + 
  scale_y_discrete(labels = rev(seq_along(levels(result_all$id))))+
  ylab("Individual") + 
  xlab("Prey captures per dive") +
#  theme(legend.position = "inside",legend.position.inside = c(0.85, 0.85),  # Adjust legend position
#        legend.background = element_blank()) + # Remove legend background  # Adjust legend position
  annotate("text", x = 35, y = 16, label = "2022", color = "black")+  # Add vertical text outside plot
  annotate("text", x = 35, y = 11, label = "2023", color = "black")  # Add vertical text outside plot

PC_dive_id 

# Save as PNG
ggsave("./plots/Supp_fig_pce.png", PC_dive_id, width = 5, height = 7, dpi = 1200)


result_df <- result_df %>%
  group_by(year) %>%
  mutate(id_number = as.numeric(factor(id))) %>%
  ungroup()

unique((result_df$id_number))

# Filter data based on ID names
PC_dive_id2 = 
  result_df %>% 
  ggplot() + 
  aes(x = video_pce_IN, y = as.factor(id_number)) +
  geom_density_ridges2(alpha= 1,  stat="binline", binwidth = 2,
                       fill = "darkblue", color = "black",
                       panel_scaling = F) +
  
  theme_bw() + 
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) + 
  scale_y_discrete()+
  ylab("Individual") + 
  xlab("Prey captures per dive") +
  theme(legend.position = "none") +
  facet_grid(~year) 

PC_dive_id2

# Save as PNG
ggsave("./plots/Supp_fig_pce_hist.png", PC_dive_id2, width = 6, height = 8, dpi = 1200)



#--------------------------------------
# Kendall's rank correlation 
#--------------------------------------

tau_Halsey_1m <- cor.test(result_df$video_pce_IN,
                           result_df$wiggleN_Halsey_1m, 
                           method="kendall")
tau_Halsey_1m

tau_Halsey_2m <- cor.test(result_df$video_pce_IN,
                          result_df$wiggleN_Halsey_2m, 
                          method="kendall")
tau_Halsey_2m

tau_Simeone <- cor.test(result_df$video_pce_IN,
                        result_df$wiggleN_Simeone, 
                        method="kendall")
tau_Simeone

tau_Takahashi <- cor.test(result_df$video_pce_IN,
                          result_df$wiggleN_Takahashi, 
                          method="kendall")
tau_Takahashi


#-------------------------
# Catch per unit effort 
#-------------------------
# Attempts of catch per unit effort (ACPUE) 
# ACPUE = # Total number of wiggles / Total time in bottom duration (Riaz et al. 2020, 2021)

# Calculate the sum of PCE and botttim across IDs:
result_df = result_df %>% 
  group_by(id) %>%
  mutate(CPUE_video = sum(video_pce_IN) / sum(botttim),
         CPUE_Halsey_1m = sum(wiggleN_Halsey_1m) / sum(botttim),
         CPUE_Halsey_2m = sum(wiggleN_Halsey_2m) / sum(botttim),
         CPUE_Simeone = sum(wiggleN_Simeone) / sum(botttim),
         CPUE_Takahashi = sum(wiggleN_Takahashi) / sum(botttim),
         CPUE_bottdist = sum(bottdist) / sum(botttim)) %>%
  ungroup()

# Find the maximum values for each CPUE column
max_CPUE <- result_df %>%
  summarise(
    max_CPUE_video = max(CPUE_video, na.rm = TRUE),
    max_CPUE_Halsey_1m = max(CPUE_Halsey_1m, na.rm = TRUE),
    max_CPUE_Halsey_2m = max(CPUE_Halsey_2m, na.rm = TRUE),
    max_CPUE_Simeone = max(CPUE_Simeone, na.rm = TRUE),
    max_CPUE_Takahashi = max(CPUE_Takahashi, na.rm = TRUE),
    max_CPUE_bottdist = max(CPUE_bottdist, na.rm = TRUE)
  )

# Scale each CPUE value by the corresponding maximum value
result_df <- result_df %>%
  mutate(
    scaled_CPUE_video = CPUE_video / max_CPUE$max_CPUE_video,
    scaled_CPUE_Halsey_1m = CPUE_Halsey_1m / max_CPUE$max_CPUE_Halsey_1m,
    scaled_CPUE_Halsey_2m = CPUE_Halsey_2m / max_CPUE$max_CPUE_Halsey_2m,
    scaled_CPUE_Simeone = CPUE_Simeone / max_CPUE$max_CPUE_Simeone,
    scaled_CPUE_Takahashi = CPUE_Takahashi / max_CPUE$max_CPUE_Takahashi,
    scaled_CPUE_bottdist = CPUE_bottdist / max_CPUE$max_CPUE_bottdist
  )

# View the result
print(result_df)

n_distinct(result_df$CPUE_video)
n_distinct(result_df$CPUE_Halsey_2m)

cpue_long = result_df %>%
  dplyr::select(id, 
                year,
                CPUE_video,
                CPUE_Halsey_1m, 
                CPUE_Halsey_2m, 
                CPUE_Simeone, 
                CPUE_Takahashi, 
                CPUE_bottdist) %>% 
  pivot_longer(cols = starts_with("CPUE"),
               names_to = "CPUE_metric",
               values_to = "CPUE_id") %>%
  distinct()
cpue_long

table(cpue_long$CPUE_metric, cpue_long$year)

# Reorder the factor levels
cpue_long$CPUE_metric <- factor(cpue_long$CPUE_metric, 
                                levels = c("CPUE_video",
                                           "CPUE_Simeone",
                                           "CPUE_Takahashi",
                                           "CPUE_Halsey_1m",
                                           "CPUE_Halsey_2m",
                                           "CPUE_bottdist"),
                                labels = c("CPUE_video" = "Video\nobservations",
                                           "CPUE_Simeone"  = "Simeone &\nWilson\n2003",
                                           "CPUE_Takahashi" = "Takahashi\n et al. 2004",
                                           "CPUE_Halsey_1m" = "Halsey et al.\n2007 (1m)",
                                           "CPUE_Halsey_2m" = "Halsey et al.\n2007 (2m)",
                                           "CPUE_bottdist" = "Bottom\ndistance"))


cpue_long %>%
  ggplot(aes(x =  CPUE_metric, y = CPUE_id)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0, width = 0.1, color = "aquamarine4", alpha = 0.7) + 
  gg_theme() +
  xlab("Wiggle metric") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size=11))  

#---------------------------------
# Relative CPUE
# Divide by the max per group:
#---------------------------------

cpue_long_scaled = result_df %>%
  dplyr::select(id, 
                year,
                scaled_CPUE_video,
                scaled_CPUE_Halsey_1m, 
                scaled_CPUE_Halsey_2m, 
                scaled_CPUE_Simeone, 
                scaled_CPUE_Takahashi, 
                scaled_CPUE_bottdist) %>% 
  pivot_longer(cols = starts_with("scaled"),
               names_to = "CPUE_metric",
               values_to = "CPUE_id") %>%
  distinct()
cpue_long_scaled

table(cpue_long_scaled$CPUE_metric, cpue_long_scaled$year)

# Reorder the factor levels
cpue_long_scaled$CPUE_metric <- factor(cpue_long_scaled$CPUE_metric, 
                                       levels = c("scaled_CPUE_video",
                                                  "scaled_CPUE_Simeone",
                                                  "scaled_CPUE_Takahashi",
                                                  "scaled_CPUE_Halsey_1m",
                                                  "scaled_CPUE_Halsey_2m",
                                                  "scaled_CPUE_bottdist"),
                                       labels = c("scaled_CPUE_video" = "Video\nobservations",
                                                  "scaled_CPUE_Simeone"  = "Simeone &\nWilson\n2003",
                                                  "scaled_CPUE_Takahashi" = "Takahashi\n et al. 2004",
                                                  "scaled_CPUE_Halsey_1m" = "Halsey et al.\n2007 (1m)",
                                                  "scaled_CPUE_Halsey_2m" = "Halsey et al.\n2007 (2m)",
                                                  "scaled_CPUE_bottdist" = "Bottom\ndistance"))


# Define the number of colors you want
nb.cols <- 38
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

# Convert ID to a factor and get the levels as numeric values
cpue_long_scaled$id_num <- as.numeric(factor(cpue_long_scaled$id))
cpue_long$id_num <- as.numeric(factor(cpue_long$id))

cpue_long_scaled %>%
  ggplot(aes(x = CPUE_metric, y = CPUE_id, colour = id)) + 
  geom_boxplot(color = "darkslategray") +
  geom_jitter(height = 0.1, width = 0.1, alpha = 1) + 
  gg_theme() +
  xlab("Wiggle metric") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size=11)) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = mycolors) 

cpue_long_scaled


cpue_long %>%
  ggplot(aes(x = CPUE_metric, y = CPUE_id, colour = id, label = id_num)) + 
  geom_point(shape = 95, size = 20, alpha = .43)+ 
  theme(legend.position = "none") +
  gg_theme() +
  xlab("Wiggle metric") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size=11)) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = mycolors) + 
  geom_text()

a = cpue_long_scaled  %>%
  filter( CPUE_metric == 'Video\nobservations')

b = cpue_long  %>%
  filter( CPUE_metric == 'Video\nobservations')

cpue_long_scaled %>%
  ggplot(aes(x = CPUE_metric, y = CPUE_id, colour = id, label = id_num)) + 
  geom_point(shape = 95, size = 20, alpha = .43)+ 
  gg_theme() +
  xlab("Wiggle metric") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size=11)) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = mycolors) + 
  geom_text()


#---------------------
# Tanglegram of CPUE
#---------------------

# Create an empty list to store ggplots
plot_list <- list()
tau_list <- list()

for(i in unique(cpue_long$CPUE_metric)) { 

  tmp = cpue_long_scaled %>%
    filter(CPUE_metric  == "Video\nobservations" | 
             CPUE_metric  ==  i)
  
  tau_vid = subset(tmp, tmp$CPUE_metric == "Video\nobservations")
  tau_wig = subset(tmp, tmp$CPUE_metric == i)
  
  tau_i <- cor.test(tau_vid$CPUE_id, tau_wig$CPUE_id, method="kendall")
  tau_list[[i]] = tau_i 
  tau_label = round(tau_i$estimate,2)

  plot_list[[i]] = 
    ggplot(data = tmp, aes(x = CPUE_metric,
                           y = CPUE_id,
                           color = id,
                           group = id)) + 
    geom_point(size = 3) + 
    geom_line(color = "black")+ 
    gg_theme() +
    scale_color_manual(values = mycolors) +
    theme(legend.position = "none") +
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ylab("Catch per unit effort (normalised)") +
    annotate("text", x = 0.8, y = 0.5, label = "Video observations", 
             angle = 90, vjust = 1) + 
    annotate("text", x = 2.1, y = 0.5, label = i, angle = 90, vjust = 1) + 
    annotate("text", x = 2.3, y = 0.02, label = tau_label, size = 6)
  
}

#------------------------------------------------------
# Make a plot for video obs too
#------------------------------------------------------
p1 = cpue_long_scaled  %>%
  dplyr::filter(CPUE_metric == "Video\nobservations")
p1$tmp = paste(p1$CPUE_metric, 1)
p2 = cpue_long_scaled  %>%
  dplyr::filter(CPUE_metric == "Video\nobservations")
p2$tmp = paste(p2$CPUE_metric, 2)
pp = rbind(p1,p2)

p = ggplot(data = pp, aes(x = tmp,
                          y = CPUE_id,
                          color = id,
                          group = id)) + 
  geom_point(size = 3) + 
  geom_line(color = "black")+ 
  gg_theme() +
  scale_color_manual(values = mycolors) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Catch per unit effort (normalised)") +
  annotate("text", x = 0.8, y = 0.5, label = "Video observations", 
           angle = 90, vjust = 1) + 
  annotate("text", x = 2.1, y = 0.5, label = "Video observations", angle = 90, vjust = 1) + 
  annotate("text", x = 2.3, y = 0.02, label = 1, size = 6)

p
#--------------------------------------------------------

plot_list[[1]] = p

cpue_tanglegram = patchwork::wrap_plots(c(plot_list[1],
                                          plot_list[4],
                                          plot_list[5],
                                          plot_list[2],
                                          plot_list[3],
                                          plot_list[6]),
                                        ncol = 3)  # Arrange plots in 2 columns
cpue_tanglegram


tau_list

# Save as PNG
ggsave("./figures/Figure4.png", cpue_tanglegram, width = 10, height = 6.5, dpi = 1200)


#--------tanglegrams end------------------------------------------------------------------


cpue_long_scaled %>%
  ggplot(aes(x = CPUE_metric, y = CPUE_id, colour = id, label = id_num)) + 
  geom_point(shape = 95, size = 20, alpha = .43)+ 
  xlab("Wiggle metric") + 
  ylab("Catch per unit effort") +
  theme(axis.text = element_text(colour = "black", size = 9),
        axis.title = element_text(size=11)) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = mycolors) + 
  geom_text()

#========================
# CPUE boxplots
#========================
cpue = result_df %>%
  dplyr::select(id, bout, year, CPUE_video,
                CPUE_bottdist, CPUE_Halsey_1m, CPUE_Halsey_2m, CPUE_Simeone, CPUE_Takahashi) %>%
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
  dplyr::select(year, CPUE_bottdist) %>%
  drop_na() %>%
  #summarise(mean_CPUE = mean(ACPUE_bottdist)) %>%
  ggplot(aes(x = as.factor(year), y = CPUE_bottdist)) + 
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
  dplyr::select(year, CPUE_bottdist) %>%
  drop_na() %>%
  summarise(mean_CPUE = mean(CPUE_bottdist)) %>%
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
  dplyr::select(year, CPUE_video) %>%
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


cpue_CPUE_Halsey_1m = 
  result_df %>%
  subset(maxdep > 3) %>%
  group_by(id, year) %>%
  dplyr::select(year, CPUE_Halsey_1m) %>%
  drop_na() %>%
  summarise(mean_CPUE = mean(CPUE_Halsey_1m)) %>%
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
           label = "CPUE Halsey et al. 2007 (1m)", size = 4.5, 
           hjust = 1, vjust = 0)    # Adjust hjust and vjust

cpue_CPUE_Halsey_1m


cpue_fig = cpue_CPUE_video / cpue_CPUE_Halsey_1m / cpue_bottdist 
cpue_fig

# Save as PNG
ggsave("./plots/cpue_figs.png", cpue_fig, width = 4, height = 7, dpi = 1500)

#--------------------------
#--------------------------
saveRDS(result_df, './output/result_df_quant.rds')
#result_df = readRDS('./output/result_df_quant.rds')


