# Set system time zone
Sys.setenv(TZ = "GMT")

# Load libraries
library(tidyverse)
library(diveMove)


# What files are there in the data directory? 
process.dat <- list.files("./data_processed_1hz/",
                          pattern = "2022_01_22_AC2110_DI06",
                          #pattern = '2022_01_07',           # debugging
                          # pattern = '2023_01_14_KI18',
                          full.names = TRUE)

# print the name of each file
for (file in process.dat) {
  print(file)
}

# how many files
length(process.dat)

# create lists to save dive statistics and dive phase data for every id
DiveStats = list() 
DiveDat = list()

for (file in process.dat) {
  
  #---------------------
  # 1. import tag data
  #---------------------
  dat = readRDS(file)
  
  sum(dat$PCE_1hz)
  
  # Choose a depth threshold (in m) and time threshold (in sec) to define a dive
  # The time threshold can be used to filter dives after DiveMove calculated dives.
  depth.threshold = 3
  
  # remove dives that are not complete at the start and end of the video time series:
  # Find the index of the first and last values less than 0.5 depth
  dat = dat %>% arrange(date.time)
  index_start = min(which(dat$depth < depth.threshold))
  index_end = max(which(dat$depth < depth.threshold))
  
  # Slice the data frame from index_start to index_end
  dat = dplyr::slice(dat, index_start:index_end)
  
  # Calculate PCE in the data
  dat$totalPCE = sum(dat$PCE_1hz)
  
  #===================================
  # 2. Divemove analysis
  #===================================
  dat = as.data.frame(dat)
  
  # now start creating the TDR dive object
  filename = "Divedata"
  
  tdr <- createTDR(time = dat$date.time, 
                   depth = dat$depth, 
                   speed = FALSE, 
                   dtime = 1,   #  sampling interval used in seconds
                   file = filename)
  
  # Make sure 'depth' is positive and starts at zero!
  # show(tdr)
  
  # detect individual dives
  tdr.calib = calibrateDepth(tdr,
                             dive.thr = depth.threshold, # only select dives deeper than threshold
                             zoc.method='filter',
                             k=c(3, 5760),
                             probs=c(0.5, 0.02),
                             dive.model = "unimodal",
                             smooth.par=0.1,
                             knot.factor=20, 
                             descent.crit.q=0.01, 
                             ascent.crit.q=0,
                             na.rm=T)
  
  tdr.dat = as.data.frame(tdr.calib@tdr)
  
  # create dive summary metrics for each dive 
  dives <- diveStats(tdr.calib)        
  
  # add id to divestats
  dives$id = tools::file_path_sans_ext(basename(file))
  
  # How many dives did this penguin do?
  dives$diveN = as.numeric(length(unique(dives$begdesc)))
  
  # add row numbers to divestats - each row is a new dive
  dives$dive.nr <- seq.int(nrow(dives))
  
  # when do dives end? (time at end of dive)
  dives$enddive <- as.POSIXct(dives$divetim, origin = dives$begdesc, tz = "GMT") 
  
  # what was the depth when the bird started to ascent? 
  begasc_times = as.data.frame(dives$begasc)
  names(begasc_times) = 'time'
  
  begasc_depths = merge(begasc_times, tdr.dat)
  dives$begasc_dep = begasc_depths$depth
  
  #select only the columns you want
  dives = dives %>% dplyr::select(id, diveN,
                                  dive.nr,
                                  begdesc, enddesc, begasc, enddive,
                                  desctim, asctim, botttim, divetim,
                                  maxdep, begasc_dep,
                                  bottdist,bottdep.sd)
  
  dives$totalPCE = mean(dat$totalPCE)
  
  DiveStats[[file]] = dives 
  
  # add dive phases to the dive data
  nrow(dat)
  nrow(tdr.dat)
  dat$depth = tdr.dat$depth   # replace measured depth with ZOC depth (they hardly differ)
  length(tdr.calib@dive.phases)
  dat$InDive = tdr.calib@dive.phases
  DiveDat[[file]] = dat
  
}  # end of diveMove loop

dive.stats = dplyr::bind_rows(DiveStats)
unique(dive.stats$id)
dim(dive.stats)
saveRDS(dive.stats, "./output/dive_stats.rds")

divedat = dplyr::bind_rows(DiveDat)
saveRDS(divedat, "./output/divedat.rds")

#----------------------------------
# 2. DiveMove summary statistics
#----------------------------------
sum.stats = dive.stats %>%
  group_by(id) %>% 
  summarize(diveN = unique(diveN),
            mean.maxdep = mean(maxdep),
            mean.divetim = mean(divetim),
            #  sum.bottdist = sum(bottdist, na.rm = TRUE),
            # mean bottom distance in dives that had bottom time
            #    mean.bottdist = mean(bottdist, na.rm = TRUE),
            # mean bottom distance in ALL dives, including those that did not have a bottom time
            #    bottdistPerDive = sum.bottdist/diveN,   
            Vid.totalPCE = mean(totalPCE))

sum.stats

# sum.stats is the summary per individual (thus 42 rows for 2022 and 2023 SOI chinstrap penguin data)
# for dives deeper than the diveMove depth threshold. 
# The Vid.PCE_allN is the total count of PC annotations the videos. This PC count is the sum total 
# REGARDLESS of dive depth or dive time. 

#============================================================================================
#----------------------------------------------------------------
# Sanity check: was the TDR data (diveMove analysis) at 1 second? 
#----------------------------------------------------------------
divedat = divedat %>% 
  group_by(id) %>% 
  mutate(dive.gap = difftime(date.time, 
                             lag(date.time, 
                                 default = date.time[1]), units = "sec")) 

table(divedat$dive.gap) # it is: the 0 is the start time for each ID. And there is 1 occasion with a 2 sec. gap (unimportant)

#============================================================================================