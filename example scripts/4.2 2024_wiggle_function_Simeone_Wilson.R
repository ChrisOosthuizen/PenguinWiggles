

#-----------------------------------------------------------------------------
# Chris Oosthuizen
# March 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

# In the function below, 'subdat' is the TDR data (time and depth) from a single dive.
# Note: diveMove works with positive dives, so in this case I make dives negative.
# Make sure whether your input data for dives are positive or negative.

#----------------------------------------------------------------------------------
# Calculate undulations following Simeone and Wilson 2003 
#----------------------------------------------------------------------------------
# Simeone, A., & Wilson, R. P. (2003). In-depth studies of Magellanic penguin 
# (Spheniscus magellanicus) foraging: can we estimate prey consumption by 
# perturbations in the dive profile?. Marine Biology, 143, 825-831.

# An undulation was considered to have occurred when a change in depth over any 1 s
# was >0.3 m (equivalent to ca. twice the maximum ventro-dorsal diameter of a Magellanic penguin)
# more than the mean rate of change of depth recorded over the previous 3 s, 
# unless an undulation had already occurred within this 3-s time window. 

# Options: set the threshold for change in depth
# threshold <- 0.3

Simeone_wiggles <- function(subdat, wiggle_threshold = 0.3) {

  # the code breaks down if there are fewer than 5 rows in a dive.
  # add this if statement; at the end, the 'else' returns a 0 (0 wiggles) for dives < 5 data points
  
  if (nrow(subdat) > 4) {
    
  #-----------------------------------
  # Get the (absolute) vertical speed
  #-----------------------------------
  # make depth negative for wiggle analysis 
  subdat$wig_depth = subdat$depth * -1
  
  subdat$vertical_speed <- c(NA, diff(subdat$wig_depth)/as.numeric(diff(subdat$date.time)))
  subdat$rate_of_change <- abs(subdat$vertical_speed)
  
  #---------------------
  # Find undulations 
  #---------------------
  undulation_indices <- c()
  subdat$mean_rate = NA
  
  # Iterate through the rows of the dataset
  for (i in 5:nrow(subdat)) {
    # Calculate the mean rate of change of depth over the previous 3 datapoints
    subdat$mean_rate[i] <- mean(subdat$rate_of_change[(i-3):(i-1)])
    
    # Check if the current rate of change is greater than the threshold + mean rate
    if (subdat$rate_of_change[i] > (subdat$mean_rate[i] + wiggle_threshold)) {
      # Check if an undulation has already occurred within the previous 3 datapoints
      if (!any((i-3):(i-1) %in% undulation_indices)) {
        # Add the index of the current datapoint as an undulation
        undulation_indices <- c(undulation_indices, i)
      }
    }
  }
  
  #--------------------------------------------
  # Print the indices where undulations occur
  #--------------------------------------------
  subdat$undulations <- 0
  subdat$undulations[undulation_indices] <- 1
  
  # Create a column to add up how many wiggles there are 
  
  subdat = subdat %>%
    mutate(wiggle_count = replace_na(undulations, 0)) %>%
    mutate(wiggle_count = cumsum(undulations))
  
  wiggleN_Simeone = max(subdat$wiggle_count)
  
  return(list(wiggleN_Simeone = wiggleN_Simeone,
              subdat_Simeone = subdat))
  
  } 
  # what should happen if the subdata is < 5 data points long?
  # Report that there are 0 wiggles (wiggleN_Simeone is the variable that gets saved 
  # in the other script)
  else {
    # If there are 10 or fewer rows, you can specify what to do in this block
    wiggleN_Simeone <- list(wiggleN_Simeone = 0,
                            subdat_Simeone = subdat)
  }
}
