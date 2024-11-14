
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# March 2024
# Calculate penguin wiggles
#-----------------------------------------------------------------------------

# In the function below, 'subdat' is the TDR data (time and depth) from a single dive.
# Note: diveMove works with positive dives, so in this case I make dives negative.
# Make sure whether your input data for dives are positive or negative.

#----------------------------------------------------------------------------------
# Calculate wiggles following Halsey et al. 2007 
#----------------------------------------------------------------------------------
# From Halsey et al. 2007 : Polar Biol (2007) 30:991–1004

# Wiggles occur where an increase in depth over time changes to a decrease in depth 
# and then back to an increase in depth (thus: decent to accent to decent). 
# This creates a short period in the dive profile that is concave in shape. 
# Thus, wiggles are defined by 3 points of the vertical speed that passes below (or through) 0 ms–1).
# If useful, certain wiggles could be ignored, e.g. using a threshold based on depth
  
# We could define the following: 
# The decent (in the decent to accent to decent) should be at least x m (on one side of the
# wiggle) and at least y m on the other side. This prevents 'steps' with a small decrease in 
# depth from being identified as wiggles. The thresholds are specified below, 
# and can be relaxed by setting to a smaller value (or zero - no threshold).
  
#--------------------------------
# Options
#--------------------------------
# Set the chosen wiggle threshold: what is the minimum depth difference (in m) for a wiggle
# wiggle_threshold = 0.5

# Given that the threshold (above) is satisfied (e.g., on the lead end) of the profile 
# what is the minimum depth difference for the other end (i.e., on the tail end) for a wiggle
# = wiggle_threshold2  
#--------------------------------

Halsey_wiggles <- function(subdat, wiggle_threshold = 0.5, wiggle_threshold2 = 0) {
  
  #--------------------------
  #Get the vertical speed
  #--------------------------
  # make depth negative for wiggle analysis 
  subdat$wig_depth = subdat$depth * -1
  
  subdat$vertical_speed_val <- c(NA, diff(subdat$wig_depth)/as.numeric(diff(subdat$date.time)))
  subdat$vertical_speed_val
  # make an index (1 or -1) from vertical_speed_val
  vertical_speed = NA
  subdat$vertical_speed[subdat$vertical_speed_val >= 0] <- 1    # ascent
  subdat$vertical_speed[subdat$vertical_speed_val < 0] <- -1    # decent
  
  #--------------------
  # Find change points
  #--------------------
  subdat$change_points = NA
  subdat$change_points[1:(nrow(subdat)-1)] <- diff(subdat$vertical_speed )
  # Set change points that are negative to -1 and those that are positive to 1
  subdat$change_points[subdat$change_points > 0] <- 1
  subdat$change_points[subdat$change_points < 0] <- -1
  subdat$change_points
  
  #---------------------
  # Find wiggles 
  #---------------------
  
  # Create columns for wiggles
  subdat$wiggle <- subdat$wiggle_period <- NA
  
  # Loop through all the 'negative' change points 
  # (i.e. changes from ascend to descend) and look for valid 'wiggles'
  
  for(ipt in which(subdat$change_points == -1)){
  
    # Wiggle START - the PREVIOUS closest positive change point (descend to ascend)
    wiggle_start <- which(subdat$change_points[1:ipt] == 1)
    
    # Wiggle END - the NEXT closest positive change point (descend to ascend)
    next_wiggle_ends <- which(subdat$change_points[ipt+1:nrow(subdat)] == 1)
    
    # Make sure there is a start and end to the wiggle within the dive
    if (length(next_wiggle_ends) > 0 & length(wiggle_start) > 0){
      # Get the row number of wiggle start
      wiggle_start = wiggle_start[length(wiggle_start)]
      
      # Get row number of wiggle end
      wiggle_end = ipt + next_wiggle_ends[1]
      
      # Get subdat associated with the wiggle
      wiggle_subdat = subdat[wiggle_start:wiggle_end,]
      
      # Get the range of depths in the wiggle and keep if > 2m (lead or lag) 
      d_delta_wiggle = max(wiggle_subdat$wig_depth) - min(wiggle_subdat$wig_depth)
      
      # Get the range of depths (in the wiggle and keep if > 0.5m (lead AND lag) 
      d_delta_wiggle1 = max(wiggle_subdat$wig_depth) - wiggle_subdat$wig_depth[1]
      d_delta_wiggle2 = max(wiggle_subdat$wig_depth) - tail(wiggle_subdat$wig_depth, n=1)
      d_delta_wiggle3 = min(d_delta_wiggle1, d_delta_wiggle2)
      
      # Now check if the wiggle amplitude is larger than the set threshold (wiggle_threshold)
      if (d_delta_wiggle > wiggle_threshold & 
          d_delta_wiggle3 > wiggle_threshold2){
        
        # Mark the peak of the wiggle
        wiggle_subdat$wiggle = 0
        wiggle_subdat$wiggle[which(wiggle_subdat$wig_depth == max(wiggle_subdat$wig_depth,na.rm=T))[1]] = 1
        
        # Mark the whole wiggle period
        wiggle_subdat$wiggle_period = 1
        
        # Set the columns in the original subdat frame
        subdat$wiggle[wiggle_start:wiggle_end] <- wiggle_subdat$wiggle
        subdat$wiggle_period[wiggle_start:wiggle_end] <- wiggle_subdat$wiggle_period
      }
    }
  }
  
  # Create a column to add up how many wiggles there are 

  subdat = subdat %>%
    mutate(wiggle_count = replace_na(wiggle, 0)) %>%
    mutate(wiggle_count = cumsum(wiggle_count))

  wiggleN_Halsey = max(subdat$wiggle_count)

  return(list(wiggleN_Halsey = wiggleN_Halsey,
              subdat_Halsey = subdat))
  
}
