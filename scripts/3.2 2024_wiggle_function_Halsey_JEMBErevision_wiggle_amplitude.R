Halsey_wiggles_amplitude <- function(subdat, wiggle_threshold = 0.5, wiggle_threshold2 = 0) {
  
  #--------------------------
  # Get the vertical speed
  #--------------------------
  # Make depth negative for wiggle analysis 
  subdat$wig_depth <- subdat$depth * -1
  subdat$vertical_speed_val <- c(NA, diff(subdat$wig_depth) / as.numeric(diff(subdat$date.time)))
  
  # Create ascent/descent indicators
  subdat$vertical_speed <- NA
  subdat$vertical_speed[subdat$vertical_speed_val >= 0] <- 1    # ascent
  subdat$vertical_speed[subdat$vertical_speed_val < 0]  <- -1   # descent
  
  #--------------------------
  # Find change points
  #--------------------------
  subdat$change_points <- NA
  subdat$change_points[1:(nrow(subdat) - 1)] <- diff(subdat$vertical_speed)
  subdat$change_points[subdat$change_points > 0] <- 1
  subdat$change_points[subdat$change_points < 0] <- -1
  
  #--------------------------
  # Initialise wiggle columns
  #--------------------------
  subdat$wiggle <- subdat$wiggle_period <- NA
  subdat$wiggle_amplitude <- NA
  subdat$wiggle_id <- NA
  
  wiggle_id_counter <- 1
  
  #--------------------------
  # Loop through all candidate wiggles
  #--------------------------
  for (ipt in which(subdat$change_points == -1)) {
    
    # Find the most recent +1 before ipt
    wiggle_start_candidates <- which(subdat$change_points[1:ipt] == 1)
    
    # Find the next +1 after ipt
    next_wiggle_ends <- which(subdat$change_points[(ipt + 1):nrow(subdat)] == 1)
    
    if (length(wiggle_start_candidates) > 0 & length(next_wiggle_ends) > 0) {
      
      # Define start and end row indices
      wiggle_start <- wiggle_start_candidates[length(wiggle_start_candidates)]
      wiggle_end <- ipt + next_wiggle_ends[1]
      
      # Subset the data
      wiggle_subdat <- subdat[wiggle_start:wiggle_end, ]
      
      # Compute full amplitude (always recorded)
      d_delta_wiggle <- max(wiggle_subdat$wig_depth) - min(wiggle_subdat$wig_depth)
      subdat$wiggle_amplitude[wiggle_start:wiggle_end] <- d_delta_wiggle
      
      # Compute lead/lag constraints
      d_delta_wiggle1 <- max(wiggle_subdat$wig_depth) - wiggle_subdat$wig_depth[1]
      d_delta_wiggle2 <- max(wiggle_subdat$wig_depth) - tail(wiggle_subdat$wig_depth, n = 1)
      d_delta_wiggle3 <- min(d_delta_wiggle1, d_delta_wiggle2)
      
      # Threshold filter for valid wiggle
      if (d_delta_wiggle > wiggle_threshold & d_delta_wiggle3 > wiggle_threshold2) {
        
        # Mark the peak row
        wiggle_subdat$wiggle <- 0
        wiggle_subdat$wiggle[which(wiggle_subdat$wig_depth == max(wiggle_subdat$wig_depth, na.rm = TRUE))[1]] <- 1
        
        # Mark the period and assign ID
        wiggle_subdat$wiggle_period <- 1
        wiggle_subdat$wiggle_id <- wiggle_id_counter
        
        # Copy back to main dataframe
        subdat$wiggle[wiggle_start:wiggle_end] <- wiggle_subdat$wiggle
        subdat$wiggle_period[wiggle_start:wiggle_end] <- wiggle_subdat$wiggle_period
        subdat$wiggle_id[wiggle_start:wiggle_end] <- wiggle_subdat$wiggle_id
        
        # Update max dive depth (optional)
        subdat$max_dive_depth <- min(subdat$wig_depth)
        
        # Increment counter
        wiggle_id_counter <- wiggle_id_counter + 1
      }
    }
  }
  
  #--------------------------
  # Count the number of wiggles
  #--------------------------
  subdat <- subdat %>%
    mutate(wiggle_count = replace_na(wiggle, 0)) %>%
    mutate(wiggle_count = cumsum(wiggle_count))
  
  wiggleN_Halsey <- max(subdat$wiggle_count, na.rm = TRUE)
  
  #--------------------------
  # Return
  #--------------------------
  return(list(wiggleN_Halsey = wiggleN_Halsey,
              subdat_Halsey = subdat))
}
