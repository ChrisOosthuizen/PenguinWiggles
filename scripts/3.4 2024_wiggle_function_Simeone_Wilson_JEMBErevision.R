

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
# wiggle_threshold <- 0.3

Simeone_wiggles <- function(subdat, wiggle_threshold = 0.3) {
  
  # the code breaks down if there are fewer than 5 rows in a dive.
  # add this if statement; at the end, the 'else' returns a 0 (0 wiggles) for dives < 5 data points
  
  if (nrow(subdat) > 4) {
    
    #-----------------------------------
    # Get the (absolute) vertical speed
    #-----------------------------------
    # make depth negative for wiggle analysis 
    subdat$wig_depth = subdat$depth * -1
    
    # 1. Pre-process to find vertical speed and changepoints
    subdat <- subdat %>%
      # Step 1.1: Calculate vertical speed
      mutate(vertical_speed_1s = wig_depth - lag(wig_depth)) %>%
      
      # Step 1.2a: Mean change in depth over 3s
      mutate(
        vertical_speed_mean_3s = slide_dbl(         # Rolling means with 'slider' library (intuitive option)
          abs(vertical_speed_1s),                   # NOTE - here we use absolute values
          .f = ~ round(mean(.x, na.rm = FALSE), 2),
          .before = 3,                              # Can adjust the value - i.e. 3 = include three previous points
          .after = -1,                               # Values: -1 exclude current point; 0 include current point
          .complete = TRUE),
        
        #Step 1.2b:Difference between current v-speed and previous 3s 
        vertical_speed_diff = round(abs(vertical_speed_1s) - abs(vertical_speed_mean_3s),2)) %>%  # NOTE - here we use absolute values
      
      # Step 1.3 - Changepoints  
      # Step 1.3a: Indicate direction of vertical speed threshold
      mutate(
        change_points = case_when(
          vertical_speed_1s >= 0 ~ 1,
          vertical_speed_1s < 0 ~ -1,
          TRUE ~ NA_real_ )) %>%
      
      # Step 1.3b: Find changepoints
      mutate(change_points =  lead(change_points) - change_points) 
    
    
    # 2. 
    # Give change points names  
    subdat <- subdat %>%
      mutate(start_end = case_when(
        change_points == 2 ~ "start",
        change_points == -2 ~ "end",
        change_points == 0 ~ "0"))   %>%
    # Make sure names are factors  
      mutate(start_end = factor(start_end, levels = c("start", "end", "0")))  %>%
      
    # Add row numbers to track position
      mutate(row = row_number())
    
    # Identify start and end positions
     starts <- subdat %>% filter(start_end == "start") %>% pull(row)
     ends <- subdat %>% filter(start_end == "end") %>% pull(row)
    
    # Initialize list to hold valid pairs
     valid_pairs <- list()
     used_ends <- logical(length(ends))  # track which ends have been used
    
    for (s in starts) {
      next_end_idx <- which(ends > s & !used_ends)[1]
      if (!is.na(next_end_idx)) {
        e <- ends[next_end_idx]
        valid_pairs <- append(valid_pairs, list(c(s, e)))
        used_ends[next_end_idx] <- TRUE
      }
    }
    
    # Convert to a data frame

    if (length(valid_pairs) > 0) {
      valid_pairs_df <- do.call(rbind, valid_pairs) %>%
        as.data.frame() %>%
        setNames(c("start", "end"))
      
      # Initialize start-end pairs
      subdat$s_e <- 0
      
      for (i in seq_len(nrow(valid_pairs_df))) {
        s <- valid_pairs_df$start[i]
        e <- valid_pairs_df$end[i]
        subdat$s_e[subdat$row >= s & subdat$row <= e] <- 1
      }
      
    } else {
      # No valid start–end pairs, so all values are 0
      subdat$s_e <- 0
    }
    

    # First assign 0
    subdat$s_e_num <- 0
    
    # If there are valid pairs, assign a unique number to each
    if (exists("valid_pairs_df")) {
      for (i in seq_len(nrow(valid_pairs_df))) {
        s <- valid_pairs_df$start[i]
        e <- valid_pairs_df$end[i]
        subdat$s_e_num[subdat$row >= s & subdat$row <= e] <- i
      }
    }
      
# 3. Now we process to find wiggles
      #First set of conditions
      # 3.1 First, find candidate rows for wiggles
    subdat = subdat %>% 
      mutate(
        wiggle_SW = ifelse(
          !is.na(vertical_speed_mean_3s) &  # 1. Is the difference between current and previous (3s) vertical speed > the wiggle threshold (0.3m)
            abs(vertical_speed_diff) > wiggle_threshold,   
          #Option below - didn't use the absolute value
      #   (vertical_speed_diff) >= wiggle_threshold,   
          1, 0
        )
      )
    
  # Calculate how far PCE and wiggles are from the deepest point in the dive. 
    
    # Get max depth of the dive:
    subdat$max_dive_depth = min(subdat$wig_depth)
    
    subdat <- subdat %>%
      mutate(depth_diff = if_else(PCE_1hz > 0, depth*-1 - max_dive_depth, NA_real_),
             depth_ratio = if_else(PCE_1hz > 0, round(depth*-1 / max_dive_depth, 2), NA_real_),
             depth_ratio_wiggle = if_else(wiggle_SW > 0, round(depth*-1 / max_dive_depth, 2), NA_real_))
    # Use a 60 % depth threshold - wiggles must be over 60 % of max depth in the dive to be valid.
    subdat <- subdat %>%
      mutate(wiggle_SW_bottom = if_else(is.na(depth_ratio_wiggle) | depth_ratio_wiggle < 0.6, 0, wiggle_SW))
    
  # Step 1: Filter first to check if there are any relevant rows
    valid_depth_rows <- subdat %>%
      filter(s_e_num > 0, wiggle_SW == 1)
    
    if (nrow(valid_depth_rows) == 0) {
      # No valid data for summarising — set is_max = 0 for all rows
      subdat <- subdat %>%
        mutate(is_max = 0)
    } else {
      # Step 2: summarise and flag rows with max depth
      depth_max_df <- valid_depth_rows %>%
        group_by(s_e_num) %>%
        summarise(max_depth = max(wig_depth, na.rm = TRUE), .groups = "drop")
      
      subdat <- subdat %>%
        left_join(depth_max_df, by = "s_e_num") %>%
        group_by(s_e_num) %>%
        mutate(
          is_max = {
            candidates <- which(wiggle_SW == 1 & wig_depth == max_depth)
            flag <- rep(FALSE, n())
            if (length(candidates) > 0) flag[candidates[1]] <- TRUE
            as.integer(flag)
          }
        ) %>%
        ungroup() %>%
        select(-max_depth)
    }
    

subdat <- subdat %>%
  mutate(all_wig_in_s_e = if_else(s_e ==1 & wiggle_SW == 1, 1, 0))

# Apply the function to the column
subdat <- subdat %>%
  mutate(all_wig_cooldown = cooldown_filter(all_wig_in_s_e))


SW_s_e_max = max(subdat$s_e_num)
SW_one_wig_in_s_e = sum(subdat$is_max)
SW_all_wig_in_s_e = sum(subdat$all_wig_in_s_e)
SW_all_wig_cooldown = sum(subdat$all_wig_cooldown)
wiggle_SW = sum(subdat$wiggle_SW)
wiggle_SW_bottom = sum(subdat$wiggle_SW_bottom)


    return(list(SW_s_e_max = SW_s_e_max,
                SW_one_wig_in_s_e = SW_one_wig_in_s_e,
                SW_all_wig_in_s_e = SW_all_wig_in_s_e,
                SW_all_wig_cooldown = SW_all_wig_cooldown,
                wiggle_SW =  wiggle_SW,
                wiggle_SW_bottom = wiggle_SW_bottom,
                subdat_Simeone = subdat))
    
  } 
  
  # what should happen if the subdata is < 5 data points long?
  # Report that there are 0 wiggles (wiggleN_Simeone is the variable that gets saved 
  # in the other script)
  else {
    # If there are 10 or fewer rows, you can specify what to do in this block
    wiggleN_Simeone <- list(wiggleN_Simeone = 0,
                            SW_s_e_max = 0,
                            SW_one_wig_in_s_e = 0,
                            SW_all_wig_in_s_e = 0,
                            SW_all_wig_cooldown = 0,
                            wiggle_SW = 0,
                            wiggle_SW_bottom = 0,
                            subdat_Simeone = subdat)
  }
}


# Cooldown function

cooldown_filter <- function(x, cooldown = 3) {
  out <- x
  i <- 1
  while (i <= length(x)) {
    if (x[i] == 1) {
      end_idx <- min(i + cooldown, length(x))  # prevent overshoot
      out[(i + 1):end_idx] <- 0
      i <- end_idx + 1
    } else {
      i <- i + 1
    }
  }
  out
}

