
#-----------------------------------------------------------------------------
# Chris Oosthuizen
# March 2024
# Penguin wiggles
#-----------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# Calculate wiggles following Takahashi et al. 2004 
#----------------------------------------------------------------------------------

# Takahashi, A., Dunn, M.J., Trathan, P.N., Croxall, J.P., Wilson, R.P., Sato, K. and
# Naito, Y., 2004. Krill-feeding behaviour in a chinstrap penguin compared to fish-eating 
# in Magellanic penguins: a pilot study. Marine Ornithology, 32, pp.47-54.

# A “depth wiggle” was defined as the event when birds changed their swim direction
# from descending to ascending. 

# You'll want to identify instances where the current depth is shallower than the previous depth 
# (indicating the bird is ascending) and the next depth is deeper than the current depth
# (indicating the bird is beginning to descend). 

#-----------------------------------------------------
# Function to identify times when depth wiggles occur
#-----------------------------------------------------

Takahashi_wiggles <- function(subdat) {
  
    # make depth negative for wiggle analysis 
    subdat$wig_depth = subdat$depth * -1

    depth_diff <- diff(subdat$wig_depth)  # Calculate differences between successive depth values

    # Find indices where the depth changes from descending (negative diff) to ascending (positive diff)
    wiggle_points <- which(depth_diff[-1] >= 0 & depth_diff[-length(depth_diff)] < 0) + 1
    
    # Initialize the wiggle column with 0
    subdat$wiggles_Takahashi <- 0
    
    # Set the wiggle points to 1
    subdat$wiggles_Takahashi[wiggle_points] <- 1
    
    wiggleN_Takahashi <- sum(subdat$wiggles_Takahashi)
    
    return(list(wiggleN_Takahashi = wiggleN_Takahashi,
                subdat_Takahashi = subdat))
    
  }


