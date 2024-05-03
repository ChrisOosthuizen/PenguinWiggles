
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
  
  if (nrow(subdat) > 3) {

# make depth negative for wiggle analysis 
  subdat$wig_depth = subdat$depth * -1
  
  wiggles_Takahashi <- rep(0, length(subdat$wig_depth))       # Initialize wiggle column with 0s
  
  for (i in 2:(length(subdat$wig_depth) - 1)) {
    if (subdat$wig_depth[i] < subdat$wig_depth[i - 1] &&subdat$wig_depth[i] < subdat$wig_depth[i + 1]) {
      wiggles_Takahashi[i] <- 1  # Set wiggle flag to 1 where wiggle occurs
    }
  }
  
  # Create a column to add up how many wiggles there are 

  # Add the wiggleN_Takahashi vector as a new column to subdat

  subdat$wiggles_Takahashi <- wiggles_Takahashi

  wiggleN_Takahashi <- sum(wiggles_Takahashi)

  return(list(wiggleN_Takahashi = wiggleN_Takahashi,
              subdat_Takahashi = subdat))
  
}

  # what should happen if the subdata is < 5 data points long?
  # Report that there are 0 wiggles (wiggleN_Simeone is the variable that gets saved 
  # in the other script)
  else {
    # If there are 10 or fewer rows, you can specify what to do in this block
    wiggleN_Takahashi <- list(wiggleN_Takahashi = 0,
                            subdat_Takahashi = subdat)
  }
}

