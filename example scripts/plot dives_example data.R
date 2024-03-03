
# plot example TDR data

library(tidyverse)

# What files are there in the example data directory? 
example_individuals <- list.files("./example_data/",
                     pattern = "*rds", 
                     full.names = TRUE)

# print the name of each file
for (example_animal in example_individuals) {
  print(example_animal)
}

for (example_animal in example_individuals) {
  
  dive_timeseries <- readRDS(example_animal) 
  
  #---------------------------
  # Plot video prey captures
  #---------------------------
  
  Fig = ggplot(data = dive_timeseries, 
                 aes(x = date.time, y = depth)) + 
    geom_line() +
    geom_point(data=subset(dive_timeseries, PCE_1hz > 0),
               aes(x = date.time, y = depth), 
               fill = "red", col = "azure", shape = 21, size = 2) +
    theme_bw()+
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M"), breaks = "1 hour")+
    xlab("Time") +
    ylab("Depth (m)")+
    theme(legend.position = "none") +
    annotate("text", x = min(dive_timeseries$date.time), 
             y = min(dive_timeseries$depth),
             label = "Prey capture (video)", 
             size = 3, color = "black", hjust = 0)+
    annotate("point", x = min(dive_timeseries$date.time)-220, 
             y = min(dive_timeseries$depth),
             size = 2.5, 
             fill = "red", col = "azure", shape = 21)
    
  
  Fig
  
  
ID = as.numeric(gsub("\\D", "", example_animal))

# Save as PNG
  ggsave(paste0("./example_data/video_prey_captures_", ID, ".png"), 
         Fig, width = 8, height = 6, dpi = 400)
  
}

