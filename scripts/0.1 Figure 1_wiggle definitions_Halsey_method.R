
# Figure 1 in Oosthuizen et al. 

library(patchwork)
library(tidyverse)

#------------------------
# Set Plotting theme
#------------------------
source('./scripts/0.1 ggplot theme.R') 

#------------------------------------
# Create a dive profile data frame
#------------------------------------
# Profile is based on Figure 2 in Halsey et al. 2007 (Polar Biol (2007) 30:991–1004)
# time is at 2 Hz; depth in m

data <- data.frame(
          time = seq(0, 99.5, 0.5), 
          depth = c(
          -0.57,  -1.66, -3.07,  -5.23, -10.09, -13.80, -16.03, -17.49, -17.97, -18.15, -18.48, -18.67, -18.79,
          -19.11, -19.13, -19.86, -21.83, -23.60, -25.31, -27.17, -28.92, -30.37, -31.66, -32.98, -34.38, -35.87,
          -37.35, -38.70, -39.79, -40.51, -40.95, -41.17, -41.28, -41.37, -41.51, -41.81, -42.33, -43.06, -43.95,
          -44.96, -45.97, -46.89, -47.66, -48.26, -48.64, -48.79, -48.71, -48.46, -48.10, -47.68, -47.26, -46.93,
          -46.79, -46.89, -47.19, -47.65, -48.18, -48.75, -49.28, -49.70, -49.97, -50.07, -50.04, -49.89, -49.64,
          -49.31, -48.95, -48.60, -48.32, -48.11, -47.91, -47.65, -47.29, -46.85, -46.38, -45.94, -45.58, -45.33,
          -45.18, -45.13, -45.18, -45.32, -45.56, -45.88, -46.28, -46.71, -47.10, -47.39, -47.49, -47.41, -47.16,
          -46.78, -46.31, -45.77, -45.19, -44.61, -44.01, -43.41, -42.90, -42.64, -42.75, -43.25, -44.01, -44.88,
          -45.72, -46.39, -46.74, -46.62, -45.91, -44.65, -43.05, -41.30, -39.50, -37.76, -36.16, -34.51, -32.76,
          -31.19, -30.07, -29.43, -29.30, -29.62, -30.22, -30.86, -31.32, -31.37, -30.81, -29.68, -28.23, -26.73,
          -25.36, -24.20, -23.33, -22.91, -22.86, -22.93, -22.92, -22.81, -22.73, -22.85, -22.74, -21.57, -19.91,
          -18.61, -17.99, -17.80, -17.75, -17.60, -17.51, -17.66, -17.72, -17.48, -17.58, -17.79, -17.70, -17.24,
          -16.51, -15.65, -14.80, -14.09, -13.65, -13.51, -13.64, -13.97, -14.44, -15.01, -15.62, -16.18, -16.62,
          -16.82, -16.71, -16.33, -15.73, -14.99, -14.17, -13.41, -12.83, -12.56, -12.73, -13.45, -14.54, -15.69,
          -16.56, -16.83, -16.23, -15.23, -14.51, -13.78, -12.77, -11.49,  -9.95,  -8.26,  -6.59,  -5.13,  -3.96,
          -3.05, -2.33,  -1.74,  -1.21,  -0.67))

# add row numbers to help track later change points
data = data %>%
  mutate(RowNumber = row_number())

plot(data$time, data$depth, type = "l")

#----------------------------------------------------------------------------------
# Calculate wiggles following Halsey et al. 2007 
#----------------------------------------------------------------------------------
# From Halsey et al. 2007 : Polar Biol (2007) 30:991–1004
# Wiggles occur where an increase in depth over time 
# changes to a decrease in depth 
# and then back to an increase in depth (thus: decent to accent to decent). 
# This creates a short period in the dive profile that is concave in shape. 
# Thus, wiggles are defined by 3 points of the vertical speed that passes below (or through) 0 ms–1).
# If useful, certain wiggles could be ignored, e.g. using a threshold based on depth

# We define the following: 
# The decent (in the decent to accent to decent) should be at least 2 m (on one side of the
# wiggle) and at least 0.5 m on the other side. This prevents 'steps' with a small decrease in 
# depth from being identified as wiggles. The thresholds are specified below, 
# and can be relaxed by setting to a smaller value (or zero - no threshold).

#--------------------------------
# Set the chosen wiggle threshold
#--------------------------------
# what is the minimum depth difference (in m) for a wiggle
wiggle_threshold = 2   

# Given that the 2 m threshold (above) is satisfied (e.g., on the lead end) of the profile 
# what is the minimum depth difference for the other end (i.e., on the tail end) for a wiggle
# (0.3 m - Simeone and Wilson (2003))
wiggle_threshold2 = 0.3 

#--------------------------
#Get the vertical speed
#--------------------------
data$vertical_speed_val <- c(NA, diff(data$depth)/as.numeric(diff(data$time)))
data$vertical_speed_val
# make an index (1 or -1) from vertical_speed_val
data$vertical_speed[data$vertical_speed_val >= 0] <- 1    # ascent
data$vertical_speed[data$vertical_speed_val < 0] <- -1    # decent

#--------------------
# Find change points
#--------------------
data$change_points = NA
data$change_points[1:(nrow(data)-1)] <- diff(data$vertical_speed )
# Set change points that are negative to -1 and those that are positive to 1
data$change_points[data$change_points > 0] <- 1
data$change_points[data$change_points < 0] <- -1
data$change_points

#---------------------
# Find wiggles 
#---------------------

#Create columns for wiggles
data$wiggle <- data$wiggle_period <- NA

# Loop through all the 'negative' change points 
# (i.e. changes from ascend to descend) and look for valid 'wiggles'

for(ipt in which(data$change_points == -1)){
  # ipt =  which(data$change_points == -1)[2] #DEBUGGING
  
  #Wiggle START - the PREVIOUS closest positive change point (descend to ascend)
  wiggle_start <- which(data$change_points[1:ipt] == 1)#[length(which(data$change_points[1:ipt] == 1))]
  
  #Wiggle END - the NEXT closest positive change point (descend to ascend)
  next_wiggle_ends <- which(data$change_points[ipt+1:nrow(data)] == 1)#[1]
  
  #Make sure there is a start and end to the wiggle within the dive
  if (length(next_wiggle_ends) > 0 & length(wiggle_start) > 0){
    #Get the row number of wiggle start
    wiggle_start = wiggle_start[length(wiggle_start)]
    
    #Get row number of wiggle end
    wiggle_end = ipt + next_wiggle_ends[1]
    
    #Get data associated with the wiggle
    wiggle_data = data[wiggle_start:wiggle_end,]
    
    #Get the range of depths in the wiggle and keep if > 2m (lead or lag) 
    d_delta_wiggle = max(wiggle_data$depth) - min(wiggle_data$depth)
    
    #Get the range of depths (in the wiggle and keep if > 0.5m (lead AND lag) 
    d_delta_wiggle1 = max(wiggle_data$depth) - wiggle_data$depth[1]
    d_delta_wiggle2 = max(wiggle_data$depth) - tail(wiggle_data$depth, n=1)
    d_delta_wiggle3 = min(d_delta_wiggle1, d_delta_wiggle2)
  
    #Now check if the wiggle amplitude is larger than the set threshold (wiggle_threshold)
    if (d_delta_wiggle > wiggle_threshold & 
        d_delta_wiggle3 > wiggle_threshold2){
      
      #Mark the peak of the wiggle
      wiggle_data$wiggle = 0
      wiggle_data$wiggle[which(wiggle_data$depth == max(wiggle_data$depth,na.rm=T))[1]] = 1
      
      #Mark the whole wiggle period
      wiggle_data$wiggle_period = 1
      
      #Set the columns in the original data frame
      data$wiggle[wiggle_start:wiggle_end] <- wiggle_data$wiggle
      data$wiggle_period[wiggle_start:wiggle_end] <- wiggle_data$wiggle_period
    }
  }
}

# Create a column to add up how many wiggles there are 
data = data %>%
  mutate(wiggle_count = replace_na(wiggle, 0)) %>%
  mutate(wiggle_count = cumsum(wiggle_count))


# Quick plot to show the identified wiggles
plot(data$time, data$depth, type = "l")
points(data$time[data$wiggle_period == 1], data$depth[data$wiggle_period == 1],col = "grey",pch = 16)
points(data$time[data$wiggle == 1], data$depth[data$wiggle == 1],col = 3,pch = 16)
points(data$time, data$change_points,col = 2,pch = 16)
points(data$time[data$change_points == 1], data$depth[data$change_points == 1],col = 4,pch = 16)

#--------------------
# Build Figure 1 
#--------------------

#----------------------
# ggplot dive profile
#----------------------

# Make vectors to draw lines for each wiggle 
w1 <- tibble(y = c(-52,-54,-54,-52),x =c(22.5, 22.5,30.5,30.5))
w2 <- tibble(y = c(-52,-54,-54,-52),x =c(30.5,30.5, 44.0,44.0))
w3 <- tibble(y = c(-52,-54,-54,-52),x =c(44.0,44.0, 53.0,53.0))
w4 <- tibble(y = c(-52,-54,-54,-52),x =c(53.0,53.0, 62.5,62.5))
w5 <- tibble(y = c(-52,-54,-54,-52),x =c(76.5,76.5, 84.5, 84.5))
w6 <- tibble(y = c(-52,-54,-54,-52),x =c(84.5,84.5, 91.5, 91.5))


profile = ggplot(data = data, aes(x = time, y = depth)) + 
  geom_line() + 
  geom_point(data=subset(data, wiggle_period ==  1),
             aes(x = time, y = depth, group = "Wiggles"), 
             col = "navy", shape = 1, alpha = 0.6, size  = 1) + 
  geom_point(data=subset(data, wiggle ==  1),
             aes(x = time, y = depth, group = "Wiggle peaks"), 
             fill = "navy", col = "red", shape = 21, size = 2) +
  geom_point(data=subset(data, change_points ==  1),
             aes(x = time, y = depth, group = "Change points"), 
             col = "red", shape = 16, size = 2) +
  gg_theme()+
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"), 
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("Time (sec)") +
  ylab("Depth (m)")+
  scale_x_continuous(breaks = seq(0,100, by = 20))+
  geom_line(data = w1, aes(x = x, y = y, group = 1)) +
  geom_line(data = w2, aes(x = x, y = y, group = 1))+
  geom_line(data = w3, aes(x = x, y = y, group = 1)) +
  geom_line(data = w4, aes(x = x, y = y, group = 1)) + 
  geom_line(data = w5, aes(x = x, y = y, group = 1)) +
  geom_line(data = w6, aes(x = x, y = y, group = 1))+
  annotate("text", y = -52.6, x = 26, label = "W1", size = 2) +
  annotate("text", y = -52.6, x = 39.5, label = "W2", size = 2) +
  annotate("text", y = -52.6, x = 49.5, label = "W3", size = 2) +
  annotate("text", y = -52.6, x = 60, label = "W4", size = 2) +
  annotate("text", y = -52.6, x = 80.5, label = "W5", size = 2) +
  annotate("text", y = -52.6, x = 88.5, label = "W6", size = 2) +
  annotate("text", y = -6, x = 41, label = "Wiggle", size = 3, color = "navy")+ 
  annotate("text", y = -10, x = 45, label = "Change point", size = 3, color = "red") +
  annotate("point", y = -6, x = 33,  size = 3, fill = "navy", col = "red", shape = 21)+
  annotate("point", y = -6, x = 30,  size = 2, color = "navy", shape = 1)+
  annotate("point", y = -10, x = 33,  size = 3, color = "red", shape = 16)

profile

#-------------------------
# ggplot vertical speed 
#-------------------------
# Make vectors to draw lines for each wiggle 

w1 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(22.5, 22.5,30.5,30.5))
w2 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(30.5,30.5, 44.0,44.0))
w3 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(44.0,44.0, 53.0,53.0))
w4 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(53.0,53.0, 62.5,62.5))
w5 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(76.5,76.5, 84.5, 84.5))
w6 <- tibble(y = c(-2.4,-3.2,-3.2,-2.4),x =c(84.5,84.5, 91.5, 91.5))


speed = ggplot(data = data, aes(x = time, y = vertical_speed_val)) + 
  geom_line() + 
  gg_theme()+
  theme(legend.position = "inside", legend.position.inside =c(0.7, 0.2),
        legend.title = element_blank(),
        plot.margin=grid::unit(c(1,1,1,1), "mm"))+
  geom_point(data=subset(data, wiggle_period ==  1),
             aes(x = time, y = vertical_speed_val, group = "Wiggles"), col = "navy", shape = 1, alpha = 0.6, size  = 1) + 
  geom_point(data=subset(data, wiggle ==  1),
             aes(x = time, y = vertical_speed_val, group = "Wiggle peaks"), fill = "navy", col = "red", shape = 21, size = 2) +
  geom_point(data=subset(data, change_points ==  1),
             aes(x = time, y = vertical_speed_val, group = "Change points"), col = "red", shape = 16, size = 2) +
  labs(x = "Time (sec)",
       y = bquote('Vertical speed' ~ (m.s^-1)))+
  ylim(-10,5.4)+
  scale_x_continuous(breaks = seq(0,100, by = 20))+
  geom_hline(yintercept = 0, col = "black")+
  geom_line(data = w1, aes(x = x, y = y, group = 1)) +
  geom_line(data = w2, aes(x = x, y = y, group = 1))+
  geom_line(data = w3, aes(x = x, y = y, group = 1)) +
  geom_line(data = w4, aes(x = x, y = y, group = 1)) + 
  geom_line(data = w5, aes(x = x, y = y, group = 1)) +
  geom_line(data = w6, aes(x = x, y = y, group = 1)) + 
  annotate("text", y = -2.8, x = 26, label = "W1", size = 2) +
  annotate("text", y = -2.8, x = 39.5, label = "W2", size = 2) +
  annotate("text", y = -2.8, x = 49.5, label = "W3", size = 2) +
  annotate("text", y = -2.8, x = 60, label = "W4", size = 2) +
  annotate("text", y = -2.8, x = 80.5, label = "W5", size = 2) +
  annotate("text", y = -2.8, x = 88.5, label = "W6", size = 2) +
  annotate("text", y = -6, x = 41, label = "Wiggle", size = 3, color = "navy")+ 
  annotate("text", y = -7, x = 45, label = "Change point", size = 3, color = "red") +
  annotate("point", y = -6, x = 33,  size = 3, fill = "navy", col = "red", shape = 21)+
  annotate("point", y = -6, x = 30,  size = 2, color = "navy", shape = 1)+
  annotate("point", y = -7, x = 33,  size = 3, color = "red", shape = 16)

speed

profile / speed +
  plot_layout(ncol = 1, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5)))

#----------------------------
# ggplot of the first wiggle
#----------------------------

# subset data
w = data[43:65,]
# Make vector to draw line for this wiggle 
ww1 <- tibble(y = c(-50.2,-50.4,-50.4,-50.2),x =c(22.5, 22.5,30.5,30.5))

wiggle = ggplot(data = w, aes(x=time, y = depth)) +
  annotate("rect", xmin = 22.5, xmax = 23, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8)+
  annotate("rect", xmin = 26, xmax = 26.5, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8)+
  annotate("rect", xmin = 30.5, xmax = 31, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8) + 
  geom_line() + 
  gg_theme()+
  theme(plot.margin=grid::unit(c(1,1,1,1), "mm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("Time (sec)") +
  ylab("Depth (m)")+
  scale_x_continuous(breaks = seq(21,32, by = 2))+
  geom_point(data=subset(w, wiggle_period ==  1),
             aes(x = time, y = depth, group = "Wiggles"), col = "navy", shape = 1, alpha = 0.6, size  = 2) + 
  geom_point(data=subset(w, wiggle ==  1),
             aes(x = time, y = depth, group = "Wiggle peaks"), fill = "navy", col = "red", shape = 21, size = 2.5) +
  geom_point(data=subset(w, change_points ==  1),
             aes(x = time, y = depth, group = "Change points"), col = "red", shape = 16, size = 2.5) +
  annotate("text", y = -50.2, x = 26, label = "W1", size = 2) +
  geom_line(data = ww1, aes(x = x, y = y, group = 1))+
  geom_text(data=subset(w, change_points ==  1),
           aes(x = time, y = depth, group = "Change points",
           label = c("1", "3")), vjust = 1.7, 
           hjust = -0.4, size = 3) + 
  geom_text(data=subset(w, wiggle ==  1),
          aes(x = time, y = depth, group = "Wiggle peaks",
              label = c("2")), vjust = 1.7, 
          hjust = -0.4, size = 3)
  
wiggle 

#----------------------------------------------
# ggplot of the first wiggle's vertical speed
#----------------------------------------------

# Make vector to draw line for this wiggle 
vw1 <- tibble(y = c(-1.6,-1.7,-1.7,-1.6),x =c(22.5, 22.5,30.5,30.5))

vspeed = ggplot(data = w, aes(x=time, y = vertical_speed_val)) +
  annotate("rect", xmin = 22.5, xmax = 23, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8)+
  annotate("rect", xmin = 26, xmax = 26.5, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8)+
  annotate("rect", xmin = 30.5, xmax = 31, ymin = -Inf, ymax = Inf,fill = "grey", alpha = 0.8) + 
  geom_line() + 
  geom_hline(yintercept = 0, col = "black") + 
  gg_theme()+
  labs(x = "Time (sec)",
       y = bquote('Vertical speed' ~ (m.s^-1))) + 
  scale_x_continuous(breaks = seq(21,32, by = 2))+
  annotate("text", y = -1.6, x = 26, label = "W1", size = 2) +
  geom_line(data = vw1, aes(x = x, y = y, group = 1)) +
  geom_point(data=subset(w, wiggle_period ==  1),
             aes(x = time, y = vertical_speed_val, group = "Wiggles"), col = "navy", shape = 1, alpha = 0.6, size  = 2) + 
  geom_point(data=subset(w, wiggle ==  1),
             aes(x = time, y = vertical_speed_val, group = "Wiggle peaks"), fill = "navy", col = "red", shape = 21, size = 2.5) +
  geom_point(data=subset(w, change_points ==  1),
             aes(x = time, y = vertical_speed_val, group = "Change points"), col = "red", shape = 16, size = 2.5) +
  
  geom_text(data=subset(w, change_points ==  1),
            aes(x = time, y = vertical_speed_val, group = "Change points",
                label = c("1", "3")), vjust = 1.7, 
            hjust = -0.4, size = 3) + 
  geom_text(data=subset(w, wiggle ==  1),
            aes(x = time, y = vertical_speed_val, group = "Wiggle peaks",
                label = c("2")), vjust = 1.7, 
            hjust = 1, size = 3)

vspeed

wiggle / vspeed

# patchwork plot of all 4 figures
com_plot =  profile / wiggle /  speed / vspeed +
          plot_layout(ncol = 2, heights = c(1, .4) & theme(plot.margin = c(-5, -5, -5, -5)))+ 
          plot_annotation(tag_levels = 'A')

## Save pdf plot 
pdf("./figures/Figure1.pdf",
       useDingbats = FALSE, width = 11, height = 7)
com_plot
dev.off()


# Save as PNG
ggsave("./figures/Figure1.png", com_plot, width = 10, height = 8, dpi = 1000)


