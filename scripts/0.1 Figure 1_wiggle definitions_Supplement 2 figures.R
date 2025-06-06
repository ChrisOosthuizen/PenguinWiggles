
# Run this script together with 4.1 2024_wiggle analysis and figures_JEMBErevision
# We need the data from that script for the below (run to line 1500)

#--------------------
# Prey captures plot
#--------------------

# Define your own colors for different dive phases (diveMove)
my_colors <- c("D" = "slateblue", 
               "DB" = "slateblue",
               "B" = "darkorange1",
               "BA" = "darkorange", 
               "A" = "cadetblue",
               "DA" = "cadetblue",
               "X" = "azure3")

# Wiggles:
# Select a single dive from 1 animal as an example
S2_w = subdat_Halsey_1m %>% 
          dplyr::filter(id == "2023_01_14_DI4" &
          dive.nr == 80)

S2_w <- S2_w[15:(nrow(S2_w)-15), ]  # drop first 5 and last 5 rows

# Add the wiggle amplidude to the data to plot
source('./scripts/3.2 2024_wiggle_function_Halsey_JEMBErevision_wiggle_amplitude.R') 
S2_w = Halsey_wiggles_amplitude(S2_w, wiggle_threshold = 1, wiggle_threshold2 = 0)
S2_w = S2_w$subdat_Halsey

# get video prey captures for this animal
S2_pce = divedat %>%
           dplyr::filter(id == "2023_01_14_DI4" & 
                  date.time >= min(S2_w$date.time) &
                  date.time <= max(S2_w$date.time))

# plot video prey captures (plot bottom of the dive only for better resolution)
A = ggplot(data = S2_pce, 
              aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.6, size = 2, shape = 15) +
  scale_color_manual(values = my_colors, name = "Dive\nphase")+
  geom_point(data = subset(S2_pce, PCE_1hz > 0), aes(x = date.time, y = depth*-1), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +
  gg_theme() + 
  xlab("Time") +
  ylab("Depth (m)") +
  ylim(-68,-48)+
 annotate("text", x = max(S2_pce$date.time) - 50, y = -50, label = "Video prey captures", hjust = 0, 
          size = 3, color = "black")+
 annotate("point", x = max(S2_pce$date.time)- 53, y = -50,size = 2.5, 
          fill = "red", col = "navy", shape = 21) + 
 coord_cartesian(
    xlim = c(min(S2_w$date.time) + 10, 
             max(S2_w$date.time) - 10)) + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.18, 0.8),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "transparent", color = NA, linewidth = 0.5),
        legend.direction = "horizontal") + 
   theme(legend.title=element_text(size=8)) + 
   theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())

A

# plot Halsey 1 m wiggles
B = ggplot(data = S2_pce, 
           aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
 
   geom_point(data = subset(S2_w, wiggle_period > 0), aes(x = date.time, y = depth*-1), 
            col = "navy", shape = 1, alpha = 1, size  = 2) +
  
  geom_point(data = subset(S2_w, wiggle > 0), aes(x = date.time, y = depth*-1), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  xlab("Time") +
  ylab("Depth (m)") +
  ylim(-68,-48)+
  annotate("text", x = max(S2_pce$date.time) - 50, y = -50, label = "Three-fold changepoint wiggles (1m)", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = -50,size = 2.5, 
           fill = "navy", col = "red", shape = 21) +
  annotate("text", x = max(S2_pce$date.time) - 50, y = -52, label = "Wiggle period", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = -52,size = 2.5, 
           col = "navy", shape = 1)+ 
  coord_cartesian(
    xlim = c(min(S2_w$date.time) + 10, 
             max(S2_w$date.time) - 10)) + 
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())


#B

C = ggplot(data = S2_w, 
           aes(x = date.time, y = vertical_speed_val)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
  
  geom_point(data = subset(S2_w, change_points != 0), aes(x = date.time, y = vertical_speed_val), 
             fill = "red", col = "navy", shape = 21, alpha = 1, size  = 2) +

  geom_point(data = subset(S2_w,  wiggle > 0), aes(x = date.time, y = vertical_speed_val), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  labs(x = "Time",
       y = bquote('Vertical speed' ~ (m.s^-1)))+
  
  annotate("text", x = max(S2_pce$date.time) - 50, y = 2, label = "Three-fold changepoint wiggles (1m)", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = 2,size = 2.5, 
           fill = "navy", col = "red", shape = 21) + 
  
  annotate("text", x = max(S2_pce$date.time) - 50, y = 1.5, label = "Changepoints", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = 1.5,size = 2.5, 
           fill = "red", col = "navy", shape = 21) + 
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
 
  geom_text(data=S2_w,
            aes(x = date.time, y = vertical_speed_val,  label = round(wiggle_amplitude,1)),
             vjust = 1.7, hjust = -0.4, size = 3) +
  coord_cartesian(xlim = c(min(S2_w$date.time) + 10, 
             max(S2_w$date.time) - 10))
 # C

A <- A + theme(plot.margin = margin(1, 2, 1, 2))
B <- B + theme(plot.margin = margin(1, 2, 1, 2))
C <- C + theme(plot.margin = margin(1, 2, 1, 2))

S2_halsey1m <- (A / B / C) + 
    plot_annotation(tag_levels = 'A')  

S2_halsey1m 

# Save as PNG
ggsave("./supplement/FigureS2_3.png", S2_halsey1m , width = 7 , height = 9, dpi = 1000)



#--------------------
# Prey captures plot
#--------------------
# Wiggles:
# Select a single dive from 1 animal as an example
S2w_T = subdat_Takahashi %>% 
  dplyr::filter(id == "2023_01_14_DI4" &
                  dive.nr == 80)
S2w_T <- S2w_T[15:(nrow(S2w_T)-15), ]  # drop first 5 and last 5 rows

# Calculate differences between successive depth values
depth_diff <- diff(S2w_T$wig_depth)  

S2w_T$vertical_speed_val = c(NA, depth_diff)

# plot HTakahashi wiggles
B = ggplot(data = S2_pce, 
           aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
  
  geom_point(data = subset(S2w_T, wiggles_Takahashi > 0), aes(x = date.time, y = depth*-1), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  xlab("Time") +
  ylab("Depth (m)") +
  ylim(-68,-48)+
  annotate("text", x = max(S2_pce$date.time) - 50, y = -50, label = "Single changepoint wiggles", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = -50,size = 2.5, 
           fill = "navy", col = "red", shape = 21) +
  coord_cartesian(
    xlim = c(min(S2_w$date.time) + 10, 
             max(S2_w$date.time) - 10))  + 
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())

B

C = ggplot(data = S2_w, 
           aes(x = date.time, y = vertical_speed_val)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
  
  geom_point(data = subset(S2w_T,  wiggles_Takahashi> 0), aes(x = date.time, y = vertical_speed_val), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  labs(x = "Time",
       y = bquote('Vertical speed' ~ (m.s^-1)))+
  
  annotate("text", x = max(S2_pce$date.time) - 50, y = 1.9, label = "Single changepoint wiggles", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = 1.9, size = 2.5, 
           fill = "navy", col = "red", shape = 21) + 
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  
   coord_cartesian(xlim = c(min(S2_w$date.time) + 10, 
                           max(S2_w$date.time) - 10))
C

B <- B + theme(plot.margin = margin(1, 2, 1, 2))
C <- C + theme(plot.margin = margin(1, 2, 1, 2))

S2_takahashi <- (A / B / C) + 
  plot_annotation(tag_levels = 'A')  

S2_takahashi

# Save as PNG
ggsave("./supplement/FigureS2_2.png", S2_takahashi , width = 7 , height = 9, dpi = 1000)



#--------------------
# Prey captures plot
#--------------------
# Wiggles:
# Select a single dive from 1 animal as an example
S2wsw = subdat_Simeone %>% 
  dplyr::filter(id == "2023_01_14_DI4" &
                  dive.nr == 80)
S2wsw <- S2wsw[15:(nrow(S2wsw)-15), ]  # drop first 5 and last 5 rows


# plot Simeone wiggles
B = ggplot(data = S2_pce, 
           aes(x = date.time, y = depth*-1)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
  
  geom_point(data = subset(S2wsw, wiggle_SW_bottom > 0), aes(x = date.time, y = depth*-1), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  xlab("Time") +
  ylab("Depth (m)") +
  ylim(-68,-48)+
  annotate("text", x = max(S2_pce$date.time) - 50, y = -50, label = "Relative vertical velocity wiggles (0.35m)", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = -50,size = 2.5, 
           fill = "navy", col = "red", shape = 21) +
  coord_cartesian(
    xlim = c(min(S2_w$date.time) + 10, 
             max(S2_w$date.time) - 10)) + 
   theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())

B


C = ggplot(data = S2wsw, 
           aes(x = date.time, y = vertical_speed_diff)) + 
  geom_line() +
  geom_point(aes(colour = InDive), alpha = 0.3, size = 2, shape = 15) +
  scale_color_manual(values = my_colors)+
  
  geom_point(data = subset(S2wsw,  wiggle_SW_bottom > 0), aes(x = date.time, y = vertical_speed_diff), 
             fill = "navy", col = "red", shape = 21, alpha = 1, size  = 2) +
  
  gg_theme() + 
  theme(legend.position = "none") +
  labs(x = "Time",
       y = bquote('Relative vertical velocity' ~ (m.s^-1)))+
  
  annotate("text", x = max(S2_pce$date.time) - 50, y = 1.2, label = "Relative vertical velocity wiggles (0.35m)", 
           hjust = 0, 
           size = 3, color = "black")+
  annotate("point", x = max(S2_pce$date.time)- 53, y = 1.2, size = 2.5, 
           fill = "navy", col = "red", shape = 21) + 
  
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = -0.35, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0.35 , linetype = "dashed", color = "red") + 
  
  coord_cartesian(xlim = c(min(S2_w$date.time) + 10, 
                           max(S2_w$date.time) - 10))
C


B <- B + theme(plot.margin = margin(1, 2, 1, 2))
C <- C + theme(plot.margin = margin(1, 2, 1, 2))

S2_simeone <- (A / B / C) + 
  plot_annotation(tag_levels = 'A')  

S2_simeone

# Save as PNG
ggsave("./supplement/FigureS2_1.png", S2_simeone , width = 7 , height = 9, dpi = 1000)


#----------------------
#----------------------

