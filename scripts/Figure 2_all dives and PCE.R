
#-----------------------------------
#  Figure 2 - time series of dives
#-----------------------------------

# Density function 
# https://themockup.blog/posts/2020-08-28-heatmaps-in-ggplot2/
get_Density <- function(x, y, ...) {
  Density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, Density_out$x)
  int_y <- findInterval(y, Density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(Density_out$z[comb_int])
}


# To plot VIDEO captures, take ANY of the subdat (they are the same dimensions)
pcedat = subdat_r %>% 
      # clean up   
            filter(light.level == 'light'&
                  video.level == 'video') %>%
            mutate(year = year(date.time)) %>%
         # Create the new column with day and month
            mutate(day_month = format(date.time, "%d %b"))

head(pcedat)
table(pcedat$InDive)

# plot simple graph of all dive data
# pcedat %>% 
#   arrange(date.time) %>%
#   ggplot(aes(x = seq(1:length(depth)), y = depth*-1, color = as.factor(PCE_1hz))) +
#   geom_point(alpha = 0.2, shape = ".")


# create x variable to plot (simple sequence; works much better than 'date')
pcedat = pcedat %>% 
  arrange(date.time) %>%
 # group_by(year) %>%
  mutate(xseq = row_number()) %>%
  ungroup()

pcedat %>%
  group_by(year) %>%
  summarize(minx = min(xseq),
            maxx = max(xseq),
            mind = min(day_month),
            maxd = max(day_month)) 

# create data frame to plot PCE 
pcecapt = pcedat %>% 
  filter(PCE_1hz > 0)

# Duplicate rows based on the PCE_1hz column
# So, if PCE_1hz = 2, then it creates an extra PCE_1hz row
pcecapt_dup <- pcecapt[rep(row.names(pcecapt), pcecapt$PCE_1hz), ]

# should be the same
sum(pcecapt$PCE_1hz)
nrow(pcecapt_dup)

density_map <- pcedat %>% 
  select(xseq, depth, year, day_month) %>% 
  mutate(Density = get_Density(xseq, depth, n = 100))


# Group by day and month, then count distinct years
# duplicate_days <- pcecapt_dup %>%
#   group_by(day_month) %>%
#   summarize(years_count = n_distinct(year)) %>%
#   filter(years_count > 1)
# duplicate_days 

# display_indices =  pcecapt_dup %>% 
#           filter(day_month == "08 Jan" | 
#                  day_month == "16 Jan" |
#                  day_month == "15 Jan" ) %>%
#           distinct(day_month, .keep_all = TRUE )
#   
# display_indices 

# # Subset the day_month_data to include only the selected labels
# selected_labels <- display_indices$day_month
# selected_labels 

x.scale = c(1, 25000, 50000, 75000, 
            92000,
            102000, 112000, 122000, 132000)

x.dates = pcedat %>% 
           filter(xseq == 1 | 
                  xseq == 25000 |
                  xseq == 50000 |
                  xseq == 75000|
                  xseq == 92000|
                  xseq == 102000|
                  xseq == 112000|
                  xseq == 122000|
                  xseq == 132000) %>%
           distinct(date.time, .keep_all = TRUE) %>% 
           select(day_month, xseq, date.time)
x.scale
x.dates

dive_fig = ggplot() + 
  # plot depth heatplot
  geom_point(data = density_map, aes(x =xseq, y = depth*-1, color = Density),
  alpha = 1, shape = 16, size = 0.5) +
  facet_wrap(~year, scales = "free_x") + 
  scale_color_viridis_c(option = 'viridis', end = 1, begin = 0.2) +
  gg_theme() + 
  # add prey captures
  geom_jitter(data = pcecapt_dup, aes(x =xseq, y = depth*-1), color = "red", size = 0.3, shape = 16) + 
 # make plot pretty
  xlab("Chronological order of dives") + 
  ylab("Depth (m)") +
  scale_x_continuous(breaks = x.scale,  
                    labels = x.dates$day_month) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(# strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.92, 0.22))+
  theme(plot.margin = margin(b = 20)) + 
  theme(axis.title.x = element_text(vjust=-1.8))

dive_fig 

# Create a data frame for annotation
annotation_text <- data.frame(year = 2022, x = 1870, y = -130, label = "Prey captures")
annotation_point <- data.frame(year = 2022, x = 150, y = -130)

# Add annotation to a specific facet
dive_fig = dive_fig + 
  geom_text(data = annotation_text %>% filter(year == 2022), aes(x=x,y=y,label = label), hjust = 0)+
  geom_point(data = annotation_point %>% filter(year == 2022), aes(x=x,y=y),shape = 16, color = "red")

# Save the ggplot as a TIFF file
ggsave("./plots/Figure2.tiff", plot = dive_fig, width = 8, height = 5, units = "in", dpi = 500)



