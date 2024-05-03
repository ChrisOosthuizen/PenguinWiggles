gg_theme <- function () { 
  theme_bw() %+replace% 
    theme(
      axis.text = element_text(colour = "black", size = 11),
      axis.title = element_text(size=12),
      axis.ticks = element_line(colour = "black"),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      axis.line = element_line(colour = "black"),
      legend.background = element_blank())
}
