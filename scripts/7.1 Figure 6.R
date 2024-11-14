figure8 = Density_map_wiggleN_Halsey_2m %>% 
  filter(id == "2022_01_13_AC2105_HPM09" | 
         id == "2022_01_16_AC2105_DI06" | 
         id == "2022_01_22_AC2110_DI06" | 
         id == "2022_01_10_AC2003_DI09") %>%
  ggplot(aes(x = wiggleN_Halsey_2m, y =video_pce_IN, color = Density)) +
  geom_jitter(alpha = 0.4, size = 1.9, height = 0, width = 0) +
  scale_color_viridis_c(option = 'inferno', end = 0.9, begin = 0.1)+
  gg_theme() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")+
  ylab("Video prey captures") +
  xlab("No. wiggles") +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100, by = 25))+
  scale_x_continuous(limits = c(-0.1, 6.1)) +
  theme(legend.position = "none")+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +  
  facet_wrap(~id, scales = "fixed", ncol = 2) + 
  theme(
    legend.position = "inside", legend.position.inside = c(0.99, 0.99),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right") + 
 theme(legend.key.size=unit(0.6, "lines")) 

figure8

# Save as PNG
ggsave("./figures/figure8.png", figure8, width = 4, height = 4, dpi = 1000)






