# Open clean file
tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)
# Re-order factor levels in tracks
tracks$Crab_ID <- factor(tracks$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                           "crab_6","crab_7","crab_8","crab_9","crab_10",
                                           "crab_11", "crab_12", "crab_13", "crab_14","crab_15",
                                           "crab_16", "crab_17"))

# Visualize individual crab paths
library(ggplot2)
library(gganimate)
library(plyr)


crab_label <- tracks$Crab_ID


# Create distintive colors for individuals
library(RColorBrewer)
library(viridis)
n_colors <- length(unique(tracks$Crab_ID))
id_colors <- viridis_pal(option = 'D')(n_colors)



###Creating color ramp function
display.brewer.all()
# getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
# id_colors_brew <- getPalette(n_colors)
getPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
id_colors_brew <- getPalette(n_colors)
id_colors_brew
set.seed(15)
id_colors_brew <- sample(id_colors_brew)
id_colors_brew
save(id_colors_brew, file = "data/tidy/GP010016_fast_id_colors_brew_17.Rdata")

# Visualize individual crab paths with static labels
g3_smooth <- ggplot(tracks, aes(x=x_smooth, y=y_smooth, group = Crab_ID)) + 
  # geom_path(aes(color = Crab_ID), size = .5) + 
  geom_point(aes(fill = Crab_ID, color = Crab_ID), size = 5, pch = 21) +
  scale_color_manual(values = id_colors_brew, guide = FALSE) +
  scale_fill_manual(name='Crab Individuals', values = id_colors_brew
                    # ,
                    # breaks=c('GP010016_fast.mp4_uca_cc00(313, 298)',
                    #          'GP010016_fast.mp4_uca_cc01(291, 205)',
                    #          'GP010016_fast.mp4_uca_ccl01(232, 391)',
                    #          'GP010016_fast.mp4_uca_ccl03(225, 391)',
                    #          'GP010016_fast.mp4_uca_cl03(163, 335)',
                    #          'GP010016_fast.mp4_uca_lm01(446, 249)',
                    #          'GP010016_fast.mp4_uca_lu01(369, 98)',
                    #          'GP010016_fast.mp4_uca_n00(382, 12)',
                    #          'GP010016_fast.mp4_uca_nc00(225, 133)',
                    #          'GP010016_fast.mp4_uca_nw01(52, 85)'),
                    # labels=c('Crab01', 'Crab02', 'Crab03', 'Crab04', 'Crab05',
                    #          'Crab06', 'Crab07', 'Crab08', 'Crab09', 'Crab10')
                    ) +
  coord_cartesian(clip = 'off') +
  labs(title = "Crabs' movement smooth", x = '80 cm', y = '80 cm') +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5), legend.position = 'bottom') +
  scale_y_reverse() + 
  transition_reveal(tracks$Frame_number) +
  shadow_wake(wake_length = 0.1, wrap = FALSE, colour = 'white', falloff = 'quintic-in')
# shadow_trail(max_frames = 10, size = 2, alpha = 0.2)
animate(g3_smooth, nframes = 200, detail = 10)
anim_save(filename = 'Crab_movement_animation.gif', animation = last_animation(), path ='src/img_repo')

