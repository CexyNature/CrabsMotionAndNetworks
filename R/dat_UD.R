library(here)
here::here()


tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)


# Convert coordinates to meter scale
tracks$x_coord <- tracks$Crab_position_x/100
tracks$y_coord <- tracks$Crab_position_y/100


# Remove rows with NAs
summary(tracks)
tracks <- tracks[!is.na(tracks$Crab_position_x),]
summary(tracks)


# adehabitatHR documentation: https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf
library(adehabitatHR)


# Create Spatial Object using coordinates from tracks
coordinates(tracks) <- tracks[, c('x_coord', 'y_coord')]


# Estimate the Utilization Distribution for all individuals, and create UD object
## (A) kernelUD using 'same4all' set to 'TRUE'. This will conserve all xlim and ylim equal to all individuals.
## However, it produce an error when plotting home ranges contours (function 'getverticeshr')
kd <- kernelUD(tracks[,1], same4all = TRUE)
## (B) This kernel does not produce the error described above, but it change xlim and ylim for each individual
kd <- kernelUD(tracks[,1])
## (C) The solution to issues described above is to create a grid
# kd <- kernelUD(tracks[,2], same4all = TRUE, grid = 65)


# Currently using B
# Accessing the smoothing parameters for each invididual
for(i in seq(1, length(kd[]), by = 1)){
  smooth_par = kd[[i]]@h
  print(paste0("Smoothing parameter for individual ", i, " is ", smooth_par[1]))
  rm(list = c('smooth_par', 'i'))
  gc()
}


# Visualize UD for each individual in same window
image(kd, col = terrain.colors(100))


# Visualize each kernel Utilization Distribution and contours separately
for(i in seq(1, length(kd[]), by = 1)){
  image(kd[[i]], col = terrain.colors(100))
  plot(getverticeshr(kd[[i]], percent = 95), add = TRUE)
  print(paste0('Done: track number ', i))
  rm(list = c('i'))
  gc()
}


library(ggplot2)
library(reshape2)
load(file = "data/tidy/GP010016_fast_id_colors_brew_17.Rdata")
# Size of home range per individual and HR-level
area_ud <- kernel.area(kd, unin = "m", unout = "m2")
area_ud * 10000
area_ud_melt <- area_ud
area_ud_melt$HR_level <- rownames(area_ud_melt)
area_ud_melt <- melt(area_ud_melt, id.vars = 'HR_level', variable.name = "Individual")
# Change order of factors
area_ud_melt$Individual <- factor(area_ud_melt$Individual, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                             "crab_6","crab_7","crab_8","crab_9","crab_10","crab_11","crab_12","crab_13",
                             "crab_14","crab_15","crab_16","crab_17"))
  

g0 <- ggplot(area_ud_melt, aes(x = HR_level, y = value * 10000, group = Individual, color = Individual)) +
  geom_path(size = 1) +
  scale_color_manual(values = id_colors_brew) +
  scale_x_discrete(breaks = seq(20,95,15)) +
  scale_y_continuous(breaks = seq(50,1150,100)) +
  # facet_wrap(.~Individual, ncol = 3) +
  labs(x = "Home range level",
       y = "Home range size" ~("cm"^"2")) +
  coord_cartesian(ylim = c(0, 1150), expand = FALSE) +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5), 
        legend.position = 'bottom',
        # legend.justification = c("right", "top"),
        legend.key = element_rect(fill = "white", color = NA),
        panel.background = element_blank(),
        axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_line(color = "gray", size = 0.25),
        panel.border = element_rect(color = "black", fill = NA, size = .5))
g0 + guides(colour = guide_legend(nrow = 3, byrow=TRUE))
# ggsave('figures/GP010016_fast_individuals_UDsizes_17.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Extract the Home Range contour per individual at different percentrage levels for HR estimation.
# This functions creates a Spatil Polygons Data Frame
ud <- getverticeshr(kd, percent = 95)
class(ud)
# Plot contours for all individuals
plot(ud)
# tracks[,2]
# Save individual IDs to a vector
kd_names <- names(kd)


# Apply function for extracting contours to all individuals, one at the time, using percentage 50
ud50 <- lapply(kd, function(x) try(getverticeshr(x, 50)))
# Change each polygons id to the species name for rbind call
sapply(1:length(ud50), function(i) {
  row.names(ud50[[i]]) <<- kd_names[i]
})
# Combines (i.e. Reduce) the list into single Spatial Polygons Data Frame
sdf_poly50 <- Reduce(rbind, ud50)
plot(sdf_poly50)


# Kernel 75%
ud75 <- lapply(kd, function(x) try(getverticeshr(x, 75)))
sapply(1:length(ud75), function(i) {
  row.names(ud75[[i]]) <<- kd_names[i]
})
sdf_poly75 <- Reduce(rbind, ud75)


# Kernel 95%
ud95 <- lapply(kd, function(x) try(getverticeshr(x, 95)))
sapply(1:length(ud95), function(i) {
  row.names(ud95[[i]]) <<- kd_names[i]
})
sdf_poly95 <- Reduce(rbind, ud95)


# Convert Spatial Polygons Data Frame into a dataframe. This is requiered to plot contours polygons in 'ggplot'.
# 'fortify' function may be deprecated in the future. The alternative is use the broom package.
df50 <- fortify(sdf_poly50)
df75 <- fortify(sdf_poly75)
df95 <- fortify(sdf_poly95)

# Re-order factor levels in df50, df75, df95
df50$id <- factor(df50$id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                             "crab_6","crab_7","crab_8","crab_9","crab_10",
                             "crab_11","crab_12","crab_13","crab_14","crab_15",
                             "crab_16","crab_17"))
df75$id <- factor(df75$id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                             "crab_6","crab_7","crab_8","crab_9","crab_10",
                             "crab_11","crab_12","crab_13","crab_14","crab_15",
                             "crab_16","crab_17"))
df95$id <- factor(df95$id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                             "crab_6","crab_7","crab_8","crab_9","crab_10",
                             "crab_11","crab_12","crab_13","crab_14","crab_15",
                             "crab_16","crab_17"))


# Open tracks file
tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)
# Re-order factor levels in tracks
tracks$Crab_ID <- factor(tracks$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                         "crab_6","crab_7","crab_8","crab_9","crab_10",
                                         "crab_11","crab_12","crab_13",
                                         "crab_14","crab_15","crab_16","crab_17"))


# Get centroids ofr UDK50 polygons. These will be used to label crabs UD.
library(dplyr)
df50_centroids <- df50 %>% 
  dplyr::select(long, lat, id) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(long_c = mean(long), lat_c = mean(lat))


# Change centroid for crab_10 polygon
df50_centroids$lat_c[10] <- 0.32
# Change centroid for crab_5 polygon
df50_centroids$lat_c[5] <- 0.56
df50_centroids$long_c[5] <- 0.12
# Create dummy data frame to be able to create dummy layer in plot
dummy <- data.frame(x =0.5, y = 0.5)


library(ggrepel)


# Figure including the position of the burrows
burrows_all <- read.csv("data/raw/GP010016_burrows_map.csv", header = TRUE, skip = 4)

burrows_all$x1 <- burrows_all$Burrow_coord_x*0.1584
burrows_all$y1 <- burrows_all$Burrow_coord_y*0.1584

g_hr_tr_bu_all <- ggplot() +
  geom_polygon(data = df50, aes(x = long, y = lat, fill = id, group = group), alpha = 0.2, size = 0.5, colour = "black", linetype = 2) +
  geom_polygon(data = df95, aes(x = long, y = lat, fill = id, group = group), alpha= 0.2, size = 0.5, colour = "black", linetype = 1) +
  geom_polygon(data = df75, aes(x = long, y = lat, fill = id, group = group), alpha= 0.2, size = 0.5, colour = "black", linetype = 5) +
  scale_fill_manual(name = "", values = id_colors_brew) +
  geom_path(data = subset(tracks, !is.na(tracks$x_smooth)), 
            aes(x=x_smooth/100, y=y_smooth/100, group = Crab_ID),
            size = 0.5,linetype = 3) +
  geom_text_repel(data = subset(df50_centroids, !(id %in% c("crab_8", "crab_2", "crab_14", "crab_13", 
                                                            "crab_5", "crab_4", "crab_12", "crab_11",
                                                            "crab_15", "crab_16", "crab_10", "crab_17",
                                                            "crab_7", "crab_1", "crab_3"))),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.05,
                  nudge_y = 0.05,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_8", "crab_2", "crab_4")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = 0.05,
                  nudge_y = 0.07,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_14")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = 0.0,
                  nudge_y = 0.08,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_13", "crab_5", "crab_12", "crab_11")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.08,
                  nudge_y = 0.07,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_15", "crab_16")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.11,
                  nudge_y = -0.1,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_10", "crab_17")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.18,
                  nudge_y = 0,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_7")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = 0.05,
                  nudge_y = -0.05,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_1")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.1,
                  nudge_y = -0.1,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  geom_text_repel(data = subset(df50_centroids, id %in% c("crab_3")),
                  aes(x = long_c, y = lat_c, label = id),
                  force = 3,
                  direction = "x",
                  segment.size = 0.25,
                  nudge_x = -0.05,
                  nudge_y = 0.19,
                  colour = "blue",
                  size = 3,
                  seed = 11) +
  coord_fixed(xlim = c(0,0.8), ylim = c(0,0.8), expand = FALSE) +
  geom_point(data = subset(burrows_all, !is.na(burrows_all$x1)), 
             aes(x = x1/100, y = y1/100, colour = "black"), 
             shape = 16, 
             # color = "black", 
             size = 3,
             alpha = 0.85) +
  scale_color_manual("", values = "black", label = "Burrow") +
  # New layer to create linetype legend, observe alpha equal zero
  geom_polygon(data = subset(df75, id %in% c("crab_13", "crab_5", "crab_12")), 
               aes(x = long, y = lat, fill = id, group = group, linetype = id), alpha= 0, size = 0.5, colour = "black") +
  scale_linetype_manual("", 
                        values = c(1, 5, 2), 
                        labels = c("95% UD", "75% UD",  "50% UD")) +
  # New dummy layer to create individuals' tracks legend
  geom_hline(dummy, mapping = aes(yintercept = 0.5, size = 0.5), colour = "black", linetype = 3, alpha = 0) +
  scale_size_identity("", label = "Individuals' tracks", breaks = 1, guide = "legend") +
  # coord_equal(expand = TRUE) +
  # theme_void() +
  scale_y_reverse() + 
  # scale_y_reverse(sec.axis = dup_axis(name = "")) + 
  # scale_x_continuous(sec.axis = dup_axis(name = "")) +
  labs(x = "Distance (m)", y = "Distance (m)") +
  theme(panel.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = "right",
        legend.spacing = unit(0.05, "cm"),
        axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_line(color = "gray", size = 0.25),
        panel.border = element_rect(color = "black", fill = NA, size = .5))

g_hr_tr_bu_all + guides(fill = guide_legend(ncol = 1, byrow=TRUE, order = 1, keyheight = 0.3),
                    color = guide_legend(order = 4),
                    linetype = guide_legend(order = 2),
                    size = guide_legend(order = 3, override.aes = list(alpha = 1)))

ggsave('figures/Fig_crabs_tracks_UD_burrows.png', width = 21, height = 20, units = 'cm', dpi = 300)

