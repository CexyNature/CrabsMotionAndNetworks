library(here)
library(tidyverse)


all_dist <- readRDS(file = here::here('data/tidy', 'GP010016_fast_17_pair_df.rds'))


# Check all pair combinations in df
unique(all_dist[,c("id", "cd_id")])
# Set factor order for crab id
all_dist$id <- factor(all_dist$id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                     "crab_6","crab_7","crab_8","crab_9","crab_10",
                                     "crab_11","crab_12","crab_13",
                                     "crab_14","crab_15","crab_16","crab_17"))
# Set factor order for closest crab
all_dist$cd_id <- factor(all_dist$cd_id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                           "crab_6","crab_7","crab_8","crab_9","crab_10",
                                           "crab_11","crab_12","crab_13",
                                           "crab_14","crab_15","crab_16","crab_17"))
# Order df by "cd_id" and "id"
all_dist_or <- all_dist[order(all_dist$cd_id, all_dist$id),]
# Check all pair combinations in df
unique(all_dist_or[,c("id", "cd_id")])


# Create boolean column to indicate if crab is distance between crab pairs is x cm or closer
all_dist_or$under_10cm <- ifelse(all_dist_or$cd_distloc <= 10, "TRUE", "FALSE")
length(which(all_dist_or$under_10cm == "TRUE"))


all_dist_or %>% 
  dplyr::filter(!is.na(cd_distloc)) %>%
  dplyr::group_by(id, cd_id) %>% 
  dplyr::filter(cd_distloc <= 10) %>%
  dplyr::summarise(count = n(), mean_R2n = mean(cd_R2n))


# Open tracking data
tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)


# Set digits for decimal seconds to six decimal positions
options(digits.secs = 6)
# Test the decimal positions for seconds
Sys.time()
# Set starting time
st.time <- Sys.time()


# Change date and time data format to POSIXct
tracks$Time_lapsed_since_start.secs. <- as.POSIXct(tracks$Time_lapsed_since_start.secs.)
# Set Crab_ID order factor
tracks$Crab_ID <- factor(tracks$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                           "crab_6","crab_7","crab_8","crab_9","crab_10",
                                           "crab_11","crab_12","crab_13",
                                           "crab_14","crab_15","crab_16","crab_17"))
# Convert coordinates to meter scale
tracks$x_coord <- tracks$Crab_position_x
tracks$y_coord <- tracks$Crab_position_y


# Create association file
net_int2 <- all_dist_or %>% 
  dplyr::filter(!is.na(cd_distloc)) %>%
  dplyr::group_by(id, cd_id) %>% 
  dplyr::mutate(count_t = n()) %>%
  dplyr::filter(cd_distloc <= 10) %>%
  dplyr::summarise(count_n = n(), count_t = mean(count_t)) %>%
  dplyr::mutate(prop = round(count_n / count_t, 2)) %>%
  dplyr::mutate(prop_lvl = ifelse(prop > 0.5, "High", "Low"))


# Load individuals meta information
crabs_info <- read.csv('data/tidy/crabs_meta_info.csv', row.names = 1,
                       stringsAsFactors = FALSE)
crabs_info$Crab_ID <- factor(crabs_info$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                                   "crab_6","crab_7","crab_8","crab_9","crab_10",
                                                   "crab_11","crab_12","crab_13",
                                                   "crab_14","crab_15","crab_16","crab_17"))
crabs_info <- crabs_info[order(crabs_info$Crab_ID),]


crabs_info <- crabs_info %>% mutate(Sex_label = case_when(Sex == "Male" ~ "\u2642",
                                                          Sex == "Unknown" ~ "\u003F",
                                                          TRUE ~ "\u2640"),
                                    image = "https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png",
                                    Handedness_label = case_when(Handedness == "Left" ~ "L",
                                                                 Handedness == "Right" ~ "R",
                                                                 TRUE ~ "U"))


# Change column name to match crabs_info, so vertex will have same names
colnames(net_int2)[1] <- "Crab_ID"


# Create Network
library(igraph)
net_int2_g <- graph.data.frame(net_int2, directed = FALSE, vertices = crabs_info)
library(ggplot2)
library(ggraph)


attributes(E(net_int2_g))
attributes(V(net_int2_g))


net_int2_g <- set.vertex.attribute(net_int2_g, "label", value = c(1:17))
V(net_int2_g)$name
V(net_int2_g)$label
vertex_attr(net_int2_g)


edge_density(net_int2_g, loops = FALSE)
gsize(net_int2_g)
edge_attr(net_int2_g)


layout0 <- create_layout(net_int2_g, layout = "igraph", algorithm = "fr")
layout0


# Graph pub with legend
ggraph(layout0) +
  geom_edge_diagonal(aes(edge_colour = prop_lvl), edge_width = 0.75) +
  scale_edge_color_manual("Proportion time within 10 cm", values = c("#000000", "#909090"),
                          labels = c(">0.5", "<0.5")) +
  new_scale("colour") +
  geom_node_point(aes(colour = Handedness, size= (mean_size/2.39)*100)) +
  # geom_image(aes(x = layout0$x, y = layout0$y, 
                 # image="https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png", size = I(mean_size/50))) +
  scale_color_manual(values = c("#6600cc", "#009900", "#e67300")) +
  scale_size_continuous("Size (cm)", breaks = c(40, 60, 80, 100),
                        labels = c(round((40/100)*2.39, 2),
                                   round((60/100)*2.39, 2),
                                   round((80/100)*2.39, 2),
                                   round((100/100)*2.39, 2))) +
  # scale_shape_manual(values = c(17, 18, 19)) +
  geom_node_text(aes(label = Sex_label), position = position_nudge(y=-0.22, x=-0.2)) +
  geom_node_text(aes(label = label), position = position_nudge(y=-0.45, x=-0.25)) +
  # geom_node_text(aes(label = "\u1F980"), position = position_nudge(y=-0.15, x=-0.2)) +
  coord_equal() +
  theme(panel.background = element_blank(),
        legend.position = "right",
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
# ggsave('figures/GP010016_fast_17_net_proximity.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Graph pub without legend
ggraph(layout0) +
  geom_edge_diagonal(aes(edge_colour = prop_lvl), edge_width = 0.75) +
  scale_edge_color_manual("Proportion time within 10 cm", values = c("#000000", "#909090"),
                          labels = c(">0.5", "<0.5")) +
  new_scale("colour") +
  geom_node_point(aes(colour = Handedness, size= (mean_size/2.39)*100)) +
  # geom_image(aes(x = layout0$x, y = layout0$y, 
  # image="https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png", size = I(mean_size/50))) +
  scale_color_manual(values = c("#6600cc", "#009900", "#e67300")) +
  scale_size_continuous("Size (cm)", breaks = c(40, 60, 80, 100),
                        labels = c(round((40/100)*2.39, 2),
                                   round((60/100)*2.39, 2),
                                   round((80/100)*2.39, 2),
                                   round((100/100)*2.39, 2))) +
  # scale_shape_manual(values = c(17, 18, 19)) +
  geom_node_text(aes(label = Sex_label), position = position_nudge(y=-0.22, x=-0.2)) +
  geom_node_text(aes(label = label), position = position_nudge(y=-0.45, x=-0.25)) +
  # geom_node_text(aes(label = "\u1F980"), position = position_nudge(y=-0.15, x=-0.2)) +
  coord_equal() +
  theme(panel.background = element_blank(),
        legend.position = "none",
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
# ggsave('figures/GP010016_fast_17_net_proximity_noleg.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Graph with proportions as continous, and edge color as physichal interactions
net_int2_g2_con <- readRDS(file = here::here('data/tidy', 'net_int2_g2_con.rds'))


edge_attr(net_int2_g)
E(net_int2_g)


ends(net_int2_g2_con, E(net_int2_g2_con))
edges_con <- as.vector(t(ends(net_int2_g2_con, E(net_int2_g2_con))))
edges_con <- get.edge.ids(net_int2_g, edges_con, directed = FALSE)
edges_con
ends(net_int2_g2_con, E(net_int2_g2_con))
E(net_int2_g)


E(net_int2_g)[edges_con]$colour_con <- "red"
E(net_int2_g)[-edges_con]$colour_con <- "black"


edge_attr(net_int2_g)


set.seed(85)
layout0_2 <- create_layout(net_int2_g, layout = "igraph", algorithm = "fr")


ggraph(layout0_2) +
  geom_edge_diagonal(aes(width = prop, colour = colour_con)) +
  scale_edge_width("Proportion of time where pair \ndistance was less than 10 cm",
                   breaks = c(0.25, 0.5, 0.75, 1), 
                   range = c(0.5, 2)) +
  scale_edge_color_manual("Interaction type:", 
                          values = c("#000000", "#f00000"),
                          labels = c("Non-agonistic", "Agonistic")) +
  new_scale("colour") +
  geom_node_point(aes(fill = Handedness, size= (mean_size/2.39)*100), 
                  shape = 21, 
                  stroke = 1) +
  # geom_image(aes(x = layout0$x, y = layout0$y, 
  # image="https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png", size = I(mean_size/50))) +
  scale_fill_manual(values = c("#4d0099", "#009900", "#e67300")) +
  scale_size_continuous("Crab size (cm)",
                        limits = c(20,100),
                        breaks = c(40, 60, 80, 100),
                        labels = c(round((40/100)*2.39, 2),
                                   round((60/100)*2.39, 2),
                                   round((80/100)*2.39, 2),
                                   round((100/100)*2.39, 2))) +
  # scale_shape_manual(values = c(17, 18, 19)) +
  geom_node_text(aes(label = Sex_label), 
                 position = position_nudge(y=-0.22, x=-0.2)) +
  geom_node_text(aes(label = label), 
                 position = position_nudge(y=-0.45, x=-0.25)) +
  # geom_node_text(aes(label = "\u1F980"), position = position_nudge(y=-0.15, x=-0.2)) +
  coord_equal() +
  theme(panel.background = element_blank(),
        legend.position = "right",
        legend.key = element_rect(fill = NA, color = NA),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 4), nrow =2),
         size = guide_legend(nrow = 2))
ggsave('figures/GP010016_fast_17_network_proximity_and_agonistic.png', width = 15, height = 15, units = 'cm', dpi = 300)

