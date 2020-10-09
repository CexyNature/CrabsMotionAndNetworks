library(here)
here::here()


# Open file
int_mat <- read.csv("data/raw/interaction_matrix.csv")
int_mat_rnames <- as.character(int_mat[,1])
int_mat_rnames
int_mat <- as.matrix(int_mat[,-1], dimnames = list(names(int_mat)[-1], names(int_mat)[-1]))
int_mat
rownames(int_mat) <- int_mat_rnames
int_mat


# noncon = interaction without physichal contact
# con = interactions which involve physichal contact
indexes_upper <- which(upper.tri(int_mat, diag=FALSE), arr.ind = TRUE)
ind_names <- dimnames(int_mat)
int_mat_noncon <- data.frame(Crab_ID = ind_names[[1]][indexes_upper[,1]],
                             cd_id = ind_names[[2]][indexes_upper[,2]],
                             interac = int_mat[indexes_upper])


indexes_lower <- which(lower.tri(int_mat, diag = FALSE), arr.ind = TRUE)
int_mat_con <- data.frame(Crab_ID = ind_names[[1]][indexes_lower[,1]],
                          cd_id = ind_names[[2]][indexes_lower[,2]],
                          interac = int_mat[indexes_lower])


library(tidyverse)
int_mat_noncon <- int_mat_noncon %>%
  filter(!is.na(interac))
int_mat_con <- int_mat_con %>%
  filter(!is.na(interac))


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


# Create Network
library(igraph)
library(ggplot2)
library(ggraph)
library(ggimage)


net_int2_g2_noncon <- graph.data.frame(int_mat_noncon, directed = FALSE, vertices = crabs_info)
net_int2_g2_con <- graph.data.frame(int_mat_con, directed = FALSE, vertices = crabs_info)


attributes(E(net_int2_g2_noncon))
attributes(V(net_int2_g2_noncon))


attributes(E(net_int2_g2_con))
attributes(V(net_int2_g2_con))


net_int2_g2_noncon <- set.vertex.attribute(net_int2_g2_noncon, "label", value = c(1:17))
V(net_int2_g2_noncon)$name
V(net_int2_g2_noncon)$label
vertex_attr(net_int2_g2_noncon)


edge_density(net_int2_g2_noncon, loops = FALSE)
gsize(net_int2_g2_noncon)
edge_attr(net_int2_g2_noncon)


net_int2_g2_con <- set.vertex.attribute(net_int2_g2_con, "label", value = c(1:17))
V(net_int2_g2_con)$name
V(net_int2_g2_con)$label
vertex_attr(net_int2_g2_con)


edge_density(net_int2_g2_con, loops = FALSE)
gsize(net_int2_g2_con)
edge_attr(net_int2_g2_con)


set.seed(85)
layout0_noncon <- create_layout(net_int2_g2_noncon, layout = "igraph", algorithm = "fr")
layout0_noncon


set.seed(85)
layout0_con <- create_layout(net_int2_g2_con, layout = "igraph", algorithm = "fr")
layout0_con


# Graph pub with legend
ggraph(layout0_noncon) +
  geom_edge_diagonal() +
  # geom_edge_diagonal(aes(edge_colour = prop_lvl), edge_width = 0.75) +
  # scale_edge_color_manual("Proportion time within 10 cm", values = c("#000000", "#909090"),
  #                         labels = c(">0.5", "<0.5")) +
  # new_scale("colour") +
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
# ggsave('figures/GP010016_fast_17_net_noncon_pub_leg.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Graph pub without legend
ggraph(layout0_con) +
  geom_edge_diagonal(aes(width = interac)) +
  scale_edge_width("Number of interactions", breaks = c(1,2,3), range = c(0.5, 1.5)) +
  # geom_edge_diagonal(aes(edge_colour = prop_lvl), edge_width = 0.75) +
  # scale_edge_color_manual("Proportion time within 10 cm", values = c("#000000", "#909090"),
  #                         labels = c(">0.5", "<0.5")) +
  # new_scale("colour") +
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
# ggsave('figures/GP010016_fast_17_net_con_pub_leg.png', width = 15, height = 15, units = 'cm', dpi = 300)


# Combine both type of interactions:
int_mat1 <- int_mat_noncon %>%
  dplyr::mutate(Crab_ID = as.character(as.factor(Crab_ID)), cd_id = as.character(as.factor(cd_id))) 


int_mat <- int_mat_con %>%
  dplyr::mutate(Crab_ID = as.character(as.factor(Crab_ID)), cd_id = as.character(as.factor(cd_id))) %>%
  dplyr::bind_rows(int_mat1) %>%
  dplyr::mutate(node0 = pmin(Crab_ID, cd_id), node1 = pmax(Crab_ID, cd_id)) %>%
  dplyr::group_by(node0, node1) %>%
  dplyr::summarise(interac = sum(interac, na.rm = TRUE))


net_int2_g2_combined <- graph.data.frame(int_mat, directed = FALSE, vertices = crabs_info)


attributes(E(net_int2_g2_combined))
attributes(V(net_int2_g2_combined))


net_int2_g2_combined <- set.vertex.attribute(net_int2_g2_combined, "label", value = c(1:17))
V(net_int2_g2_combined)$name
V(net_int2_g2_combined)$label
vertex_attr(net_int2_g2_combined)


edge_density(net_int2_g2_combined, loops = FALSE)
gsize(net_int2_g2_combined)
edge_attr(net_int2_g2_combined)


set.seed(85)
layout0_combined <- create_layout(net_int2_g2_combined, layout = "igraph", algorithm = "fr")
layout0_combined


# Graph pub with legend
ggraph(layout0_combined) +
  geom_edge_diagonal(aes(width = interac)) +
  scale_edge_width("Number of interactions", breaks = c(1,2,3,4), range = c(0.5, 2)) +
  # geom_edge_diagonal(aes(edge_colour = prop_lvl), edge_width = 0.75) +
  # scale_edge_color_manual("Proportion time within 10 cm", values = c("#000000", "#909090"),
  #                         labels = c(">0.5", "<0.5")) +
  # new_scale("colour") +
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
# ggsave('figures/GP010016_fast_17_net_combined_pub_leg.png', width = 15, height = 15, units = 'cm', dpi = 300)


saveRDS(net_int2_g2_con, file = here::here('data/tidy', 'net_int2_g2_con.rds'))
