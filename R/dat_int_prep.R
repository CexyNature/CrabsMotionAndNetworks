library(here)
here::here()

# Open clean tracks data
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
# Create a data frame for each crab and save in list
tracks_ls <- split(tracks, f = tracks$Crab_ID)
# Drop factor levels from each data frame (to avoid error in ltraj)
tracks_ls[] <- lapply(names(tracks_ls), function(x) droplevels(tracks_ls[[x]]))


# Create 'ltraj' object
library(adehabitatLT)
tracks_lslt <- lapply(names(tracks_ls), 
                      function(x) as.ltraj(tracks_ls[[x]][,c('x_coord', 'y_coord')],
                                           date = tracks_ls[[x]]$Time_lapsed_since_start.secs.,
                                           id = tracks_ls[[x]]$Crab_ID, typeII = TRUE))


# Print summary for each data frame (i.e. individual)
for (i in 1:length(tracks_lslt)) {
  print(summary.ltraj(tracks_lslt[[i]]))
}


# Calculate closest individual per individual
library(hab)
# Check point time 1
check.p1 <- Sys.time()


# Calculate closest distance for each pair
int_pairs_m <- sapply(tracks_lslt, function(x) sapply(tracks_lslt, function(y) 
  closest(from = x, to = y, dt = c(-1,1), units = c("sec"), ref = "start",
          prefix = "cd_", protect = c("id", "dist", "R2n", "abs.angle", "rel.angle"), range = "()")))

check.p2 <- Sys.time()
print(paste0("Time since start ", check.p2 - st.time))
print(paste0("Last function time ", check.p2 - check.p1))


# Only keep one pair comparison for each combination and exlude the diagonal
int_pairs_m_lw <- int_pairs_m[upper.tri(int_pairs_m,diag=FALSE)]


df_list <- list()
# Create list with infolocs
for (i in 1:length(int_pairs_m_lw)) {
  df_list[[i]] <- attributes(int_pairs_m_lw[[i]])$infolocs
  
}


all_dist <- do.call(rbind, df_list)
all_dist$id <- sub("\\..*", "", all_dist$pkey)
all_dist <- all_dist[,c(13, 1:12)]
all_dist$id <- as.factor(as.character(all_dist$id))
all_dist$pkey <- as.character(as.factor(all_dist$pkey))


# Save R objects to file
saveRDS(int_pairs_m, file = here::here('data/tidy', 'GP010016_fast_17_pair_matrix.rds'))
saveRDS(int_pairs_m_lw, file = here::here('data/tidy', 'GP010016_fast_17_pair_matrix_upper.rds'))
saveRDS(df_list, file = here::here('data/tidy', 'GP010016_fast_17_pair_list_df.rds'))
saveRDS(all_dist, file = here::here('data/tidy', 'GP010016_fast_17_pair_df.rds'))


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


# Create boolean column to indicate if crab is distance between crab pairs is 5 cm or closer
all_dist_or$under_10cm <- ifelse(all_dist_or$cd_distloc <= 10, "TRUE", "FALSE")
length(which(all_dist_or$under_10cm == "TRUE"))


library(tidyverse)
all_dist_or %>% 
  dplyr::filter(!is.na(cd_distloc)) %>%
  dplyr::group_by(id, cd_id) %>% 
  dplyr::filter(cd_distloc <= 10) %>%
  dplyr::summarise(count = n(), mean_R2n = mean(cd_R2n))
# Detection were scarce over time, thus, R2n cannot be calculated. This is the reason for R2n equal NA for some crabs


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
                                    image = "https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png")


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


library(Cairo)
library(extrafont)
library(ggimage)
loadfonts(device = "win")


set.seed(85)
layout0 <- create_layout(net_int2_g, layout = "igraph", algorithm = "fr")
layout0


ggraph(layout0) +
  # geom_edge_density(aes(fill = prop_lvl)) +
  # geom_edge_diagonal(aes(label = prop_lvl, colour = prop_lvl), 
  #                    alpha = 1, angle_calc = "along", label_dodge = unit(2.5, "mm")) +
  geom_edge_diagonal(aes(colour = prop_lvl), edge_width = 1) +
  # geom_node_point(aes(color = Sex, size= (mean_size/2.39)*100, shape = Handedness)) +
  geom_image(aes(x = layout0$x, y = layout0$y, 
    image="https://e.unicode-table.com/orig/05/57660e78fcf1d00dd0a7e1ecb9108b.png", size = I(mean_size/50))) +
  scale_color_manual(values = c("#004d99", "#696969")) +
  scale_shape_manual(values = c(17, 18, 19)) +
  geom_node_text(aes(label = Sex_label), position = position_nudge(y=-0.15, x=-0.2)) +
  # geom_node_text(aes(label = Sex_label), position = position_nudge(y=-0.22, x=-0.2)) +
  geom_node_text(aes(label = label), position = position_nudge(y=-0.45, x=-0.25)) +
  # geom_node_text(aes(label = "\u1F980"), position = position_nudge(y=-0.15, x=-0.2)) +
  coord_equal() +
  ggtitle("Network Graph based on crabs proximity") +
  theme(text = element_text(family = "FontAwesome"),
        panel.background = element_blank(),
        legend.position = "None",
        # axis.line = element_line(color = "black", size = .5),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


ggsave('src/img_repo/GP010016_fast_network.png', width = 15, height = 15, units = 'cm', dpi = 300)
