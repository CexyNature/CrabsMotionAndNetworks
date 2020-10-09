tracks <- read.csv('data/tidy/GP010016_fast_17.csv', row.names = 1)

# Create 'ltraj' object
library(adehabitatLT)


options(digits.secs = 6)
Sys.time()
tracks$Time_lapsed_since_start.secs. <- as.POSIXct(tracks$Time_lapsed_since_start.secs., format = )


tracks$Crab_ID <- factor(tracks$Crab_ID, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                           "crab_6","crab_7","crab_8","crab_9","crab_10",
                                           "crab_11","crab_12","crab_13",
                                           "crab_14","crab_15","crab_16","crab_17"))


# Define coordinates
tracks$x_coord <- tracks$Crab_position_x
tracks$y_coord <- tracks$Crab_position_y


track_lt <- as.ltraj(xy = tracks[,c('x_coord', 'y_coord')], date = tracks$Time_lapsed_since_start.secs., 
                     id = tracks$Crab_ID, 
                     infolocs = tracks[,c(4, 7:21)])
track_lt
# This object is Type II and irregular becasue the time lag is variable among successive relocations.
# This is not strictly true in this data set. The data is not recognized as 'regular' because of impressicion working in milliseconds
adehabitatLT::is.regular(track_lt)
# Explorng one element (i.e. individual) from the list
head(track_lt[[1]])
# variables dx, dy and dist are expressed in the same units as x & y. abs.angle and rel.angle are expressed in radians.
# R2n is the squared distance between first observation position and current position
# dx, dy and dist represent direction and time between current location and the next one (i & i+1)
# abs.angle represents angle between i and i+1.
# rel.angle represents angle betwwen (i-1 and i) and (i and i+1). Called turning angle.
plot(track_lt)


# Print mean distance and mean relative angle between sucessive relocations per crab individual
summary_df <- data.frame(Crab_ID = character(0), mean_step_dist = numeric(0), 
                         mean_step_rel_angle = numeric(0), mean_step_abs_angle = numeric(0), stringsAsFactors = FALSE)
{for (i in seq(1, length(track_lt), by = 1)){
  a <- attr(track_lt[[i]], 'id')
  b <- mean(track_lt[[i]]$dist, na.rm = TRUE)
  c <- mean(track_lt[[i]]$rel.angle, na.rm = TRUE)
  d <- mean(track_lt[[i]]$abs.angle, na.rm = TRUE)
  summary_df[nrow(summary_df)+1, ] <- c(a, b, c, d)
  print(paste0(a, ' _mean_dist_in_cm_ ', b, ' _mean_rel.angle_ ', c, '_mean_abs.angle_', d))
  # print(length(which(tracks$Crab_ID == unique(tracks$Crab_ID)[i])))
} 
  rm(list = c('a', 'b', 'c', 'd', 'i'))
  gc()
  }


# Convert object class ltraj to class data frame
track_ltdf <- ld(track_lt)
# Create function to convert radians to degrees
rad2deg <- function(x) {(x * 180) / (pi)}
# Use function to transform radians to degree
library(dplyr)
track_ltdf <- track_ltdf %>% 
                  mutate(abs.angle_deg = rad2deg(abs.angle), rel.angle_deg = rad2deg(rel.angle))


# Checking values
min(track_ltdf$abs.angle, na.rm = TRUE)
max(track_ltdf$abs.angle, na.rm = TRUE)
sum(is.na(track_ltdf$abs.angle))

min(track_ltdf$rel.angle, na.rm = TRUE)
max(track_ltdf$rel.angle, na.rm = TRUE)
sum(is.na(track_ltdf$rel.angle))

min(track_ltdf$abs.angle_deg, na.rm = TRUE)
max(track_ltdf$abs.angle_deg, na.rm = TRUE)

min(track_ltdf$rel.angle_deg, na.rm = TRUE)
max(track_ltdf$rel.angle_deg, na.rm = TRUE)


# Re-order factors in tack_ltdf
track_ltdf$id <- factor(track_ltdf$id, c("crab_1", "crab_2","crab_3","crab_4","crab_5",
                                         "crab_6","crab_7","crab_8","crab_9","crab_10",
                                         "crab_11","crab_12","crab_13",
                                         "crab_14","crab_15","crab_16","crab_17"))


# Get indexes for first row of each crab individual
# vec <- which.max(track_ltdf$id=='crab_1')
# vec
vec <- c()
{for (i in seq(1, length(unique(track_ltdf$id)), by = 1)){
  vec[i] <- which.max(track_ltdf$id== unique(track_ltdf$id)[i])
}
  rm(list = c('i'))
  gc()
}
vec


# Calculate total distance travelled by individual
total.dist <- track_ltdf %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(tot.dist = sum(dist, na.rm = TRUE))
colnames(total.dist)[1] <- "Crab_ID"
total.dist


write.csv(total.dist, file = here::here('data/tidy', 'GP010016_fast_17_total_distance_travelled.csv'))


# Explore burst where sqrt(R2n) is greater than 20 cm
crabs_away20 <- which.ltraj(track_lt, 'sqrt(R2n)>20')
crabs_away20
# Extract burst which satisfy the criteria
burst_away20 <- track_lt[burst(track_lt)%in%crabs_away20$burst]
burst_away20


# Check NAs
runsNAltraj(burst_away20)


#save file
write.csv(track_ltdf, file = here::here('data/tidy', 'GP010016_fast_17_ltdf.csv'))
