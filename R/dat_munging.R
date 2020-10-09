library(here)
here::here()


# Gather all file names contained inside foler 'tracks' in a list
file.names <- list.files('data/raw/tracks', pattern = "*.csv", full.names = TRUE)
file.names


# Take each file name, read it and save it in the environment.
# gc = garbage collection takes place. Useful when reading large objects and returning memory to OS
for (i in 1:length(file.names)){
  start.stripped.i <- unlist(strsplit(x = file.names[i], split = 'data/raw/tracks/'))[2]
  obj.name.i <- unlist(strsplit(x = start.stripped.i, split = '\\.'))[1] # escape character before. so it's not treated as a wildcard 
  X.i <- read.csv(file.names[i], skip=4, header = TRUE, colClasses = c("numeric","character", "numeric", 'character', 
                                                                       'numeric', 'numeric', 'numeric', 'numeric',
                                                                       "character", "character", "character", "numeric",
                                                                       "numeric", "numeric", "character"))
  assign(x = obj.name.i, value = X.i)
  rm(list = c('start.stripped.i', 'obj.name.i', 'X.i', 'i'))
  gc()
}


# Create empty list and add all files contained in folder 'tracks'
my_list <- list()
for (i in 1:length(file.names)){
  start.stripped.i <- unlist(strsplit(x = file.names[i], split = 'data/raw/tracks/'))[2]
  # print(start.stripped.i)
  obj.name.i <- unlist(strsplit(x = start.stripped.i, split = '\\.'))[1]
  print(obj.name.i)
  my_list[[obj.name.i]] <- get(obj.name.i)
  #obj.name.i[,2] <- gsub('[()]', '', obj.name.i[,2])
  #obj.name.i <- separate(obj.name.i, Crab_Position, into = c('x_coord', 'y_coord'), sep = ',')
  rm(list = c('start.stripped.i', 'obj.name.i', 'i'))
  gc()
}


library(plyr)
library(tidyr)
# Create a dataframe from list
tracks <- ldply(my_list, data.frame)


# Transform coordinates units from pixels to cm
tracks$Crab_position_x <- tracks$Crab_position_x*0.1584
tracks$Crab_position_y <- tracks$Crab_position_y*0.1584
tracks$Crab_position_cx <- tracks$Crab_position_cx*0.1584
tracks$Crab_position_cy <- tracks$Crab_position_cy*0.1584


rm(list = names(my_list))
rm(file.names)
rm(my_list)


# Smooth path by using moving average
# library(data.table)
detach(package:plyr, unload = TRUE)
detach(package:tidyr)
library(dplyr)
library(zoo)


tracks <- tracks %>% 
                group_by(Crab_ID) %>%
                mutate(x_smooth = rollapply(Crab_position_x, 10, mean, fill = NA), 
                       y_smooth = rollapply(Crab_position_y, 10, mean, fill = NA),
                       cx_smooth = rollapply(Crab_position_cx, 10, mean, fill = NA), 
                       cy_smooth = rollapply(Crab_position_cy, 10, mean, fill = NA))
                        

# Create alias for individual crabs
Crab_ID <- character(length = length(unique(tracks$Crab_ID)))
Crab_alias_ID <- character(length = length(unique(tracks$Crab_ID)))

{for(i in seq(1, length(unique(tracks$Crab_ID)), by = 1)){
  Crab_ID[i] <- unique(tracks$Crab_ID)[i]
  crab = 'crab_'
  Crab_alias_ID[i] = paste0(crab, i)
  }
  Crab_alias_ID <- as.data.frame(cbind(Crab_ID, Crab_alias_ID))
  tracks <- (merge(tracks, Crab_alias_ID, by = "Crab_ID"))
  rm(list = c('i', 'crab', 'Crab_ID', 'Crab_alias_ID'))
  gc()
}


# Make 'Crab_alias' the first column in dataframe
tracks <- tracks[, c(21, 1:20)]
colnames(tracks)[2] <- 'Crab_number'
colnames(tracks)[1] <- 'Crab_ID'


# Order observation by Frame_number and Crab_ID
tracks <- tracks[order(tracks$Crab_ID, tracks$Frame_number),]


# Create timestamps
# Import video meta information
video_log <- read.csv('data/raw/video_log.csv', header = TRUE, skip = 1)


# Define target video: if the video used was modified at any time (i.e. frames dropped, etc) the original video
# should be only used to get the video date and time.
# Define target video using video file name without file extension
target_v <- 'GP010016'


# Import video creation date and time
# Set system time to show three decimal digits in seconds
options(digits.secs = 6)
Sys.time()
# Convert data column 'created' from factor to character type
video_log$created <- as.character(video_log$created)
# Extract date and time of video creation for target file
startv <- video_log[video_log$file_name == target_v, 'created']
# Remove 'z' and 'T' characters from date/time object
startv <- substr(startv, 1, nchar(startv)-1)
startv <-gsub('\\T', ' ', startv)
startv
# Convert character objetc to POSIXlt type
library(chron)
startv <- as.POSIXlt(startv, tz = 'Australia/Brisbane')
startv


# Import video frame rate per second
# Redefine target video (only if the original video was modified, for instance video was speed up)
target_v <- 'GP010016_fast'
# Convert data column 'fps' from factor to character type
video_log$fps <- as.character(video_log$fps)
# Extratc fps from traget video
fps <- video_log[video_log$file_name == target_v, 'fps']
# Evaluate character vector as numeric to get frame rate as numeric
fps <- eval(parse(text = fps))
fps


# Import total number of frames in video
frames <- video_log[video_log$file_name == target_v, 'nb_frames']
frames


# Import video total duration
duration <- video_log[video_log$file_name == target_v, 'duration']
duration <- as.character(duration)
# duration <- times(duration)
duration
duration_ana_hour <- as.numeric(strsplit(duration, ':')[[1]][1])
duration_ana_min <- as.numeric(strsplit(duration, ':')[[1]][2])
duration_ana_sec <- as.numeric(strsplit(duration, ':')[[1]][3])


# Import real time elapsed in original video
target_v <- 'GP010016'
duration_obs <- video_log[video_log$file_name == target_v, 'duration']
duration_obs <- as.character(duration_obs)
duration_obs
duration_obs_hour <- as.numeric(strsplit(duration_obs, ':')[[1]][1])
duration_obs_min <- as.numeric(strsplit(duration_obs, ':')[[1]][2])
duration_obs_sec <- as.numeric(strsplit(duration_obs, ':')[[1]][3])

# Calculate final date and time of video observation
startv
endv <- startv
endv$hour <- endv$hour + duration_obs_hour
endv$min <- endv$min + duration_obs_min
endv$sec <- endv$sec + duration_obs_sec
startv
endv


# Print frame length per crab individual
{for (i in seq(1, length(unique(tracks$Crab_ID)), by = 1)){
  print(length(which(tracks$Crab_ID == unique(tracks$Crab_ID)[i])))
}
  rm(list = c('i'))
  gc()
}


# Check if all frame numbers have the corresponding timestamps
plot(tracks$Frame_number~tracks$Time_lapsed_since_start.secs.)
# It seems that there are 4 observations with incorrect timestamps (This might come from using an ealier version of Crabspy???)
tracks[tracks$Time_lapsed_since_start.secs.==0 & tracks$Frame_number!=1,]
# Find the correct timestamp for these four frame numbers
tracks[tracks$Frame_number==39,] #row 6700, value 1.626625
tracks[tracks$Frame_number==670,] # row 21549, value 27.94458
tracks[tracks$Frame_number==820,] # row 26371, value 34.20083
tracks[tracks$Frame_number==1357,] # row 1355, value 56.59821
# Replace mistakes with right timestamps
ind.row <- which(tracks$Time_lapsed_since_start.secs.==0 & tracks$Frame_number!=1)
ind.row
tracks[ind.row,6] <- c(56.59821, 27.94458, 34.20083, 1.626625)
# Check changes were done well
plot(tracks$Frame_number~tracks$Time_lapsed_since_start.secs.)


total_obs_seconds <- (duration_obs_hour*60*60) + (duration_obs_min*60) + duration_obs_sec
total_ana_Seconds <- (duration_ana_hour*60*60) + (duration_ana_min*60) + duration_ana_sec
total_obs_seconds/total_ana_Seconds


# Create  a timestamp which reflects real time scale observed
tracks$Time_lapsed_since_start.secs. <- tracks$Time_lapsed_since_start.secs. * 4.00083
tracks$Time_lapsed_since_start.secs. <- tracks$Time_lapsed_since_start.secs. + startv
plot(tracks$Frame_number~tracks$Time_lapsed_since_start.secs.)


# Save data to 'data_clean' folder
dir.create("data/tidy")
write.csv(tracks, file = here::here('data/tidy', 'GP010016_fast_17.csv'))
