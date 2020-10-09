R Scripts
==============

# Content:

1. [Data munging](#Data-munging)
2. [Data exploration](#Data-exploration)
3. [Data munging timestamps](#Data-munging-timestamps)
4. [Analysing crabs trajectories](#Analysing-crabs-trajectories)
5. [Analysing Utilization Distribution](#Analysing-Utilization-Distribution)
6. [Analysing Size](#Analysing-size)
7. [Munging crab interaction data](#Munging-crab-interaction-data)
8. [Interaction analysis based on behaviour](#Interaction-analysis-based-on-behaviour)
9. [Interaction analysis based on proximity threshold and observed behaviour](#Interaction-analysis-based-on-proximity-threshold-and-observed-behaviour)
10. [Other files](#Other-files)

## Data munging

Run file `dat_munging.R`

This file takes all tracking files and combines them in one data frame. It also transforms the tracking coordinates from pixel to centimeter scale using the conversion factor (manually input). This script adds four new columns corresponding to the moving average coordinates for each axis (i.e. x and y) and each coordinate measurement type (tracking center or blob moment). It currently uses a 10 frame window size for smoothing. Finally, the script creates individual alias for each crab in the form *crab_number*, for instance: *crab_1*, *crab_2*, etc. 

## Data exploration

Run file `dat_viz_animation.R`

This file produces several images and animations representing crabs tracking data. The script also creates a colour palette using `RColorBrewer`. This palette is saved on folder `data_clean/`.

## Analysing crabs trajectories

Run file `dat_trajectories.R`

This file creates various plots with crabs trajectories and movements metrics. Not all plots are saved. It also saves a new .csv file which contains `as.ltraj` information.

## Analysing Utilization Distribution

Run file `dat_UD.R`

This file calculates and plots kernel utilization distributions based on crabs trajectories.

## Analysing size

Run file `dat_size.R`

This file calculates crabs sizes based on Crabspy measures, and it calculates how well observed sizes fit to their surrogates.

## Munging crab interaction data

Run `dat_int_prep.R` 

This file calculates the closest distance between each pair of individuals. It also creates and saves the distance matrix among individuals as full matrix, lower diagonal, list and data frame. This scripts also includes few network plots.

## Interaction analysis based on behaviour

Run file `dat_int_matrix.R` 

This script includes the interaction characterization observed in videos. It produces network plots.

## Interaction analysis based on proximity threshold and observed behaviour

Run `dat_int_analysis.R` 

It produces main network plot considering proximity and antagonistic interactions. 

## Other files

`new_aes.R`

Amazing ggplot trick from this [gist](https://gist.github.com/eliocamp/eabafab2825779b88905954d84c82b32) which allows to add a second colour/fill scale. 
