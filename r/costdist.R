library(movecost)
library(sf)
library(raster)
# library(rgdal)
library(sp)
library(terra)
library(dplyr)

spat_data_dir <- "D:/GLOFOR/UCPH/Preparing-Nepal/spatial_data/"

study_ext_path <- paste0(spat_data_dir, "/final-extent-proposal.shp")
study_ext <- st_read(study_ext_path)

gork_vil_path <- paste0(spat_data_dir, "/settlement_gorkha_list.shp")
gork_vil <- st_read(gork_vil_path) %>% 
  st_crop(study_ext) %>% 
  as("Spatial")

gork_vil_data <- as_tibble(gork_vil)

psp_path <- paste0(spat_data_dir, "/CF-Plots.shp")
psp <- st_read(psp_path) %>% 
  st_crop(study_ext) %>%
  dplyr::select(PlotNo, ForestType, Site) %>% 
  as("Spatial")

# psp <- psp[,-2]
# gork_vil <- gork_vil[,-(1:21)]

gork_cont_path <- paste0(spat_data_dir, "/N28E084_N28E084.vrt")
gork_cont <- raster::raster(gork_cont_path) %>% 
  crop(extent(study_ext))


# Create an empty list to store the results
list_dest.loc.w.cost <- list()

# Loop through each row of gork_vil
for (i in 1:nrow(gork_vil)) {
  # Extract the current row
  current_row <- gork_vil[i, ]
  
  # Perform the operation using the current row
  result <- movecost(dtm = gork_cont,
                     origin = current_row,
                     #studyplot = study_ext,
                     destin = psp)
  
  # Store the result in the list
  list_dest.loc.w.cost[[i]] <- result
}

# Combine all dest.loc.w.cost variables into one SpatialPointsDataFrame
dest.loc.w.cost_df_comb <- do.call(rbind, lapply(list_dest.loc.w.cost, function(result) result$dest.loc.w.cost)) %>% 
  as_tibble() %>% 
  group_by(Site) %>%
  summarise(average_cost = mean(cost, na.rm = TRUE))
