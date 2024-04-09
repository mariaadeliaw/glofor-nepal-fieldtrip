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

result_ghyachowk <- movecost(#dtm = gork_cont,
                             origin = gork_vil[1,],
                             destin = psp,
                             studyplot = study_ext,
                             funct = "pcf", #pandof with downhill correction
                             #V = 0,
                             W = 60, #average body weight
                             L = 10, #carried load weight
                             N = 1.67 #terrain factors: Bad trails, stony outcrops and river beds 
)


result_milim <- movecost(#dtm = gork_cont,
                             origin = gork_vil[5,],
                             destin = psp,
                             funct = "pcf", #pandof with downhill correction
                             studyplot = study_ext,
                             #V = 0,
                             W = 60, #average body weight
                             L = 10, #carried load weight
                             N = 1.67 #terrain factors: Bad trails, stony outcrops and river beds 
)
result_thumgaum <- movecost(#dtm = gork_cont,
                         origin = gork_vil[4,],
                         destin = psp,
                         funct = "pcf", #pandof with downhill correction
                         studyplot = study_ext,
                         V = 0,
                         W = 60, #average body weight
                         L = 10, #carried load weight
                         N = 1.67 #terrain factors: Bad trails, stony outcrops and river beds 
)
cost_ghyachowk <- extract(result_ghyachowk$accumulated.cost.raster, psp)
cost_ghyachowk_df <- data.frame(
  PlotNo = psp$PlotNo,  # Include PlotNo column
  # x = psp$PlotNo,
  # y = point_data$y_column_name,
  cost_ghyachowk = cost_ghyachowk
)
cost_milim <- extract(result_milim$accumulated.cost.raster, psp)
cost_milim_df <- data.frame(
  PlotNo = psp$PlotNo,  # Include PlotNo column
  # x = psp$PlotNo,
  # y = point_data$y_column_name,
  cost_milim = cost_milim
)
cost_thumgaum <- extract(result_thumgaum$accumulated.cost.raster, psp)
cost_thumgaum_df <- data.frame(
  PlotNo = psp$PlotNo,  # Include PlotNo column
  # x = psp$PlotNo,
  # y = point_data$y_column_name,
  cost_thumgaum = cost_thumgaum
)
combined_cost <- cost_ghyachowk_df %>% 
  left_join(cost_milim_df, by = "PlotNo") %>% 
  left_join(cost_thumgaum_df, by = "PlotNo")

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
  
  # Save the result into a different object
  result_name <- paste0("result_", current_row$PlotNo)
  assign(result_name, result)
}

# Combine all dest.loc.w.cost variables into one SpatialPointsDataFrame
dest.loc.w.cost_df_comb <- do.call(rbind, lapply(list_dest.loc.w.cost, function(result) result$dest.loc.w.cost)) %>% 
  as_tibble() %>% 
  group_by(PlotNo) 

dest.loc.w.cost_df_comb_avg <- dest.loc.w.cost_df_comb %>% 
  summarise(average_cost = mean(cost, na.rm = TRUE))

# save rds
saveRDS(dest.loc.w.cost_df_comb, "r/rds/result_cost.rds")


# compare methods ---------------------------------------------------------

result_comp <- movecomp(dtm = gork_cont,
                        origin = current_row,
                        #studyplot = study_ext,
                        destin = psp,
                        choice = c("t",
                                   "wcs",
                                   "pcf"))

result_comp_df <- result_comp$LCPs %>% as_tibble()
