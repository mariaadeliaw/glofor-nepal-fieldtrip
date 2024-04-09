library(BiodiversityR) # avoid package conflict by downloading from R console
library(dplyr)
library(readxl)
library(tidyr)

# location of data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"


# calculate ivi -----------------------------------------------------------


# Loop function for ivi per site
create_ivis <- function(site_data) {
  tryCatch({
    ivi <- site_data %>% importancevalue(site = "Accessibility",
                                         species = "species",
                                         count = "count",
                                         basal = "ba",
                                         factor = "") %>% 
      as.data.frame()
    ivi$species <- rownames(ivi)
    ivi <- ivi %>% relocate(species) %>% as_tibble()
    return(ivi)
  }, error = function(e) {
    message("Error processing data for site. Details: ", e)
    return(NULL)
  })
}

# plot inside group's study extent
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)


# 2016 ivi calculation ----------------------------------------------------


# 2016 ivi calc
psp_data_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Accessibility)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_2016)) {
  assign(paste0("psp_data_2016_", unique(psp_data_2016[[i]]$Accessibility)), psp_data_2016[[i]])
} 


# Loop through each element of the list
for (i in seq_along(psp_data_2016)) {
  site_data <- psp_data_2016[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Accessibility)
  ivi_site <- create_ivis(site_data)
  if (!is.null(ivi_site)) {
    assign(paste0("ivi_2016_", site_name), ivi_site)
  } else {
    message("Skipping site ", site_name, " due to errors.")
  }
}

# 2019 ivi calculation ----------------------------------------------------


# 2019 ivi calc
psp_data_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Accessibility)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_2019)) {
  assign(paste0("psp_data_2019_", unique(psp_data_2019[[i]]$Accessibility)), psp_data_2019[[i]])
} 

# Loop through each element of the list
for (i in seq_along(psp_data_2019)) {
  site_data <- psp_data_2019[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Accessibility)
  ivi_site <- create_ivis(site_data)
  if (!is.null(ivi_site)) {
    assign(paste0("ivi_2019_", site_name), ivi_site)
  } else {
    message("Skipping site ", site_name, " due to errors.")
  }
}

# 2024 ivi calculation ----------------------------------------------------


# 2024 ivi calc
psp_data_2024 <- psp_data %>% 
  read_excel(sheet = "data_2024") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Accessibility)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_2024)) {
  assign(paste0("psp_data_2024_", unique(psp_data_2024[[i]]$Accessibility)), psp_data_2024[[i]])
} 


# Loop through each element of the list
for (i in seq_along(psp_data_2024)) {
  site_data <- psp_data_2024[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Accessibility)
  ivi_site <- create_ivis(site_data)
  if (!is.null(ivi_site)) {
    assign(paste0("ivi_2024_", site_name), ivi_site)
  } else {
    message("Skipping site ", site_name, " due to errors.")
  }
}


# regeneration data -------------------------------------------------------
# 2024 ivi calc
psp_data_reg <- psp_data %>% 
  read_excel(sheet = "regeneration_data_2024") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Accessibility)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_reg)) {
  assign(paste0("psp_data_2024_", unique(psp_data_2024[[i]]$Accessibility)), psp_data_2024[[i]])
} 


# Loop through each element of the list
for (i in seq_along(psp_data_reg)) {
  site_data <- psp_data_reg[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Accessibility)
  ivi_site <- create_ivis(site_data)
  if (!is.null(ivi_site)) {
    assign(paste0("ivi_reg_", site_name), ivi_site)
  } else {
    message("Skipping site ", site_name, " due to errors.")
  }
}


# combine all ivi with the same site --------------------------------------


# remove unecessary objects
rm(site_data, ivi_site, site_name)


# List all ivi_[year]_[site] objects
ivi_objects <- ls(pattern = "^ivi_\\d{4}_\\w+$")

merge_ivis <- function(ivi_objects) {
  # Initialize an empty list to store merged dataframes
  merged_ivis <- list()
  
  # Loop through each unique site identity
  for (site in unique(sapply(ivi_objects, function(x) unlist(strsplit(x, "_"))[3]))) {
    # Subset ivi_objects with the same site identity
    ivi_site_objects <- grep(paste0("_", site, "$"), ivi_objects, value = TRUE)
    # Accessing the data frames from ivi_site_objects
    ivi_2016 <- get(ivi_site_objects[[1]])
    ivi_2019 <- get(ivi_site_objects[[2]])
    ivi_2024 <- get(ivi_site_objects[[3]])
    
    # Merge IVI data frames for three different years
    ivi_merged <- merge(ivi_2016, ivi_2019, by = "species", all = TRUE) %>%
      merge(ivi_2024, by = "species", all = TRUE) %>%
      rename(importance.value_2016 = importance.value.x,
             importance.value_2019 = importance.value.y,
             importance.value_2024 = importance.value) %>%
      replace(is.na(.), 0) %>%
      mutate(across(starts_with("importance.value"), ~ round(., 4)))
    
    # Append the merged dataframe to the list
    merged_ivis[[site]] <- ivi_merged
  }
  
  return(merged_ivis)
}
combined_ivis <- merge_ivis(ivi_objects)

library(gt)
library(janitor)
combined_ivis_filt_Easy <- combined_ivis$"Easy" %>%
  left_join(ivi_reg_Easy, by = "species") %>% 
  dplyr::select(species, importance.value_2016, importance.value_2019, importance.value_2024) %>%   
  replace(is.na(.), 0) %>% 
  slice_max(importance.value_2024, n = 10) %>% 
  clean_names(case = "title")
  
# combined_ivis_filt_Easy %>%
#   gt() %>%
#   gtsave("r/output/combined_ivis_filt_Easy.png", expand = 10)

combined_ivis_filt_Hard <- combined_ivis$"Hard"  %>%  
  left_join(ivi_reg_Hard, by = "species") %>% 
  dplyr::select(species, importance.value_2016, importance.value_2019, importance.value_2024) %>% 
  # rename(importance.value_reg = importance.value) %>% 
  replace(is.na(.), 0) %>% 
  slice_max(importance.value_2024, n = 10) %>% 
  clean_names(case = "title")


# combined_ivis_filt_Hard %>%
#   gt() %>%
#   gtsave("r/output/combined_ivis_filt_Hard.png", expand = 10)



# save rds
# saveRDS(combined_ivis, "r/rds/combined_ivis.rds")
# saveRDS(ivi_2016_204, "r/rds/ivi_2016_204.rds")
# saveRDS(ivi_2019_204, "r/rds/ivi_2019_204.rds")
# 
