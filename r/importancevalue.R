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
    ivi <- site_data %>% importancevalue(site = "PlotNo",
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
plot_numbers <- c(90005, 90010, 90001, 90004, 90006, 90028, 90011)


# 2016 ivi calculation ----------------------------------------------------


# 2016 ivi calc
psp_data_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH2016) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Site)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_2016)) {
  assign(paste0("psp_data_2016_", unique(psp_data_2016[[i]]$Site)), psp_data_2016[[i]])
} 


# Loop through each element of the list
for (i in seq_along(psp_data_2016)) {
  site_data <- psp_data_2016[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Site)
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
  rename(dbh = DBH2019) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  group_split(Site)

# Now, create separate data frames for each Site value
for (i in seq_along(psp_data_2019)) {
  assign(paste0("psp_data_2019_", unique(psp_data_2019[[i]]$Site)), psp_data_2019[[i]])
} 

# Loop through each element of the list
for (i in seq_along(psp_data_2019)) {
  site_data <- psp_data_2019[[i]] %>% as.data.frame()
  site_name <- unique(site_data$Site)
  ivi_site <- create_ivis(site_data)
  if (!is.null(ivi_site)) {
    assign(paste0("ivi_2019_", site_name), ivi_site)
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
    # Check if either ivi_2016 or ivi_2019 has only 2 columns
    if (ncol(ivi_2016) == 2 || ncol(ivi_2019) == 2) {
      message("Skipping site ", site, " due to insufficient number of plots")
      next  # Skip to the next iteration
    }
    # compare 2016 and 2019
    ivi_comp <- merge(ivi_2016, ivi_2019, by = "species", all = TRUE) %>% 
      as_tibble() %>% 
      dplyr::select(species, importance.value.x, importance.value.y) %>% 
      rename(importance.value.2016 = importance.value.x) %>% 
      rename(importance.value.2019 = importance.value.y) %>% 
      replace(is.na(.), 0) %>% 
      mutate(difference = importance.value.2019 - importance.value.2016) %>% 
      mutate(across(where(is.numeric), ~ round(., 4))) %>% 
      arrange(difference)
    # Append the merged dataframe to the list
    merged_ivis[[site]] <- ivi_comp
  }
  return(merged_ivis)
}
combined_ivis <- merge_ivis(ivi_objects)
