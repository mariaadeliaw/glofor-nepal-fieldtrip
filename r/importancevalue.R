library(BiodiversityR) # avoid package conflict by downloading from R console
library(dplyr)
library(readxl)

# import data from local
# data has been cleaned manually in excel, separated into different sheet for different year obs
# empty rows from missing observation in each years had been deleted manually too
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

# plot inside group's study extent
plot_numbers <- c(90005, 90010, 90001, 90004, 90006, 90028, 90011)

# 2016 ivi calc
psp_data_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) # basal area in meter square

ivi_2016 <- psp_data_2016 %>% 
  importancevalue(site = "PlotNo",
                  species = "species",
                  count = "count",
                  basal = "ba",
                  factor = "") %>% 
  as.data.frame()
ivi_2016$species <- rownames(ivi_2016)
ivi_2016 <- ivi_2016 %>% relocate(species) %>% as_tibble()


# 2019 ivi calc
psp_data_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) # basal area in meter square

ivi_2019 <- psp_data_2019 %>% 
  importancevalue(site = "PlotNo",
                  species = "species",
                  count = "count",
                  basal = "ba",
                  factor = "") %>% 
  as.data.frame()
ivi_2019$species <- rownames(ivi_2019)
ivi_2019 <- ivi_2019 %>% relocate(species) %>% as_tibble()
