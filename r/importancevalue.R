library(BiodiversityR) # avoid package conflict by downloading from R console
library(dplyr)
library(readxl)

# import data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

# Assuming "PlotNo" column contains the Plot numbers
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
                  factor = "")
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
                  factor = "")
