library(vegan)
library(dplyr)
library(tidyr)

# location of data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

plot_numbers <- c(90005, 90010, 90001, 90004, 90006, 90028, 90011)

# 2016 H calc
psp_div_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH2016) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  dplyr::select(species, PlotNo, count) %>% 
  group_by(species, PlotNo) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>% 
  as.data.frame()

rownames(psp_div_2016) <- psp_div_2016$PlotNo
psp_div_2016[,1] <- NULL

diversity_stat_2016 <- diversity(psp_div_2016, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2016 <- tibble::rownames_to_column(diversity_stat_2016, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016) <- c("PlotNo","Simpson_2016")

# 2019 H calc
psp_div_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH2019) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  dplyr::select(species, PlotNo, count) %>% 
  group_by(species, PlotNo) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>% 
  as.data.frame()

rownames(psp_div_2019) <- psp_div_2019$PlotNo
psp_div_2019[,1] <- NULL

diversity_stat_2019 <- diversity(psp_div_2019, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2019 <- tibble::rownames_to_column(diversity_stat_2019, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2019) <- c("PlotNo","Simpson_2019")

# merge two years
diversity_stat <- merge(diversity_stat_2016, diversity_stat_2019, by = "PlotNo", all = TRUE) %>% 
  as_tibble() %>% 
  mutate(difference = Simpson_2016 - Simpson_2019) %>% 
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  arrange(difference)

diversity_stat_filt <- diversity_stat %>% 
  mutate(PlotNo = as.numeric(PlotNo)) %>%
  dplyr::select(PlotNo, difference)


# shanon ------------------------------------------------------------------

# 2016
diversity_stat_2016_shanon <- diversity(psp_div_2016, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2016_shanon <- tibble::rownames_to_column(diversity_stat_2016_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016_shanon) <- c("PlotNo","shannon_2016")

# 2019
diversity_stat_2019_shanon <- diversity(psp_div_2019, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2019_shanon <- tibble::rownames_to_column(diversity_stat_2019_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2019_shanon) <- c("PlotNo","shannon_2019")

# merge two years
diversity_stat_shanon <- merge(diversity_stat_2016_shanon, diversity_stat_2019_shanon, by = "PlotNo", all = TRUE) %>% 
  as_tibble() %>% 
  mutate(difference = shannon_2016 - shannon_2019) %>% 
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  arrange(difference)

diversity_stat_filt_shanon <- diversity_stat %>% 
  mutate(PlotNo = as.numeric(PlotNo)) %>%
  dplyr::select(PlotNo, difference)