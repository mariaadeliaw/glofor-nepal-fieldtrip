library(vegan)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(BiodiversityR)


# location of data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

# select plot numbers to be analyzed
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)

# accessibility group
access_group <- psp_data %>% 
  read_excel(sheet = "access_group") %>% 
  as.data.frame()
access_group$PlotNo <- as.character(access_group$PlotNo)
access_group$Accessibility <- as.factor(access_group$Accessibility)
rownames(access_group) <- access_group$PlotNo

# graphs
clean_background <- theme(plot.background = element_rect("white"),
                          panel.background = element_rect("white"),
                          panel.grid = element_line("white"),
                          axis.line = element_line("gray25"),
                          axis.text = element_text(size = 12, color = "gray25"),
                          axis.title = element_text(color = "gray25"),
                          legend.text = element_text(size = 12),
                          legend.key = element_rect("white"))
pal <- c("lightsalmon1", "gold1", "palegreen4", "lightblue1", "khaki1", "lightcoral")

# prepare vegan dataset
psp_div_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  filter(DBH >= 10) %>%
  rename(dbh = DBH) %>% 
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

psp_div_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  filter(DBH >= 10) %>%
  rename(dbh = DBH) %>% 
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

psp_div_2024 <- psp_data %>% 
  read_excel(sheet = "data_2024") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  filter(DBH >= 10) %>%
  rename(dbh = DBH) %>%
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  dplyr::select(species, PlotNo, count) %>% 
  group_by(species, PlotNo) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>%
  pivot_wider(names_from = species, values_from = count, values_fill = 0) %>% 
  as.data.frame()
rownames(psp_div_2024) <- psp_div_2024$PlotNo
psp_div_2024[,1] <- NULL

psp_div_comb <- bind_rows(psp_div_2016, psp_div_2019, psp_div_2024)

# Species Richness -----------------------------------------------------------------

# 2016
sppr_2016 <- specnumber(psp_div_2016)
sppr_2016 <- sppr_2016 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2016) <- c("PlotNo","Spec_Num")

sppr_2016_df <- sppr_2016 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

# 2019
sppr_2019 <- specnumber(psp_div_2019) 
sppr_2019 <- sppr_2019 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2019) <- c("PlotNo","Spec_Num")
sppr_2019_df <- sppr_2019 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

# 2024
sppr_2024 <- specnumber(psp_div_2024) 
sppr_2024 <- sppr_2024 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2024) <- c("PlotNo","Spec_Num")

sppr_2024_df <- sppr_2024 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

# combine
sppr_comb <- sppr_2016_df %>% 
  left_join(sppr_2019_df, by = "PlotNo") %>% 
  left_join(sppr_2024_df, by = "PlotNo") %>% 
  select(PlotNo, Accessibility, Spec_Num.x, Spec_Num.y, Spec_Num) %>% 
  rename(Species_Number_2016 = Spec_Num.x,
         Species_Number_2019 = Spec_Num.y,
         Species_Number_2024 = Spec_Num) %>% 
  gather(Year, Species_Number, -PlotNo, -Accessibility) %>%
  mutate(Year = factor(Year, levels = c("Species_Number_2016", "Species_Number_2019", "Species_Number_2024")),
         Year = gsub("Species_Number_", "", Year))

sppr_comb_plot <- ggplot(sppr_comb, aes(x = Accessibility, y = Species_Number, fill = Year)) +
  geom_boxplot() +
  labs(title = "Species Number Distribution by Accessibility Level",
       x = "Accessibility",
       y = "Species Number",
       fill = "Year") +
  theme_minimal() +
  facet_wrap(~ Year)
ggsave("r/output/sppr_comb_plot.jpeg", plot = sppr_comb_plot, width = 10, height = 6, dpi = 300)


# Species Accumulation Curve ----------------------------------------------

sac.2016 <- accumcomp(psp_div_2016,
          y = access_group,
          factor = "Accessibility",
          method = "exact",
          legend = F,)
sac.2019 <- accumcomp(psp_div_2019,
                      y = access_group,
                      factor = "Accessibility",
                      method = "exact",
                      legend = F)
sac.2024 <- accumcomp(psp_div_2024,
                      y = access_group,
                      factor = "Accessibility",
                      method = "exact",
                      legend = F)

# Simpson ------------------------------------------------------------------

diversity_stat_2016 <- diversity(psp_div_2016, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2016 <- tibble::rownames_to_column(diversity_stat_2016, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016) <- c("PlotNo","Simpson_2016")

div_plot_df_2016 <- diversity_stat_2016 %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by accessibility
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(Simpson_2016), 2),
            err = sd(Simpson_2016)/sqrt(length(Simpson_2016))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# 2019
diversity_stat_2019 <- diversity(psp_div_2019, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2019 <- tibble::rownames_to_column(diversity_stat_2019, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2019) <- c("PlotNo","Simpson_2019")

# bar plot per accessibility

div_plot_df_2019 <- diversity_stat_2019 %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(Simpson_2019), 2),
            err = sd(Simpson_2019)/sqrt(length(Simpson_2019))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# 2024
diversity_stat_2024 <- diversity(psp_div_2024, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2024 <- tibble::rownames_to_column(diversity_stat_2024, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2024) <- c("PlotNo","Simpson_2024")

# bar plot per accessibility

div_plot_df_2024 <- diversity_stat_2024 %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(Simpson_2024), 2),
            err = sd(Simpson_2024)/sqrt(length(Simpson_2024))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# Combine diversity data for 2016, 2019, and 2024
div_plot_df_combined <- bind_rows(
  div_plot_df_2016 %>% mutate(Year = 2016),
  div_plot_df_2019 %>% mutate(Year = 2019),
  div_plot_df_2024 %>% mutate(Year = 2024)
)

plot_simpdiv_combined <- ggplot(div_plot_df_combined, aes(x = factor(Year), y = mean, fill = Accessibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.25, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = mean_label), position = position_dodge(width = 0.8), vjust = -0.5) +
  scale_fill_manual(values = pal, name = "Accessibility") +
  labs(x = "Year",
       y = "Mean Simpson Dominance",
       title = "Simpson's Dominance Comparison (2016, 2019, 2024)") +
  theme_minimal() +
  theme(legend.position = "top")

# Set dimensions and resolution
ggsave("r/output/plot_simpdiv_combined.jpeg", plot = plot_simpdiv_combined, width = 10, height = 6, dpi = 300)






# Shanon ------------------------------------------------------------------

diversity_stat_2016_shanon <- diversity(psp_div_2016, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2016_shanon <- tibble::rownames_to_column(diversity_stat_2016_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016_shanon) <- c("PlotNo","shannon_2016")

div_plot_df_2016_shanon <- diversity_stat_2016_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2016), 2),
            err = sd(shannon_2016)/sqrt(length(shannon_2016))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# 2019
diversity_stat_2019_shanon <- diversity(psp_div_2019, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2019_shanon <- tibble::rownames_to_column(diversity_stat_2019_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2019_shanon) <- c("PlotNo","shannon_2019")

div_plot_df_2019_shanon <- diversity_stat_2019_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2019), 2),
            err = sd(shannon_2019)/sqrt(length(shannon_2019))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# 2024
diversity_stat_2024_shanon <- diversity(psp_div_2024, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2024_shanon <- tibble::rownames_to_column(diversity_stat_2024_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2024_shanon) <- c("PlotNo","shannon_2024")
div_plot_df_2024_shanon <- diversity_stat_2024_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2024), 2),
            err = sd(shannon_2024)/sqrt(length(shannon_2024))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# Combine diversity data for 2016, 2019, and 2024
div_plot_df_combined_shanon <- bind_rows(
  div_plot_df_2016_shanon %>% mutate(Year = 2016),
  div_plot_df_2019_shanon %>% mutate(Year = 2019),
  div_plot_df_2024_shanon %>% mutate(Year = 2024)
)

# Create the bar plot with error bars
plot_shandiv_combined <- ggplot(div_plot_df_combined_shanon, aes(x = factor(Year), y = mean, fill = Accessibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.25, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = mean_label), position = position_dodge(width = 0.8), vjust = -0.5) +
  scale_fill_manual(values = pal, name = "Accessibility") +
  labs(x = "Year",
       y = "Mean Shannon Diversity",
       title = "Shannon Diversity Comparison (2016, 2019, 2024)") +
  theme_minimal() +
  theme(legend.position = "top")

# Set dimensions and resolution
ggsave("r/output/plot_shandiv_combined.jpeg", plot = plot_shandiv_combined, width = 10, height = 6, dpi = 300)


# Eveness -----------------------------------------------------------------

# Eveness
eveness_2016 <- diversity_stat_2016_shanon %>% 
  left_join(sppr_2016, by = "PlotNo") %>% 
  mutate(eveness = shannon_2016/log(Spec_Num)) %>% 
  left_join(access_group, by = "PlotNo") %>% 
  group_by(Accessibility) %>% 
  drop_na(eveness)
# Summarize data by Accessibility
sum_even_2016 <- eveness_2016 %>%
  group_by(Accessibility) %>%
  summarize(mean = round(mean(eveness), 2),
            err = sd(eveness)/sqrt(length(eveness))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

eveness_2019 <- diversity_stat_2019_shanon %>% 
  left_join(sppr_2019, by = "PlotNo") %>% 
  mutate(eveness = shannon_2019/log(Spec_Num)) %>% 
  left_join(access_group, by = "PlotNo") %>% 
  group_by(Accessibility) %>% 
  drop_na(eveness)
# Summarize data by Accessibility
sum_even_2019 <- eveness_2019 %>%
  group_by(Accessibility) %>%
  summarize(mean = round(mean(eveness), 2),
            err = sd(eveness)/sqrt(length(eveness))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

eveness_2024 <- diversity_stat_2024_shanon %>% 
  left_join(sppr_2024, by = "PlotNo") %>% 
  mutate(eveness = shannon_2024/log(Spec_Num)) %>% 
  left_join(access_group, by = "PlotNo") %>% 
  group_by(Accessibility) %>% 
  drop_na(eveness)
# Summarize data by Accessibility
sum_even_2024 <- eveness_2024 %>%
  group_by(Accessibility) %>%
  summarize(mean = round(mean(eveness), 2),
            err = sd(eveness)/sqrt(length(eveness))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

# Combine data
combined_data_even <- bind_rows(
  mutate(sum_even_2016, Year = 2016),
  mutate(sum_even_2019, Year = 2019),
  mutate(sum_even_2024, Year = 2024)
)

# Plotting
combined_plot_even <- ggplot(combined_data_even, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.25, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = mean_label), position = position_dodge(width = 0.8), vjust = -0.5) +
  scale_fill_manual(values = pal, name = "Accessibility") +
  labs(title = "Eveness",
       x = "Accessibility",
       y = "Average Eveness") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~Year)

# Set dimensions and resolution
ggsave("r/output/combined_plot_even.jpeg", plot = combined_plot_even, width = 10, height = 6, dpi = 300)

