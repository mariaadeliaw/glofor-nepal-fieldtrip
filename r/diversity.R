library(vegan)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# location of data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"
# select plot numbers to be analyzed
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)

#accessibility group
access_group <- psp_data %>% 
  read_excel(sheet = "access_group")
access_group$PlotNo <- as.character(access_group$PlotNo)
clean_background <- theme(plot.background = element_rect("white"),
                          panel.background = element_rect("white"),
                          panel.grid = element_line("white"),
                          axis.line = element_line("gray25"),
                          axis.text = element_text(size = 12, color = "gray25"),
                          axis.title = element_text(color = "gray25"),
                          legend.text = element_text(size = 12),
                          legend.key = element_rect("white"))
pal <- c("lightsalmon1", "gold1", "palegreen4", "lightblue1", "khaki1", "lightcoral")

# 2016 H calc
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

# species richness
sppr_2016 <- specnumber(psp_div_2016)

# analysis of variance takes the same form as the usual models you'd see in R
# response ~ dependent, data = environmental grouping
sppr_2016_aov <- aov(sppr_2016 ~ Index, data = access_group)
summary(sppr_2016_aov)

sppr_2016 <- sppr_2016 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2016) <- c("PlotNo","Spec_Num")

sppr_2016_df <- sppr_2016 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

plot_sppr_2016 <- ggplot(sppr_2016_df, aes(x = Accessibility, y = Spec_Num, fill = Accessibility)) +
  geom_boxplot() +
  scale_fill_manual(values = pal) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12)) + 
  labs(x = "Accessibility",
       y = "Number of species per site",
       title = "Species richness 2016")
plot_sppr_2016

# diversity

diversity_stat_2016 <- diversity(psp_div_2016, index = "simpson") %>% 
  as.data.frame()
diversity_stat_2016 <- tibble::rownames_to_column(diversity_stat_2016, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016) <- c("PlotNo","Simpson_2016")


# bar plot per accessibility

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

plot_simpdiv_2016 <- ggplot(div_plot_df_2016, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Simpson diversity",
       title = "Simpson diversity 2016")
plot_simpdiv_2016

# 2019 H calc
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

# species richness
sppr_2019 <- specnumber(psp_div_2019) 

sppr_2019_aov <- aov(sppr_2019 ~ Index, data = access_group)
summary(sppr_2019_aov)

sppr_2019 <- sppr_2019 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2019) <- c("PlotNo","Spec_Num")

sppr_2019_df <- sppr_2019 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

plot_sppr_2019 <- ggplot(sppr_2019_df, aes(x = Accessibility, y = Spec_Num, fill = Accessibility)) +
  geom_boxplot() +
  scale_fill_manual(values = pal) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12)) + 
  labs(x = "Accessibility",
       y = "Number of species per site",
       title = "Species richness 2019")
plot_sppr_2019 + 

# diversity
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

plot_simpdiv_2019 <- ggplot(div_plot_df_2019, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Simpson diversity",
       title = "Simpson diversity 2019")

plot_simpdiv_2019

# 2024 H calc
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

# species richness
sppr_2024 <- specnumber(psp_div_2024) 

sppr_2024_aov <- aov(sppr_2024 ~ Index, data = access_group)
summary(sppr_2024_aov)

sppr_2024 <- sppr_2024 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "PlotNo") %>% 
  as_tibble()
colnames(sppr_2024) <- c("PlotNo","Spec_Num")

sppr_2024_df <- sppr_2024 %>% 
  # enframe() %>% 
  full_join(access_group, by = c("PlotNo"))

plot_sppr_2024 <- ggplot(sppr_2024_df, aes(x = Accessibility, y = Spec_Num, fill = Accessibility)) +
  geom_boxplot() +
  scale_fill_manual(values = pal) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  theme(legend.position = "none",
        plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey90"),
        axis.line = element_line("gray25"),
        axis.text = element_text(size = 12, color = "gray25"),
        axis.title = element_text(color = "gray25"),
        legend.text = element_text(size = 12)) + 
  labs(x = "Accessibility",
       y = "Number of species per site",
       title = "Species richness 2024")
plot_sppr_2024 + stat_compare_means(method = "t.test")

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

plot_simpdiv_2024 <- ggplot(div_plot_df_2024, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Simpson diversity",
       title = "Simpson diversity 2024")

plot_simpdiv_2024

# merge two years
# diversity_stat <- merge(diversity_stat_2016, diversity_stat_2024, by = "PlotNo", all = TRUE) %>% 
#   as_tibble() %>% 
#   mutate(difference = Simpson_2016 - Simpson_2024) %>% 
#   mutate(across(where(is.numeric), ~ round(., 4))) %>% 
#   arrange(difference)
# 
# diversity_stat_filt <- diversity_stat %>% 
#   mutate(PlotNo = as.numeric(PlotNo)) %>%
#   dplyr::select(PlotNo, difference)

# all years

# Combine data frames
combined_data <- inner_join(diversity_stat_2016, diversity_stat_2019, by = "PlotNo") %>%
  inner_join(diversity_stat_2024, by = "PlotNo")

# Reshape data into long format
combined_data_long <- combined_data %>%
  pivot_longer(cols = c(Simpson_2016, Simpson_2019, Simpson_2024),
               names_to = "Year", values_to = "Simpson_Value") %>%
  mutate(Year = gsub("Simpson_", "", Year))  # Extract the year from column names

# Plot
combined_data_plot <- ggplot(combined_data_long, aes(x = as.numeric(Year), y = Simpson_Value, color = PlotNo)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Simpson Diversity Index", color = "Plot No") +
  theme_minimal()

# shanon ------------------------------------------------------------------

# 2016
diversity_stat_2016_shanon <- diversity(psp_div_2016, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2016_shanon <- tibble::rownames_to_column(diversity_stat_2016_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2016_shanon) <- c("PlotNo","shannon_2016")
# bar plot per accessibility

div_plot_df_2016 <- diversity_stat_2016_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2016), 2),
            err = sd(shannon_2016)/sqrt(length(shannon_2016))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

plot_shandiv_2016 <- ggplot(div_plot_df_2016, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Shannon diversity",
       title = "Shannon diversity 2016")

plot_shandiv_2016

# 2019
diversity_stat_2019_shanon <- diversity(psp_div_2019, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2019_shanon <- tibble::rownames_to_column(diversity_stat_2019_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2019_shanon) <- c("PlotNo","shannon_2019")
div_plot_df_2019 <- diversity_stat_2019_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2019), 2),
            err = sd(shannon_2019)/sqrt(length(shannon_2019))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

plot_shandiv_2019 <- ggplot(div_plot_df_2019, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Shannon diversity",
       title = "Shannon diversity 2019")

plot_shandiv_2019

# 2024
diversity_stat_2024_shanon <- diversity(psp_div_2024, index = "shannon") %>% 
  as.data.frame()
diversity_stat_2024_shanon <- tibble::rownames_to_column(diversity_stat_2024_shanon, var = "PlotNo") %>% as_tibble()
colnames(diversity_stat_2024_shanon) <- c("PlotNo","shannon_2024")
div_plot_df_2024 <- diversity_stat_2024_shanon %>% 
  # join with site_type
  left_join(access_group, ., by = "PlotNo") %>% 
  # group by landtype
  group_by(Accessibility) %>% 
  # calculate mean and standard error of diversity
  summarize(mean = round(mean(shannon_2024), 2),
            err = sd(shannon_2024)/sqrt(length(shannon_2024))) %>% 
  dplyr::mutate(label = "mean") %>% 
  unite("mean_label", label, mean, sep = " = ", remove = FALSE)

plot_shandiv_2024 <- ggplot(div_plot_df_2024, aes(x = Accessibility, y = mean, fill = Accessibility)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.5) +
  geom_text(aes(x = Accessibility, y = mean + err + 0.07, label = mean_label)) +
  # scale_x_discrete(labels = c("dry \n (n = 96)", "mix \n (n = 59)", "riparian \n (n = 55)")) +
  # scale_y_continuous(limits = c(0, 2.75), expand = c(0,0)) +
  clean_background + 
  theme(legend.position = "none") +
  labs(x = "Accessibility",
       y = "Mean Shannon diversity",
       title = "Shannon diversity 2024")

plot_shandiv_2024

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
  summarise(avg_eveness = mean(eveness))

# Plotting
eveness_2016_plot <- ggplot(sum_even_2016, aes(x = Accessibility, y = avg_eveness)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  labs(title = "Eveness (2016)",
       x = "Accessibility",
       y = "Average Eveness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

eveness_2019 <- diversity_stat_2019_shanon %>% 
  left_join(sppr_2019, by = "PlotNo") %>% 
  mutate(eveness = shannon_2019/log(Spec_Num)) %>% 
  left_join(access_group, by = "PlotNo") %>% 
  group_by(Accessibility) %>% 
  drop_na(eveness)
# Summarize data by Accessibility
sum_even_2019 <- eveness_2019 %>%
  group_by(Accessibility) %>%
  summarise(avg_eveness = mean(eveness))

# Plotting
eveness_2019_plot <- ggplot(sum_even_2019, aes(x = Accessibility, y = avg_eveness)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  labs(title = "Eveness (2019)",
       x = "Accessibility",
       y = "Average Eveness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

eveness_2024 <- diversity_stat_2024_shanon %>% 
  left_join(sppr_2024, by = "PlotNo") %>% 
  mutate(eveness = shannon_2024/log(Spec_Num)) %>% 
  left_join(access_group, by = "PlotNo") %>% 
  group_by(Accessibility) %>% 
  drop_na(eveness)
# Summarize data by Accessibility
sum_even_2024 <- eveness_2024 %>%
  group_by(Accessibility) %>%
  summarise(avg_eveness = mean(eveness))

# Plotting
eveness_2024_plot <- ggplot(sum_even_2024, aes(x = Accessibility, y = avg_eveness)) +
  geom_col(color = "black") +
  scale_fill_manual(values = pal) +
  labs(title = "Eveness (2024)",
       x = "Accessibility",
       y = "Average Eveness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# merge two years
# diversity_stat_shanon <- merge(diversity_stat_2016_shanon, diversity_stat_2024_shanon, by = "PlotNo", all = TRUE) %>% 
#   as_tibble() %>% 
#   mutate(difference = shannon_2016 - shannon_2024) %>% 
#   mutate(across(where(is.numeric), ~ round(., 4))) %>% 
#   arrange(difference)
# 
# diversity_stat_filt_shanon <- diversity_stat_shanon %>% 
#   mutate(PlotNo = as.numeric(PlotNo)) %>%
#   dplyr::select(PlotNo, difference)

# all years

# Combine data frames
combined_data_shannon <- inner_join(diversity_stat_2016_shanon, diversity_stat_2019_shanon, by = "PlotNo") %>%
  inner_join(diversity_stat_2024_shanon, by = "PlotNo")

# Reshape data into long format
combined_data_shannon_long <- combined_data_shannon %>%
  pivot_longer(cols = c(shannon_2016, shannon_2019, shannon_2024),
               names_to = "Year", values_to = "Shannon_Value") %>%
  mutate(Year = gsub("shannon_", "", Year))  # Extract the year from column names

# Plot
combined_data_shannon_plot <- ggplot(combined_data_shannon_long, aes(x = as.numeric(Year), y = Shannon_Value, color = PlotNo)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Shannon Diversity Index", color = "Plot No") +
  theme_minimal()

library(ggpubr)

figure_shannon <- ggarrange(plot_shandiv_2016, plot_shandiv_2019, plot_shandiv_2024,
                    # labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
figure_specrich <- ggarrange(plot_sppr_2016, plot_sppr_2019, plot_sppr_2024,
                             ncol = 3, nrow = 1)
figure_simpdiv <- ggarrange(plot_simpdiv_2016, plot_simpdiv_2019, plot_simpdiv_2024,
                             ncol = 3, nrow = 1)
figure_eveness <- ggarrange(eveness_2016_plot, eveness_2019_plot, eveness_2024_plot,
                            ncol = 3, nrow = 1)
# save rds ----------------------------------------------------------------
# 
# saveRDS(diversity_stat, "r/rds/diversity_stat.rds")
# saveRDS(diversity_stat_shanon, "r/rds/diversity_stat_shanon.rds")
