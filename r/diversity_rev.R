library(vegan)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(BiodiversityR)
library(ggpubr)
library(grid)


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
BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12, family="Arial"),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

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

# total community species richness
diversityresult(psp_div_2016, index = "richness", method = "pooled")
diversityresult(psp_div_2019, index = "richness", method = "pooled")
diversityresult(psp_div_2024, index = "richness", method = "pooled")

# 2016
sppr_2016 <- diversitycomp(x = psp_div_2016, 
              y=access_group, 
              factor1="Accessibility", 
              index="richness",
              method = "mean") %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Accessibility") %>% 
  as_tibble() %>% 
  rename(richness_2016 = richness)

# 2019
sppr_2019 <- diversitycomp(x = psp_div_2019, 
                           y=access_group, 
                           factor1="Accessibility", 
                           index="richness",
                           method = "mean") %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Accessibility") %>% 
  as_tibble() %>% 
  rename(richness_2019 = richness)

# 2024
sppr_2024 <- diversitycomp(x = psp_div_2024, 
                           y=access_group, 
                           factor1="Accessibility", 
                           index="richness",
                           method = "mean") %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Accessibility") %>% 
  as_tibble() %>% 
  rename(richness_2024 = richness)


# combine
sppr_comb <- sppr_2016 %>% 
  left_join(sppr_2019, by = "Accessibility") %>% 
  left_join(sppr_2024, by = "Accessibility") %>% 
  select(Accessibility, n, richness_2016, richness_2019, richness_2024)

# sppr_comb_plot <- ggplot(sppr_comb, aes(x = Accessibility, y = Species_Number, fill = Year)) +
#   geom_boxplot() +
#   labs(title = "Species Number Distribution by Accessibility Level",
#        x = "Accessibility",
#        y = "Species Number",
#        fill = "Year") +
#   theme_minimal() +
#   facet_wrap(~ Year)
# ggsave("r/output/sppr_comb_plot.jpeg", plot = sppr_comb_plot, width = 10, height = 6, dpi = 300)
# 

# Species Accumulation Curve ----------------------------------------------

sac.2016 <- accumcomp(psp_div_2016,
          y = access_group,
          factor = "Accessibility",
          method = "exact",
          legend = F,
          plotit = F) %>% 
  accumcomp.long()
sac.2019 <- accumcomp(psp_div_2019,
                      y = access_group,
                      factor = "Accessibility",
                      method = "exact",
                      legend = F,
                      plotit = F) %>% 
  accumcomp.long()
sac.2024 <- accumcomp(psp_div_2024,
                      y = access_group,
                      factor = "Accessibility",
                      method = "exact",
                      legend = F,
                      plotit = F) %>% 
  accumcomp.long()

sac.2016.plot <- ggplot(data=sac.2016, aes(x = Sites, y = Richness, ymax =  15, ymin= 0)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(data=subset(sac.2016, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=3) +
  # geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  BioR.theme +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = pal)

sac.2019.plot <- ggplot(data=sac.2019, aes(x = Sites, y = Richness, ymax =  15, ymin= 0)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(data=subset(sac.2019, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=3) +
  # geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  BioR.theme +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1")  +
  scale_color_manual(values = pal)

sac.2024.plot <- ggplot(data=sac.2024, aes(x = Sites, y = Richness, ymax =  15, ymin= 0)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=1) +
  geom_point(data=subset(sac.2024, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=3) +
  # geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  BioR.theme +
  scale_color_brewer(palette = "Set1")  +
  scale_color_manual(values = pal)

sac.combine <- ggpubr::ggarrange(sac.2016.plot + rremove("ylab") + rremove("xlab"), 
                                 sac.2019.plot + rremove("ylab") + rremove("xlab"), 
                                 sac.2024.plot + rremove("ylab") + rremove("xlab"),
                                 labels = c("2016", "2019", "2024"),
                                 ncol = 1, nrow = 3,
                                 common.legend = T,
                                 legend = "right",
                                 label.x = 0.02,  # Adjust the x position of the labels
                                 label.y = 0.9 ) %>% 
  annotate_figure(left = textGrob("Average number of species", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Sites", gp = gpar(cex = 1.3)))

ggsave("r/output/sac.combine.jpeg", plot = sac.combine, width = 10, height = 6, dpi = 300)

# Simpson ------------------------------------------------------------------

diversity_stat_2016 <- diversity(psp_div_2016, index = "invsimpson") %>% 
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
diversity_stat_2019 <- diversity(psp_div_2019, index = "invsimpson") %>% 
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
diversity_stat_2024 <- diversity(psp_div_2024, index = "invsimpson") %>% 
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
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err), width = 0.25, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = mean_label), position = position_dodge(width = 0.9), vjust = -0.5) +
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


# Renyi -------------------------------------------------------------------

renyi_2016 <- renyicomp(psp_div_2016, 
                        y = access_group,
                        factor = "Accessibility",
                        scales=c(0, 0.25, 0.5, 1, 2, 4, 8, Inf), permutations=100) %>% 
  renyicomp.long(label.freq=1)
renyi_2019 <- renyicomp(psp_div_2019, 
                        y = access_group,
                        factor = "Accessibility",
                        scales=c(0, 0.25, 0.5, 1, 2, 4, 8, Inf), permutations=100) %>% 
  renyicomp.long(label.freq=1)
renyi_2024 <- renyicomp(psp_div_2024, 
                        y = access_group,
                        factor = "Accessibility",
                        scales=c(0, 0.25, 0.5, 1, 2, 4, 8, Inf), permutations=100) %>% 
  renyicomp.long(label.freq=1)

renyi_2016_plot <- ggplot(data=renyi_2016, aes(x=Scales, y=Diversity, ymax=3, ymin=0)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(data=renyi_2016, 
            aes(x=Obs, colour=Grouping), 
            size=1) +
  geom_point(data=subset(renyi_2016, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), 
             alpha=0.8, size=3) +
  BioR.theme +
  scale_color_manual(values = pal) +
  labs(x=expression(alpha), y = "Diversity", colour = "Accessibility", shape = "Accessibility")
renyi_2019_plot <- ggplot(data=renyi_2019, aes(x=Scales, y=Diversity, ymax=3, ymin=0)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(data=renyi_2019, 
            aes(x=Obs, colour=Grouping), 
            size=1) +
  geom_point(data=subset(renyi_2019, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), 
             alpha=0.8, size=3) +
  BioR.theme +
  scale_color_manual(values = pal) +
  labs(x=expression(alpha), y = "Diversity", colour = "Accessibility", shape = "Accessibility")
renyi_2024_plot <- ggplot(data=renyi_2024, aes(x=Scales, y=Diversity, ymax=3, ymin=0)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(data=renyi_2024, 
            aes(x=Obs, colour=Grouping), 
            size=1) +
  geom_point(data=subset(renyi_2024, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), 
             alpha=0.8, size=3) +
  BioR.theme +
  scale_color_manual(values = pal) +
  labs(x=expression(alpha), y = "Diversity", colour = "Accessibility", shape = "Accessibility")

renyi_combined <- ggpubr::ggarrange(renyi_2016_plot + rremove("ylab") + rremove("xlab"), 
                                     renyi_2019_plot + rremove("ylab") + rremove("xlab"), 
                                     renyi_2024_plot + rremove("ylab") + rremove("xlab"),
                                 labels = c("2016", "2019", "2024"),
                                 ncol = 3, nrow = 1,
                                 common.legend = T,
                                 legend = "right",
                                 label.x = 0.02,  # Adjust the x position of the labels
                                 label.y = 0.9 ) %>% 
  annotate_figure(left = textGrob("Diversity", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                  bottom = textGrob(expression(alpha), gp = gpar(cex = 1.3)))

ggsave("r/output/renyi_combined.jpeg", plot = renyi_combined, width = 10, height = 6, dpi = 300)

