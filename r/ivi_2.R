library(BiodiversityR)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(tidyverse)
library(readxl)

# location of data from local
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

# plot inside group's study extent
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)

# 2016 ivi calc
psp_data_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% # basal area in meter square
  select(PlotNo, dbh, Height2016, species, Accessibility, count, ba) %>% 
  rename(height = Height2016) %>% 
  mutate(year = rep("2016"))
psp_data_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% 
  select(PlotNo, dbh, Height2019, species, Accessibility, count, ba) %>% 
  rename(height = Height2019) %>% 
  mutate(year = rep("2019"))
psp_data_2024 <- psp_data %>% 
  read_excel(sheet = "data_2024") %>% 
  filter(PlotNo %in% plot_numbers) %>% 
  as.data.frame() %>% 
  rename(dbh = DBH) %>% 
  drop_na(dbh) %>% 
  mutate(count = rep(1, each = nrow(.))) %>% 
  mutate(ba = 3.14*(dbh/100/2)^2) %>% 
  select(PlotNo, dbh, Height, species, Accessibility, count, ba) %>% 
  rename(height = Height) %>% 
  mutate(year = rep("2024"))
  
psp_bind <- rbind(psp_data_2016, psp_data_2019, psp_data_2024)

iv <- importancevalue.comp(psp_bind, site='year', species='species', count='count', 
                                      basal='ba', factor='PlotNo')
summary(iv)

iv_90001 <- data.frame(plot=rep("90001", nrow(iv$"90001")),
                     species=rownames(iv$"90001"),
                     importance.value=iv$"90001"[, "importance.value"])
iv_90004 <- data.frame(plot=rep("90004", nrow(iv$"90004")),
                       species=rownames(iv$"90004"),
                       importance.value=iv$"90004"[, "importance.value"])
iv_90006 <- data.frame(plot=rep("90006", nrow(iv$"90006")),
                       species=rownames(iv$"90006"),
                       importance.value=iv$"90006"[, "importance.value"])
iv_90010 <- data.frame(plot=rep("90010", nrow(iv$"90010")),
                       species=rownames(iv$"90010"),
                       importance.value=iv$"90010"[, "importance.value"])
iv_90011 <- data.frame(plot=rep("90011", nrow(iv$"90011")),
                       species=rownames(iv$"90011"),
                       importance.value=iv$"90011"[, "importance.value"])
iv_90028 <- data.frame(plot=rep("90028", nrow(iv$"90028")),
                       species=rownames(iv$"90028"),
                       importance.value=iv$"90028"[, "importance.value"])

row.names(iv_90004) <- row.names(iv_90001) <- row.names(iv_90006) <- row.names(iv_90010) <- row.names(iv_90011) <- row.names(iv_90028) <- NULL
iv_bind <- rbind(iv_90004, iv_90006, iv_90010, iv_90011, iv_90028, iv_90001)

iv_comp <- xtabs(importance.value ~ plot + species, data=iv_bind) %>% 
  as.data.frame.matrix() %>% 
  disttransform(method = "hellinger")

Ordination.model1 <- rda(iv_comp, data = iv_comp, scaling = "species")
plot1 <- ordiplot(Ordination.model1, scaling = 1)

sites.long1 <- sites.long(plot1)
species.long1 <- species.long(plot1)
axis.long1 <- axis.long(Ordination.model1, choices=c(1, 2))

species.long1$radius <- sqrt(species.long1$axis1^2 + species.long1$axis2^2)
species.threshold <- quantile(species.long1$radius, probs=0.80)
species.long2 <- species.long1[species.long1$radius >= species.threshold, ]
species.long2

BioR.theme <- theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

plotgg1 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab(axis.long1[1, "label"]) +
  ylab(axis.long1[2, "label"]) +  
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_point(data=sites.long1, 
             aes(x=axis1, y=axis2), 
             size=2, colour="blue") +
  geom_text_repel(data=sites.long1, 
                  aes(x=axis1, y=axis2, label=labels), 
                  size=4, colour="blue") +
  geom_segment(data=species.long2, 
               aes(x=0, y=0, xend=axis1*1.8, yend=axis2*1.8), 
               colour="red", alpha=0.7, size=0.5, 
               arrow=arrow(angle=7,
                           length=unit(1, "cm"),
                           type="open")) +
  geom_text(data=species.long2, 
            aes(x=axis1*2.0, y=axis2*2.0, label=labels),
            colour="red") +
  BioR.theme +
  ggsci::scale_colour_npg() +
  coord_fixed(ratio=1)

ggsave("r/output/pca-ivi.jpeg", plot = plotgg1, width = 10, height = 6, dpi = 300)
