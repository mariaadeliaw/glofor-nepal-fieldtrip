library(ggplot2)

# Data
regeneration_density <- data.frame(
  Category = c("Easy", "Hard"),
  Density = c(22350, 46900)
)


# Calculate percentages
regeneration_density$Percent <- round(regeneration_density$Density / sum(regeneration_density$Density) * 100, 1)

# Create column chart with labels
reg_dens_plot <- ggplot(regeneration_density, aes(x = Category, y = Density, fill = Category)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +  # coord_flip() +  # Flip the coordinates to make it a column chart
  labs(title = "Regeneration Density per Ha") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  scale_fill_manual(values = c("Easy" = "skyblue", "Hard" = "lightcoral")) +  # Customizing colors
  geom_text(aes(label = Density), vjust = -0.5, size = 5) + # Adjust label position and font size
  ggpubr::stat_compare_means(method = "t.test", comparisons = list(c("Easy", "Hard")))


ggsave("r/output/regeneration_density.jpeg", plot = reg_dens_plot, width = 10, height = 6, dpi = 300)

# Data
data <- data.frame(
  Accessibility = c("Easy", "Hard"),
  Ratio = c("21%", "6%"),
  Average_Stumps_per_Plot = c(2.25, 0)
  # % lopped trees / disturbance ratio
)

data %>% 
  janitor::clean_names(case = "title") %>% 
  gt() %>% 
  gtsave("r/output/dist_ratio.png", expand = 10)
  

# Create pie chart
lop_plot <- ggplot(data, aes(x = Area, y = Ratio, fill = Area)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  labs(title = "% Lopped Trees / Disturbance Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),  # Set font size
        axis.title = element_text(size = 16), 
        legend.position = "none") +  # Remove legend
  geom_text(aes(label = paste0(Ratio, "%")), vjust = -0.5, size = 6)  # Adjust label position and font size

ggsave("r/output/lop_plot.jpeg", plot = lop_plot, width = 10, height = 6, dpi = 300)



library(gt)
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"
access_group <- psp_data %>% 
  read_excel(sheet = "access_group") %>% 
  select(PlotNo, Accessibility) %>% 
  gt() %>% 
  gtsave("r/output/access_group.png")
psp_data_reg <- psp_data %>% 
  read_excel(sheet = "regeneration_data_2024") %>% 
  filter(PlotNo %in% plot_numbers)
  

reg_2024 <- psp_data %>% 
  read_excel(sheet = "regeneration_data_2024") %>% 
  select(species, DBH, PlotNo, Accessibility) %>% 
  count(species, Accessibility) %>% 
  arrange(desc(n))


reg_2024 %>% 
  janitor::clean_names(case = "title") %>%
  slice_max(N, n = 20) %>% 
  gt() %>% 
  gtsave("r/output/reg_spec.png", expand = 10)

ggplot(reg_2024, aes(x = species, y = n, fill = Accessibility)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count per Species per Accessibility",
       x = "Species",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
