
psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"

# Load required libraries
library(dplyr)
library(ggplot2)
library(readxl)

# Assuming your table is named 'tree_data', adjust if needed
# tree_data should have columns 'species' and 'DBH'

# Define species of interest
# species_of_interest <- c("Schima wallichii", 
#                          "Alnus nepalensis", 
#                          "Shorea robusta")
plot_numbers <- c(90001, 90004, 90006, 90010, 90011, 90028)


# Function to calculate summary statistics
calculate_summary <- function(data) {
  data %>%
    summarise(mean_DBH = mean(DBH),
              sd_DBH = sd(DBH))
}

# Define function to categorize DBH values into specified ranges
categorize_DBH <- function(DBH) {
  cut(DBH, breaks = seq(9.9, 120, by = 10), labels = paste0(seq(10, 110, by = 10), "-", seq(20, 120, by = 10)))
}


# Get PSP data of each year
psp_data_2016 <- psp_data %>% 
  read_excel(sheet = "data_2016") %>% 
  filter(PlotNo %in% plot_numbers) %>%
  group_by(PlotNo) %>%
  tidyr::drop_na(DBH) %>%
  filter(DBH >= 10) %>%
  mutate(DBH_Category = categorize_DBH(DBH))
psp_data_2019 <- psp_data %>% 
  read_excel(sheet = "data_2019") %>% 
  filter(PlotNo %in% plot_numbers) %>%
  group_by(PlotNo) %>%
  tidyr::drop_na(DBH) %>%
  filter(DBH >= 10) %>%
  mutate(DBH_Category = categorize_DBH(DBH))
psp_data_2024 <- psp_data %>% 
  read_excel(sheet = "data_2024") %>% 
  filter(PlotNo %in% plot_numbers) %>%
  group_by(PlotNo) %>%
  tidyr::drop_na(DBH) %>%
  filter(DBH >= 10) %>%
  mutate(DBH_Category = categorize_DBH(DBH))

# Convert PlotNo to factor
psp_data_2016$PlotNo <- factor(psp_data_2016$PlotNo)
psp_data_2019$PlotNo <- factor(psp_data_2019$PlotNo)
psp_data_2024$PlotNo <- factor(psp_data_2024$PlotNo)
# Calculate summary statistics for each time series
sum_2016 <- calculate_summary(psp_data_2016)
sum_2019 <- calculate_summary(psp_data_2019)
sum_2024 <- calculate_summary(psp_data_2024)

# Combine summary data
combined_summary <- bind_rows(
  mutate(sum_2016, time_series = "2016"),
  mutate(sum_2019, time_series = "2019"),
  mutate(sum_2024, time_series = "2024")
)

# Combine data
combined_data <- bind_rows(
  mutate(psp_data_2016, time_series = "2016"),
  mutate(psp_data_2019, time_series = "2019"),
  mutate(psp_data_2024, time_series = "2024")
) %>% 
  mutate(BA = ((DBH)/2)^2 * 3.14)

summary_data_ba <- combined_data %>%
  group_by(time_series) %>%
  mutate(total_ba = sum(BA) / 1000) %>% 
  select(time_series, total_ba) %>%
  # mutate(total_ba = total_ba / 3000 /10000 ) %>% 
  distinct()

# Plotting the distribution of DBH for each species as a grouped box plot with facets for each time series
plot <- ggplot(combined_data, aes(x = DBH_Category, fill = time_series)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Accessibility, nrow = 1) +  # Grouping based on PlotNo
  labs(title = "DBH Distribution by Plot and Year",
       x = "DBH Category", y = "Count") +
  theme_minimal()
plot
# # # Plotting the distribution of DBH for each species as a grouped box plot with facets for each time series
# plot <- ggplot(combined_summary, aes(x = time_series, y = mean_DBH, fill = PlotNo)) +
#   geom_boxplot(width = 0.6, position = position_dodge(width = 0.8)) +
#   geom_errorbar(aes(ymin = mean_DBH - sd_DBH, ymax = mean_DBH + sd_DBH), width = 0.2, position = position_dodge(width = 0.8)) +
#   facet_wrap(~PlotNo) +
#   labs(title = "Comparison of Species Distribution Patterns Across Time Series",
#        x = "Time Series", y = "Average DBH (cm)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Display the plot
# print(plot)

# Function to create DBH distribution plot for each year and DBH class
create_DBH_distribution_plot <- function(data, year) {
  ggplot(data, aes(x = DBH_Category, fill = DBH_Category)) +
    geom_bar() +
    labs(title = paste("DBH Distribution for", year),
         x = "DBH Category", y = "Count") +
    theme_minimal()
}
plot_dbhcat <- ggplot(combined_data, aes(x = DBH_Category, y = ..count.., fill = time_series)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Accessibility) +
  labs(title = "DBH Distribution",
       x = "DBH Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12))  # Adjust y-axis text size

# ggsave("r/output/plot_dbhcat.jpeg", plot = plot_dbhcat, width = 10, height = 6, dpi = 300)


# Create DBH distribution plots for each year and DBH class
plot_2016 <- create_DBH_distribution_plot(psp_data_2016, "2016")
plot_2019 <- create_DBH_distribution_plot(psp_data_2019, "2019")
plot_2024 <- create_DBH_distribution_plot(psp_data_2024, "2024")

#combine plot
library(ggpubr)

comb_plot_dbh_figure <- ggarrange(plot_2016, plot_2019, plot_2024,
                                  ncol = 3, nrow = 1)
# Display the plots
print(plot_2016)
print(plot_2019)
print(plot_2024)

# basal area

basal_plot <- ggplot(summary_data_ba, aes(x = time_series, y = total_ba, fill = time_series)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Basal Area Comparison",
       x = "Plot",
       y = "Basal Area (m^2)",
       fill = "Time Series") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.y = element_text(size = 14))

ggsave("r/output/basal_plot.jpeg", plot = basal_plot, width = 10, height = 6, dpi = 300)


# Convert 'time_series' to numeric if it's character
summary_data_ba$time_series <- as.numeric(summary_data_ba$time_series)

# Fit linear regression to obtain trend line
trend_data <- lm(total_ba ~ time_series, data = summary_data_ba)
trend_df <- data.frame(
  time_series = summary_data_ba$time_series,
  total_ba = predict(trend_data)
)
# Calculate R-squared
r_squared <- summary(trend_data)$r.squared

# Perform ANOVA
anova_result <- aov(total_ba ~ time_series, data = summary_data_ba)

# Perform Tukey's HSD post-hoc test
tukey_result <- emmeans::emmeans(anova_result, pairwise ~ time_series)

# Line graph with trend line
basal_plot <- ggplot(summary_data_ba, aes(x = time_series, y = total_ba)) +
  geom_point() +  # Plot points
  geom_line(data = trend_df, aes(y = total_ba), color = "red") +  # Add trend line
  labs(title = "Basal Area Comparison",
       x = "Year",
       y = "Basal Area (m^2)",
       caption = paste("R-squared = ", round(r_squared, 3))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.y = element_text(size = 14))
