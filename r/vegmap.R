psp_data <- "data/Gyachhowk2016-2019extractForGLOFOR-EDIT.xlsx"
vegmap_90001_2019 <- psp_data %>% 
  read_excel(sheet = "90001_vegmap") %>% 
  select(species, x, y)

# Filter out rows with 0s
filtered_data <- vegmap_90001_2019 %>% 
  na.omit()

# Create a ggplot object
p <- ggplot(filtered_data, aes(x = x, y = y, color = species)) +
  geom_point() + 
  labs(x = "X-axis Label", y = "Y-axis Label", title = "90001 in 2019") +
  theme_minimal()

# Move legend outside the plot
p + theme(legend.position = "right")