library(ggplot2)

# combine distance and simpson table
cost_diversity <- dest.loc.w.cost_df_comb %>%
  left_join(diversity_stat_filt, by = "PlotNo")
cost_diversity <- cost_diversity[-7,]

lm_psp <- lm(formula = average_cost ~ difference, data = cost_diversity)
summary(lm_psp)

shapiro.test(residuals(lm_psp))

ggplot(cost_diversity, aes(x = average_cost, y = difference)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "average_cost", y = "Difference", title = "Linear Regression") +
  theme_minimal()


# shanon ------------------------------------------------------------------

cost_diversity_shanon <- dest.loc.w.cost_df_comb %>%
  left_join(diversity_stat_filt_shanon, by = "PlotNo")
lm_psp_shanon <- lm(formula = average_cost ~ difference, data = cost_diversity)
summary(lm_psp_shanon)


shapiro.test(residuals(lm_psp_shanon))

ggplot(cost_diversity, aes(x = average_cost, y = difference)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "average_cost", y = "Difference", title = "Linear Regression") +
  theme_minimal()
