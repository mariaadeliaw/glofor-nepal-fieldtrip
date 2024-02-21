library(ggplot2)

# combine distance and simpson table
cost_diversity <- dest.loc.w.cost_df_comb_avg %>%
  left_join(diversity_stat_filt, by = "PlotNo")
cost_diversity <- cost_diversity[-7,]

# linear regression
lm_psp <- lm(formula = average_cost ~ difference, data = cost_diversity)
summary(lm_psp)

# normality test
shapiro.test(residuals(lm_psp))

# pearson correlation test
cor(cost_diversity$average_cost, cost_diversity$difference)

# result plot
ggplot(cost_diversity, aes(x = average_cost, y = difference)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "average_cost", y = "Difference", title = "Linear Regression") +
  theme_minimal()


# shanon ------------------------------------------------------------------

cost_diversity_shanon <- dest.loc.w.cost_df_comb_avg %>%
  left_join(diversity_stat_filt_shanon, by = "PlotNo")


lm_psp_shanon <- lm(formula = average_cost ~ difference, data = cost_diversity)
summary(lm_psp_shanon)


shapiro.test(residuals(lm_psp_shanon))

ggplot(cost_diversity, aes(x = average_cost, y = difference)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "average_cost", y = "Difference", title = "Linear Regression") +
  theme_minimal()


# save rds ----------------------------------------------------------------
saveRDS(cost_diversity, file = "r/rds/cost_diversity.rds")
saveRDS(cost_diversity_shanon, file = "r/rds/cost_diversity_shanon.rds")
saveRDS(lm_psp, file = "r/rds/lm_psp.rds")
saveRDS(lm_psp_shanon, file = "r/rds/lm_psp_shanon.rds")

