combined_data <- merge(
  state_population_growth_clean,
  housing_clean[, c("NAME", "housing_cost_burden_pct_2024")],
  by = "NAME"
)

View(combined_data)
nrow(combined_data)  # Should equal 50


plot(
  combined_data$housing_cost_burden_pct_2024,
  combined_data$population_growth_pct,
  xlab = "Housing Cost Burden (%) in 2024",
  ylab = "Population Growth (%) from 2020 to 2025",
  main = "Housing Affordability vs. Population Growth",
  pch = 19
)

cor.test(
  combined_data$housing_cost_burden_pct_2024,
  combined_data$population_growth_pct
)

model <- lm(
  population_growth_pct ~ housing_cost_burden_pct_2024,
  data = combined_data
)

abline(model, col = "red", lwd = 2)
