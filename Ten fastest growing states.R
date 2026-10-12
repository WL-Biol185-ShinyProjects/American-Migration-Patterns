# Sort from highest to lowest growth
ranked <- combined_data[order(-combined_data$population_growth_pct), ]

# Select top 10
top10 <- head(ranked, 10)

# Increase left margin so full state names fit
par(mar = c(2, 14, 4, 2))   # bottom, left, top, right

barplot(
  rev(top10$population_growth_pct),
  names.arg = rev(top10$NAME),
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  xlab = "Population Growth (%)",
  main = "10 Fastest-Growing States (2020-2025)",
  cex.names = 1.2
)
