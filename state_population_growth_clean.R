# 1. Import your original dataset
data <- read.csv(
  "Population Change by State Level.csv",
  colClasses = c(SUMLEV = "character", STATE = "character")
)

# 2. Keep only the 50 states
states <- subset(
  data,
  SUMLEV == "040" & !(STATE %in% c("11", "72"))
)

# 3. Keep the columns you need
states <- states[, c(
  "STATE", "NAME",
  "POPESTIMATE2020",
  "POPESTIMATE2025"
)]

# 4. Calculate population growth (%)
states$population_growth_pct <- (
  (states$POPESTIMATE2025 -
     states$POPESTIMATE2020) /
    states$POPESTIMATE2020
) * 100

# 5. Round percentages to 2 decimal places
states$population_growth_pct <- round(
  states$population_growth_pct, 2
)

# 6. Sort from highest to lowest growth
states <- states[
  order(-states$population_growth_pct),
]

# 7. Export cleaned dataset
write.csv(
  states,
  "state_population_growth_clean.csv",
  row.names = FALSE
)

# Arrange states alphabetically
state_population_growth_clean <-
  state_population_growth_clean[
    order(state_population_growth_clean$NAME), ]

# Renumbers states from 01 to 50
state_population_growth_clean$STATE <-
  sprintf("%02d", 1:nrow(state_population_growth_clean))

# View your updated table
View(state_population_growth_clean)



