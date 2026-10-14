
# 1. Import the original housing dataset
housing_raw <- read.csv(
  "Housing Costs as a % of Household Income in the Past 12 Months.csv",
  check.names = FALSE
)

# 2. Identify the state estimate columns
estimate_cols <- grep(
  "!!Estimate$", names(housing_raw), value = TRUE
)

state_names <- sub("!!Estimate$", "", estimate_cols)

# Exclude DC and Puerto Rico
state_names <- setdiff(
  state_names, c("District of Columbia", "Puerto Rico")
)

# 3. Convert counts from text to numbers
as_num <- function(x) {
  as.numeric(gsub(",", "", x))
}

# 4. Calculate housing cost burden for each state
housing_clean <- do.call(rbind, lapply(state_names, function(st) {
  
  v <- as_num(housing_raw[[paste0(st, "!!Estimate")]])
  
  total <- v[1]
  
  # Three Over 30 Percent rows
  burdened <- sum(v[c(3, 7, 11)])
  
  # Three Not Calculated rows
  not_calculated <- sum(v[c(5, 9, 13)])
  
  calculable <- total - not_calculated
  
  data.frame(
    NAME = st,
    housing_cost_burden_pct_2024 =
      round(100 * burdened / calculable, 2)
  )
}))

# 5. Sort alphabetically
housing_clean <- housing_clean[
  order(housing_clean$NAME), ]

# 6. Number states 01-50
housing_clean$STATE <- sprintf(
  "%02d", seq_len(nrow(housing_clean))
)

# Reorder columns
housing_clean <- housing_clean[
  c("STATE", "NAME", "housing_cost_burden_pct_2024")
]

# 7. Export cleaned data
write.csv(
  housing_clean,
  "housing_cost_burden_clean.csv",
  row.names = FALSE
)

View(housing_clean)
