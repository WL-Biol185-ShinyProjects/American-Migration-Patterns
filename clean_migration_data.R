
library(tidyverse)

# Import both original CSV files
inflow <- read_csv(
  "stateinflow2223.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

outflow <- read_csv(
  "stateoutflow2223.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

# Look at the original datasets
View(inflow)
View(outflow)

# Check their structure
glimpse(inflow)
glimpse(outflow)




# Identify valid state and DC FIPS codes
state_codes <- sort(unique(inflow$y2_statefips))

# Build a lookup using real state-to-state records
state_lookup <- bind_rows(
  inflow %>%
    filter(
      y1_statefips %in% state_codes,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y1_statefips,
      state_name = y1_state_name
    ),
  outflow %>%
    filter(
      y2_statefips %in% state_codes,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y2_statefips,
      state_name = y2_state_name
    )
) %>%
  distinct()

# Check that there are 51 unique states/DC
nrow(state_lookup)

View(state_lookup)


inflow_clean <- inflow %>%
  filter(
    y1_statefips %in% state_codes,
    y2_statefips %in% state_codes,
    y1_statefips != y2_statefips
  ) %>%
  transmute(
    year = "2022-2023",
    origin_fips = y1_statefips,
    destination_fips = y2_statefips,
    tax_returns = n1,
    exemptions = n2,
    agi_thousands = AGI
  ) %>%
  left_join(
    state_lookup %>%
      rename(
        origin_fips = state_fips,
        origin_state = state_name
      ),
    by = "origin_fips"
  ) %>%
  left_join(
    state_lookup %>%
      rename(
        destination_fips = state_fips,
        destination_state = state_name
      ),
    by = "destination_fips"
  ) %>%
  select(
    year, origin_state, destination_state,
    origin_fips, destination_fips,
    tax_returns, exemptions, agi_thousands
  )

View(inflow_clean)


outflow_clean <- outflow %>%
  filter(
    y1_statefips %in% state_codes,
    y2_statefips %in% state_codes,
    y1_statefips != y2_statefips
  ) %>%
  transmute(
    year = "2022-2023",
    origin_fips = y1_statefips,
    destination_fips = y2_statefips,
    tax_returns = n1,
    exemptions = n2,
    agi_thousands = AGI
  ) %>%
  left_join(
    state_lookup %>%
      rename(
        origin_fips = state_fips,
        origin_state = state_name
      ),
    by = "origin_fips"
  ) %>%
  left_join(
    state_lookup %>%
      rename(
        destination_fips = state_fips,
        destination_state = state_name
      ),
    by = "destination_fips"
  ) %>%
  select(
    year, origin_state, destination_state,
    origin_fips, destination_fips,
    tax_returns, exemptions, agi_thousands
  )

View(outflow_clean)

#Checking if data is clean


write_csv(
  inflow_clean,
  "state_inflow_clean_2223.csv"
)

write_csv(
  outflow_clean,
  "state_outflow_clean_2223.csv"
)


