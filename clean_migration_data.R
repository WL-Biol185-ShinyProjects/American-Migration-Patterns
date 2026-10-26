
library(tidyverse)

# ==========================================
# CLEANING MIGRATION DATA: 2022-2023
# ==========================================


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


# ==========================================
# CLEANING MIGRATION DATA: 2021-2022
# ==========================================

# Import the 2021-2022 datasets
inflow_2122 <- read_csv(
  "stateinflow2122.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

outflow_2122 <- read_csv(
  "stateoutflow2122.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

# Use the state lookup from the 2021-2022 files
state_codes_2122 <- unique(inflow_2122$y2_statefips)

state_lookup_2122 <- bind_rows(
  inflow_2122 %>%
    filter(
      y1_statefips %in% state_codes_2122,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y1_statefips,
      state_name = y1_state_name
    ),
  outflow_2122 %>%
    filter(
      y2_statefips %in% state_codes_2122,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y2_statefips,
      state_name = y2_state_name
    )
) %>%
  distinct()

# Clean both datasets
clean_2122 <- function(data) {
  data %>%
    filter(
      y1_statefips %in% state_codes_2122,
      y2_statefips %in% state_codes_2122,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      year = "2021-2022",
      origin_fips = y1_statefips,
      destination_fips = y2_statefips,
      tax_returns = na_if(n1, -1),
      exemptions = na_if(n2, -1),
      agi_thousands = na_if(AGI, -1)
    ) %>%
    left_join(
      state_lookup_2122 %>%
        rename(
          origin_fips = state_fips,
          origin_state = state_name
        ),
      by = "origin_fips"
    ) %>%
    left_join(
      state_lookup_2122 %>%
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
}

# Clean inflow and outflow
inflow_clean_2122 <- clean_2122(inflow_2122)
outflow_clean_2122 <- clean_2122(outflow_2122)

# Save cleaned CSV files
write_csv(
  inflow_clean_2122,
  "state_inflow_clean_2122.csv"
)

write_csv(
  outflow_clean_2122,
  "state_outflow_clean_2122.csv"
)

# Check results
nrow(inflow_clean_2122)
nrow(outflow_clean_2122)

colSums(is.na(inflow_clean_2122))
colSums(is.na(outflow_clean_2122))


# ==========================================
# CLEANING MIGRATION DATA: 2020 -2021
# ==========================================


# STEP 1: Import the original CSV files

inflow_2021 <- read_csv(
  "stateinflow2021.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

outflow_2021 <- read_csv(
  "stateoutflow2021.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

# STEP 2: Identify valid state FIPS codes
state_codes_2021 <- unique(inflow_2021$y2_statefips)

# STEP 3: Create state name lookup
state_lookup_2021 <- bind_rows(
  inflow_2021 %>%
    filter(
      y1_statefips %in% state_codes_2021,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y1_statefips,
      state_name = y1_state_name
    ),
  outflow_2021 %>%
    filter(
      y2_statefips %in% state_codes_2021,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y2_statefips,
      state_name = y2_state_name
    )
) %>%
  distinct()

# STEP 4: Create cleaning function
clean_2021 <- function(data) {
  
  data %>%
    filter(
      y1_statefips %in% state_codes_2021,
      y2_statefips %in% state_codes_2021,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      year = "2020-2021",
      origin_fips = y1_statefips,
      destination_fips = y2_statefips,
      tax_returns = na_if(n1, -1),
      exemptions = na_if(n2, -1),
      agi_thousands = na_if(AGI, -1)
    ) %>%
    left_join(
      state_lookup_2021 %>%
        rename(
          origin_fips = state_fips,
          origin_state = state_name
        ),
      by = "origin_fips"
    ) %>%
    left_join(
      state_lookup_2021 %>%
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
}

# STEP 5: Clean both datasets
inflow_clean_2021 <- clean_2021(inflow_2021)
outflow_clean_2021 <- clean_2021(outflow_2021)

# STEP 6: View cleaned data
View(inflow_clean_2021)
View(outflow_clean_2021)

# STEP 7: Check results
nrow(inflow_clean_2021)
nrow(outflow_clean_2021)

colSums(is.na(inflow_clean_2021))
colSums(is.na(outflow_clean_2021))

# Check duplicate state pairs
sum(duplicated(
  inflow_clean_2021[c("origin_fips", "destination_fips")]
))

sum(duplicated(
  outflow_clean_2021[c("origin_fips", "destination_fips")]
))

# STEP 8: Save cleaned CSV files
write_csv(
  inflow_clean_2021,
  "state_inflow_clean_2021.csv"
)

write_csv(
  outflow_clean_2021,
  "state_outflow_clean_2021.csv"
)



# ==========================================
# CLEANING MIGRATION DATA: 2019-2020
# ==========================================

# STEP 1: Import the original CSV files

inflow_1920 <- read_csv(
  "stateinflow1920.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

outflow_1920 <- read_csv(
  "stateoutflow1920.csv",
  col_types = cols(
    y1_statefips = col_character(),
    y2_statefips = col_character(),
    .default = col_guess()
  )
)

# STEP 2: Identify the 50 states and DC

state_codes_1920 <- intersect(
  unique(inflow_1920$y2_statefips),
  unique(outflow_1920$y1_statefips)
)

# STEP 3: Create a state name lookup

state_lookup_1920 <- bind_rows(
  inflow_1920 %>%
    filter(
      y1_statefips %in% state_codes_1920,
      y2_statefips %in% state_codes_1920,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y1_statefips,
      state_name = y1_state_name
    ),
  outflow_1920 %>%
    filter(
      y1_statefips %in% state_codes_1920,
      y2_statefips %in% state_codes_1920,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      state_fips = y2_statefips,
      state_name = y2_state_name
    )
) %>%
  distinct()

# STEP 4: Create cleaning function

clean_1920 <- function(data) {
  
  data %>%
    filter(
      y1_statefips %in% state_codes_1920,
      y2_statefips %in% state_codes_1920,
      y1_statefips != y2_statefips
    ) %>%
    transmute(
      year = "2019-2020",
      origin_fips = y1_statefips,
      destination_fips = y2_statefips,
      tax_returns = na_if(n1, -1),
      exemptions = na_if(n2, -1),
      agi_thousands = na_if(AGI, -1)
    ) %>%
    left_join(
      state_lookup_1920 %>%
        rename(
          origin_fips = state_fips,
          origin_state = state_name
        ),
      by = "origin_fips"
    ) %>%
    left_join(
      state_lookup_1920 %>%
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
}

# STEP 5: Clean both datasets

inflow_clean_1920 <- clean_1920(inflow_1920)
outflow_clean_1920 <- clean_1920(outflow_1920)

# STEP 6: View cleaned data

View(inflow_clean_1920)
View(outflow_clean_1920)

# STEP 7: Check results

nrow(inflow_clean_1920)
nrow(outflow_clean_1920)

colSums(is.na(inflow_clean_1920))
colSums(is.na(outflow_clean_1920))

# Check duplicate state pairs

sum(duplicated(
  inflow_clean_1920[c("origin_fips", "destination_fips")]
))

sum(duplicated(
  outflow_clean_1920[c("origin_fips", "destination_fips")]
))

# STEP 8: Save cleaned CSV files

write_csv(
  inflow_clean_1920,
  "state_inflow_clean_1920.csv"
)

write_csv(
  outflow_clean_1920,
  "state_outflow_clean_1920.csv"
)

