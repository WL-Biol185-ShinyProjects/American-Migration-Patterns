# =============================================================================
# atlas_server.R  —  DATA + SERVER LOGIC
# American Migration Atlas | BIOL 1185 | Washington and Lee University
# =============================================================================
#
# WHAT THIS FILE DOES
#   1. Reads the 15 original CSV files and turns them into four clean tables.
#   2. Defines the formulas, labels, and chart helpers used by the plots.
#   3. Defines `server`, which builds every plot/table on the website.
#
# THE FOUR CLEAN TABLES (everything else is built from these)
#   state_population     50 rows   one row per state: 2020 & 2025 population
#   acs_by_state_year   300 rows   American Community Survey (ACS), 
#                                  50 states x ACS years 2019-2024
#                                  (income, rent, home value, poverty, ...)
#   irs_state_totals    200 rows   50 states x 4 IRS periods
#                                  (people moving in / out / net)
#   irs_route_flows     one row per state-to-state route per IRS period
#                       (used by the "Migration routes" tab)
#
# TABLE OF CONTENTS
#   1. Packages
#   2. Finding and reading the CSV files
#   3. Fixed lookup values (period names, metric labels, ...)
#   4. LOAD:  state population
#   5. LOAD:  ACS survey data (income, rent, poverty, ...)
#   6. LOAD:  IRS migration data (moves between states)
#   7. Dropdown menu choices and the list of source files
#   8. Small helper functions (number formatting, correlations)
#   9. Chart helpers (theme, map colors)
#  10. Text notes shown above charts
#  11. The `server` function, one block per tab:
#        A. Navigation               F. Compare states tab
#        B. Shared data              G. Trends tab
#        C. Overview tab             H. About us tab
#        D. Explore factors tab      I. Data & methods tab
#        E. Migration routes tab
#
# HOW THIS FILE CONNECTS TO atlas_ui.R
#   * atlas_ui.R reads some objects created here (period_labels, dropdown
#     choices, state_choices, source_groups, ...). So app.R loads THIS file first.
#   * Every `output$something` in the server must match a
#     `plotOutput("something")` / `uiOutput("something")` / etc. in atlas_ui.R.
#   * stat_box() is defined in atlas_ui.R but used here to draw summary boxes.
#
# IMPORTANT DEFINITION
#   IRS "exemptions" (called N2 in the IRS files) are the number of people
#   listed on tax returns. We use them as a proxy for people who moved.
#   They are NOT a Census count of every person who moved.
# =============================================================================


# ---------------------------------------------------------------------------
# 1. PACKAGES
# ---------------------------------------------------------------------------
# Install once in the RStudio console if needed:
# The Shiny package - - - creates the interactive app.
# dplyr package - - - organizes, filters, and combines data.
# ggplot2 package - - - makes charts.
# ploty package - - - adds interactive features such as hover and click information.
# DT package - - - displays searchable and sortable tables.
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly) 
library(DT)     


# ---------------------------------------------------------------------------
# 2. FINDING AND READING THE CSV FILES
# ---------------------------------------------------------------------------
# Make a list three possible places to find the CSVs: the current working
# folder and two named subfolders. Remove any duplicate locations.
# This only builds the list; it does not check or open the folders.

# getwd() - - - gets the folder R is currently working in.
# file.path() - - - builds the locations of two subfolders inside it.
# c() - - - groups those three locations together.
# unique() - - - removes any repeated locations
# data_folder_candidates - - - stores the results

data_folder_candidates <- unique(c(
  getwd(),
  file.path(getwd(), "American_Migration_Atlas_Shiny"),
  file.path(getwd(), "Atlas_Team_Photos_Ready")
))
# Check which folders contain the population CSV
folders_containing_data <- data_folder_candidates[
  file.exists(file.path(data_folder_candidates, "state_population_growth_clean.csv"))
]
# Stop if no matching folder was found
# If no folder contains the population CSV, stop and show a message
# explaining that the data could not be found.
if (length(folders_containing_data) == 0L) {
  stop("Could not locate state_population_growth_clean.csv. Keep the supplied CSVs ",
       "beside app.R or in American_Migration_Atlas_Shiny.")
}
# Choose the folder to use
# Choose the first matching folder and save its full address.
# Report an error if R cannot ressolve that location.
data_folder <- normalizePath(folders_containing_data[1], mustWork = TRUE)

# Read one CSV from the data folder and make sure every column has a unique,
# non-blank name (some ACS exports have blank or repeated headings).
read_source_csv <- function(filename) {
# Locate the requested file andf check that it exists
  csv_path <- file.path(data_folder, filename)
  if (!file.exists(csv_path)) stop("Missing CSV in your app folder: ", filename)
# Read the file into a table
  csv_data <- read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8-BOM", na.strings = c("NA", "", "N/A"))
# Identify and replace blank headings
  column_names <- names(csv_data)
  is_blank_name <- is.na(column_names) | trimws(column_names) == ""
  column_names[is_blank_name] <- paste0("unnamed_column_", which(is_blank_name))
# Fix repeated headings and return the table
  names(csv_data) <- make.unique(column_names, sep = "_")
  csv_data
}

# Turn text such as "$1,234" or "12.5%" into the number 1234 or 12.5.
# Anything that is not a number becomes NA (without a warning).
to_number <- function(text_values) {
  suppressWarnings(as.numeric(gsub("[$,% ]", "", as.character(text_values))))
}

# FIPS codes identify states ("1" and "01" both mean Alabama).
# Standardize them to two-character text so tables can be joined on them.
to_two_digit_fips <- function(fips_values) {
  sprintf("%02d", as.integer(to_number(fips_values)))
}


# ---------------------------------------------------------------------------
# 3. FIXED LOOKUP VALUES
# ---------------------------------------------------------------------------

# IRS periods. The code (e.g. "2122") is what appears in the CSV file names.
# irs_period_codes - - - stores the short codes for our four IRS reporting periods.
# irs_period_labels - - - connects each code to a readable year range
# so the app can display labels such as "2019-2020".

irs_period_codes <- c("1920", "2021", "2122", "2223")
irs_period_labels <- c("1920" = "2019–2020", "2021" = "2020–2021",
                       "2122" = "2021–2022", "2223" = "2022–2023")
# irs_period_end_year - - - connects each IRS period code to its ending year.
# For example, "1920" reprresents 22019-2020, so its ending year is 2020.
# The L tells R to store each year as a whole number.
irs_period_end_year <- c("1920" = 2020L, "2021" = 2021L,
                         "2122" = 2022L, "2223" = 2023L)

# Each state has 50 possible incoming routes and 50 outgoing routes:
# connections with the 49 other states and Washington D.C.
# Store this expected count as a whole number.
ROUTES_PER_STATE <- 50L

# metric_labels - - - connects internal measure names to readable display labels.
# Labels include units such as dollar signs, percentages, people, or minutes.
# Keeping them together helps charts, tables, and drop downs use consistent wording.
# This section stores labels; its does not calculate values.

metric_labels <- c(
  net_per_1000             = "Net migration proxy per 1,000",
  inflow_per_1000          = "Incoming migration proxy per 1,000",
  outflow_per_1000         = "Outgoing migration proxy per 1,000",
  net_exemptions           = "Net migration proxy (N2)",
  inflow_exemptions        = "Incoming migration proxy (N2)",
  outflow_exemptions       = "Outgoing migration proxy (N2)",
  population_growth_pct    = "Population growth, 2020–2025 (%)",
  acs_population           = "ACS total population (people)",
  median_household_income  = "Median household income ($)",
  median_home_value        = "Median home value ($)",
  home_income_ratio        = "Median home value / household income",
  median_rent              = "Median gross rent ($/month)",
  rent_share_of_income_pct = "ACS median gross rent / household income (%)",
  unemployment_pct         = "ACS unemployment rate (%)",
  poverty_pct              = "ACS poverty rate (%)",
  avg_commute_minutes      = "Average commute (minutes)"
)

# irs_metric_keys - - - lists the 6 measures that use IRS migration data.
# Other code can use this list to recognize IRS measures and apply.
# the appropriate reporting-period labels or data warnings.
irs_metric_keys <- c("net_per_1000", "inflow_per_1000", "outflow_per_1000",
                     "net_exemptions", "inflow_exemptions", "outflow_exemptions")

# diverging_metric_keys - - - List the measures that can show net migration losses or gains.
diverging_metric_keys <- c("net_per_1000", "net_exemptions")


# ---------------------------------------------------------------------------
# 4. LOAD: STATE POPULATION  ->  state_population
# ---------------------------------------------------------------------------
# Source: state_population_growth_clean.csv (population estimates, 2020 & 2025)

raw_population_file <- read_source_csv("state_population_growth_clean.csv")
stopifnot(all(c("STATE", "NAME", "POPESTIMATE2020", "POPESTIMATE2025",
                "population_growth_pct") %in% names(raw_population_file)))

# Keep the 50 states only (drops D.C. and Puerto Rico if present).
population_50_states <- filter(raw_population_file, NAME %in% state.name)

state_population <- data.frame(
  fips                  = to_two_digit_fips(population_50_states$STATE),
  state                 = population_50_states$NAME,
  abbr                  = state.abb[match(population_50_states$NAME, state.name)],
  population_2020       = to_number(population_50_states$POPESTIMATE2020),
  population_2025       = to_number(population_50_states$POPESTIMATE2025),
  population_growth_pct = to_number(population_50_states$population_growth_pct),
  stringsAsFactors = FALSE
)
state_population <- arrange(state_population, state)

# Safety checks: stop immediately if the data are not what we expect.
stopifnot(nrow(state_population) == 50L,
          !anyDuplicated(state_population$fips),
          !anyNA(state_population))
# The growth % in the file must match our own calculation.
recalculated_growth_pct <- 100 * (state_population$population_2025 -
                                    state_population$population_2020) /
  state_population$population_2020
stopifnot(all(abs(state_population$population_growth_pct - recalculated_growth_pct) < 0.011))


# ---------------------------------------------------------------------------
# 5. LOAD: ACS SURVEY DATA  ->  acs_by_state_year
# ---------------------------------------------------------------------------
# Source: ACS_2019_state_new.csv ... ACS_2024_state_new.csv (one file per year)
# Each file is cleaned by load_acs_year(), then the six years are stacked.

# The 2024 file may have been saved as "ACS_2024_state_new(1).csv".
acs_filename <- function(year) {
  filename <- paste0("ACS_", year, "_state_new.csv")
  if (year == 2024 && !file.exists(file.path(data_folder, filename))) {
    filename <- "ACS_2024_state_new(1).csv"
  }
  filename
}

# The ACS column headings are long, and the 2024 export words several of them
# differently. This function returns the exact heading to use for each measure.
get_acs_column_names <- function(year, columns_in_file) {
  columns <- list(
    population     = "Total Population",
    income         = paste0("Median Household Income (In ", year, " Inflation Adjusted Dollars)"),
    labor_force    = "Civilian Population in Labor Force 16 Years and Over:",
    unemployed     = "Civilian Population in Labor Force 16 Years and Over: Unemployed",
    home_value     = "Median Value",
    rent           = "Median Gross Rent",
    rent_share     = paste0("Median Gross Rent as a Percentage of Household ",
                            " Income in the Past 12 Months (Dollars)"),
    commute        = "Average Commute to Work (In Min)",
    poverty_total  = "Population for Whom Poverty Status Is Determined:",
    poverty_prefix = "Population for Whom Poverty Status Is Determined: "
  )

  if (year == 2024) {                       # the 2024 export uses different headings
    columns$labor_force    <- "Civilian Population in Labor Force 16 Years and Over"
    columns$home_value     <- "Median House Value"
    columns$rent_share     <- "Median Gross Rent as a Percentage of Household Income"
    columns$poverty_total  <- "Population for Whom Poverty Status Is Determined"
    columns$poverty_prefix <- paste0(columns$poverty_prefix,
                                     "Ratio of Income to Poverty Level ")
  }

  # R sometimes writes "Total.Population" instead of "Total Population".
  if (!columns$population %in% columns_in_file) columns$population <- "Total.Population"
  columns
}

# Read and clean ONE year of ACS data. Returns one row per state.
load_acs_year <- function(year) {
  filename <- acs_filename(year)
  raw_acs <- read_source_csv(filename)

  # Step 1: keep the 50 states.
  raw_acs$fips <- to_two_digit_fips(raw_acs$FIPS)
  raw_acs <- filter(raw_acs, fips %in% state_population$fips)
  stopifnot(nrow(raw_acs) == 50L, !anyDuplicated(raw_acs$fips))

  # Step 2: look up this year's column headings and confirm they all exist.
  acs_columns <- get_acs_column_names(year, names(raw_acs))
  poverty_columns <- paste0(acs_columns$poverty_prefix,
                            c("Under .50", ".50 to .74", ".75 to .99"))
  required_columns <- c(
    acs_columns$population, acs_columns$income, acs_columns$labor_force,
    acs_columns$home_value, acs_columns$rent_share, acs_columns$poverty_total,
    poverty_columns, acs_columns$rent, acs_columns$commute, acs_columns$unemployed
  )
  if (!all(required_columns %in% names(raw_acs))) {
    stop("An expected column is missing from ", filename)
  }

  # Step 3: pull out the raw counts we need to calculate percentages.
  # [[column_name]] selects a column whose name is stored in a variable.
  civilian_labor_force <- to_number(raw_acs[[acs_columns$labor_force]])
  unemployed_people    <- to_number(raw_acs[[acs_columns$unemployed]])
  people_with_known_poverty_status <- to_number(raw_acs[[acs_columns$poverty_total]])
  # "Below poverty" = income-to-poverty ratio under 1.00, i.e. the three lowest bands.
  people_below_poverty_line <- to_number(raw_acs[[poverty_columns[1]]]) +
    to_number(raw_acs[[poverty_columns[2]]]) +
    to_number(raw_acs[[poverty_columns[3]]])

  # Step 4: build the clean table for this year.
  data.frame(
    fips                     = raw_acs$fips,
    acs_year                 = as.integer(year),
    acs_population           = to_number(raw_acs[[acs_columns$population]]),
    median_household_income  = to_number(raw_acs[[acs_columns$income]]),
    median_home_value        = to_number(raw_acs[[acs_columns$home_value]]),
    median_rent              = to_number(raw_acs[[acs_columns$rent]]),
    rent_share_of_income_pct = to_number(raw_acs[[acs_columns$rent_share]]),
    # unemployment % = unemployed people / civilian labor force x 100
    unemployment_pct = ifelse(is.finite(civilian_labor_force) & civilian_labor_force > 0,
                              100 * unemployed_people / civilian_labor_force, NA_real_),
    # poverty % = people below poverty line / people with known poverty status x 100
    poverty_pct = ifelse(is.finite(people_with_known_poverty_status) &
                           people_with_known_poverty_status > 0,
                         100 * people_below_poverty_line / people_with_known_poverty_status,
                         NA_real_),
    avg_commute_minutes      = to_number(raw_acs[[acs_columns$commute]])
  )
}

# Load 2019, 2020, ..., 2024 and stack them: 6 years x 50 states = 300 rows.
acs_by_state_year <- bind_rows(lapply(2019:2024, load_acs_year))
stopifnot(nrow(acs_by_state_year) == 300L)


# ---------------------------------------------------------------------------
# 6. LOAD: IRS MIGRATION DATA  ->  irs_route_flows and irs_state_totals
# ---------------------------------------------------------------------------
# Sources: state_inflow_clean_<period>.csv and state_outflow_clean_<period>.csv
#
# The inflow and outflow files describe the SAME routes from two points of view
# (e.g. "Virginia -> South Carolina" appears in both). So we must NOT add the two
# files together. We use the inflow file as the master list of routes and only
# check that the outflow file agrees with it.

# Step 1 of 2: read the routes for one IRS period (one row per origin -> destination).
read_irs_route_flows <- function(period_code) {
  inflow_file  <- read_source_csv(paste0("state_inflow_clean_", period_code, ".csv"))
  outflow_file <- read_source_csv(paste0("state_outflow_clean_", period_code, ".csv"))

  required_columns <- c("year", "origin_state", "destination_state", "origin_fips",
                        "destination_fips", "tax_returns", "exemptions", "agi_thousands")
  stopifnot(all(required_columns %in% names(inflow_file)),
            all(required_columns %in% names(outflow_file)))

  # Each origin -> destination pair may appear only once per file.
  for (irs_file in list(inflow_file, outflow_file)) {
    if (anyDuplicated(paste(irs_file$origin_fips, irs_file$destination_fips)))
      stop("Duplicate origin-destination pairs in IRS file")
  }

  # Check that both files list the same routes with the same counts.
  inflow_route_keys  <- paste(to_two_digit_fips(inflow_file$origin_fips),
                              to_two_digit_fips(inflow_file$destination_fips))
  outflow_route_keys <- paste(to_two_digit_fips(outflow_file$origin_fips),
                              to_two_digit_fips(outflow_file$destination_fips))
  if (!setequal(inflow_route_keys, outflow_route_keys))
    stop("Inflow and outflow route pairs differ in ", period_code)

  outflow_file_same_order <- outflow_file[match(inflow_route_keys, outflow_route_keys), ,
                                          drop = FALSE]
  for (count_column in c("tax_returns", "exemptions")) {
    inflow_counts  <- to_number(inflow_file[[count_column]])
    outflow_counts <- to_number(outflow_file_same_order[[count_column]])
    if (!isTRUE(all.equal(inflow_counts, outflow_counts, check.attributes = FALSE)))
      stop("IRS inflow/outflow counts disagree for ", count_column, " in ", period_code)
  }

  # Build the master route table from the inflow file.
  # (A few routes differ by $1,000 of AGI between files; we keep the inflow value.)
  data.frame(
    irs_period        = period_code,
    period_label      = irs_period_labels[[period_code]],
    origin_state      = inflow_file$origin_state,
    destination_state = inflow_file$destination_state,
    origin_fips       = to_two_digit_fips(inflow_file$origin_fips),
    destination_fips  = to_two_digit_fips(inflow_file$destination_fips),
    tax_returns       = to_number(inflow_file$tax_returns),   # N1
    exemptions        = to_number(inflow_file$exemptions),    # N2 = people (proxy)
    agi_thousands     = to_number(inflow_file$agi_thousands), # adjusted gross income
    stringsAsFactors  = FALSE
  )
}

# Step 2 of 2: add up one period's routes into one row per STATE.
summarize_irs_state_totals <- function(route_flows, period_code) {
  # The routes involve 50 states + D.C. (D.C. is a valid partner, but is not
  # shown as its own state on the map).
  stopifnot(length(union(route_flows$origin_fips, route_flows$destination_fips)) == 51L)

  # Total people moving INTO each state (group the routes by destination).
  # missing_inflow_routes = routes with no reported count (suppressed or absent).
  incoming_totals <- route_flows %>%
    group_by(destination_fips) %>%
    summarize(
      inflow_tax_returns    = sum(tax_returns, na.rm = TRUE),
      inflow_exemptions     = sum(exemptions, na.rm = TRUE),
      missing_inflow_routes = ROUTES_PER_STATE - sum(is.finite(exemptions))
    )

  # Total people moving OUT OF each state (group the routes by origin).
  outgoing_totals <- route_flows %>%
    group_by(origin_fips) %>%
    summarize(
      outflow_tax_returns    = sum(tax_returns, na.rm = TRUE),
      outflow_exemptions     = sum(exemptions, na.rm = TRUE),
      missing_outflow_routes = ROUTES_PER_STATE - sum(is.finite(exemptions))
    )

  # Attach both to the 50 states. A state that never appears has no reported
  # routes, so it gets zero totals and "all 50 routes missing".
  state_totals <- state_population %>%
    select(fips, population_2020) %>%
    left_join(incoming_totals, by = c("fips" = "destination_fips")) %>%
    left_join(outgoing_totals, by = c("fips" = "origin_fips")) %>%
    mutate(
      inflow_tax_returns     = coalesce(inflow_tax_returns, 0),
      outflow_tax_returns    = coalesce(outflow_tax_returns, 0),
      inflow_exemptions      = coalesce(inflow_exemptions, 0),
      outflow_exemptions     = coalesce(outflow_exemptions, 0),
      missing_inflow_routes  = coalesce(missing_inflow_routes, ROUTES_PER_STATE),
      missing_outflow_routes = coalesce(missing_outflow_routes, ROUTES_PER_STATE),
      irs_period             = period_code
    )

  # Calculate the headline measures.
  #   net = people in - people out
  #   "per 1,000" = that number / the state's 2020 population x 1,000
  state_totals %>%
    mutate(
      net_exemptions        = inflow_exemptions - outflow_exemptions,
      net_per_1000          = 1000 * net_exemptions / population_2020,
      inflow_per_1000       = 1000 * inflow_exemptions / population_2020,
      outflow_per_1000      = 1000 * outflow_exemptions / population_2020,
      has_missing_routes    = (missing_inflow_routes + missing_outflow_routes) > 0L
    ) %>%
    select(fips, irs_period, inflow_tax_returns, outflow_tax_returns,
           inflow_exemptions, outflow_exemptions, net_exemptions, net_per_1000,
           inflow_per_1000, outflow_per_1000, missing_inflow_routes,
           missing_outflow_routes, has_missing_routes) %>%
    as.data.frame()
}

# Run both steps for all four periods.
route_flows_by_period <- lapply(stats::setNames(irs_period_codes, irs_period_codes),
                                read_irs_route_flows)
irs_route_flows <- bind_rows(route_flows_by_period)

irs_state_totals <- bind_rows(lapply(irs_period_codes, function(period_code) {
  summarize_irs_state_totals(route_flows_by_period[[period_code]], period_code)
}))
stopifnot(nrow(irs_state_totals) == 200L)   # 50 states x 4 periods


# ---------------------------------------------------------------------------
# 7. DROPDOWN MENU CHOICES AND SOURCE FILES
# ---------------------------------------------------------------------------
# A "choices" vector shows the LABEL to people and passes the KEY to the code.

make_choices <- function(metric_keys) {
  stats::setNames(metric_keys, unname(metric_labels[metric_keys]))
}

# Explore tab: x-axis (the "factor") and y-axis (the migration "outcome").
x_axis_choices <- make_choices(c(
  "median_household_income", "median_home_value", "home_income_ratio",
  "median_rent", "rent_share_of_income_pct", "unemployment_pct", "poverty_pct",
  "avg_commute_minutes"
))
y_axis_choices <- make_choices(c(
  "net_per_1000", "inflow_per_1000", "outflow_per_1000", "net_exemptions",
  "population_growth_pct"
))

# Overview tab: what the map can color states by.
map_choices <- make_choices(c(
  "net_per_1000", "net_exemptions", "inflow_per_1000", "outflow_per_1000",
  "population_growth_pct", "median_household_income", "median_home_value",
  "unemployment_pct", "poverty_pct", "rent_share_of_income_pct"
))

# Trends tab: three special population options, then ordinary measures.
trend_choices <- c(
  "Population change: ACS 2019–2024 (2020 baseline)" = "pop_index",
  "Total population: ACS 2019–2024"                  = "acs_population",
  "Population change: 2020–2025 endpoints"           = "pop_endpoints",
  make_choices(c("net_exemptions", "net_per_1000", "inflow_exemptions",
                 "outflow_exemptions", "median_household_income", "median_home_value",
                 "median_rent", "rent_share_of_income_pct", "unemployment_pct",
                 "poverty_pct", "avg_commute_minutes"))
)

# Rows of the "Compare states" table, in display order.
comparison_metric_keys <- c(
  "net_per_1000", "inflow_per_1000", "outflow_per_1000", "net_exemptions",
  "inflow_exemptions", "outflow_exemptions", "population_growth_pct",
  "median_household_income", "median_home_value", "median_rent",
  "rent_share_of_income_pct", "unemployment_pct", "poverty_pct", "avg_commute_minutes"
)

# State dropdown: shows "Virginia", passes "VA".
state_choices <- stats::setNames(state_population$abbr, state_population$state)

# The original CSV files, grouped for the download links on "Data & methods".
source_groups <- list(
  "Population estimates" = "state_population_growth_clean.csv",
  "ACS surveys"          = vapply(2019:2024, acs_filename, character(1)),
  "IRS inflows"          = paste0("state_inflow_clean_", names(irs_period_labels), ".csv"),
  "IRS outflows"         = paste0("state_outflow_clean_", names(irs_period_labels), ".csv")
)
source_files <- unname(unlist(source_groups))
# Each download link needs its own id, e.g. "source_csv_3".
csv_download_id <- function(filename) paste0("source_csv_", match(filename, source_files))

# The websites where the original data were obtained.
# Shown in the "Data sources" card on the Data & methods tab (see atlas_ui.R).
# To add or change a source, edit this list; the page updates automatically.
data_source_websites <- list(
  list(name = "American Community Survey (ACS) 5-year estimates",
       url  = "https://app.socialexplorer.com/my-home"),
  list(name = "IRS Migration Data",
       url  = "https://www.irs.gov/statistics/soi-tax-stats-migration-data"),
  list(name = "U.S. Census Bureau, State Population Totals and Components of Change: 2020-2025",
       url  = "https://www2.census.gov/programs-surveys/popest/tables/2020-2025/state/totals/NST-EST2025-CHG.xlsx")
)


# ---------------------------------------------------------------------------
# 8. SMALL HELPER FUNCTIONS
# ---------------------------------------------------------------------------

# 1234.5 -> "1,235"    (NA -> "N/A")
format_number <- function(x, digits = 0) {
  ifelse(is.na(x), "N/A",
         formatC(x, format = "f", digits = digits, big.mark = ","))
}

# Same, but with a "+" in front of positive numbers: 12 -> "+12"
format_signed <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", paste0(ifelse(x > 0, "+", ""), format_number(x, digits)))
}

# Format ONE value the way its measure should be displayed ($, %, +/-, ...).
format_metric_value <- function(value, metric_key) {
  if (is.na(value)) return("N/A")
  if (metric_key %in% c("median_household_income", "median_home_value", "median_rent"))
    return(paste0("$", format_number(value)))
  if (metric_key %in% c("population_growth_pct", "rent_share_of_income_pct",
                        "unemployment_pct", "poverty_pct"))
    return(paste0(format_number(value, 2), "%"))
  if (metric_key %in% c("net_exemptions", "inflow_exemptions", "outflow_exemptions"))
    return(format_signed(value, 0))
  format_signed(value, 2)
}

# Return one measure as a plain number vector, one value per row of `data`.
# "home_income_ratio" is calculated here; every other measure is a column.
# Infinite / not-a-number values are turned into NA.
get_metric_values <- function(data, metric_key) {
  if (metric_key == "home_income_ratio") {
    values <- data$median_home_value / data$median_household_income
  } else {
    values <- data[[metric_key]]
  }
  values[!is.finite(values)] <- NA_real_
  values
}

# Pearson correlation of x and y, or NA if there are too few states
# or one of the two measures does not vary.
safe_correlation <- function(x_values, y_values, minimum_states = 4) {
  both_present <- is.finite(x_values) & is.finite(y_values)
  if (sum(both_present) >= minimum_states &&
      sd(x_values[both_present]) > 0 && sd(y_values[both_present]) > 0) {
    cor(x_values[both_present], y_values[both_present])
  } else {
    NA_real_
  }
}


# ---------------------------------------------------------------------------
# 9. CHART HELPERS
# ---------------------------------------------------------------------------

# Shared look for every ggplot chart. Change colors/fonts here, not in the plots.
atlas_chart_theme <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      text = element_text(color = "#182c3d"),
      axis.text = element_text(color = "#526372"),
      axis.title = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e4e9ed", linewidth = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption = element_text(color = "#526372", hjust = 0),
      legend.position = "bottom"
    )
}

# Short color-bar titles for the map.
map_legend_titles <- c(
  net_per_1000 = "Net per 1k", population_growth_pct = "Growth (%)",
  net_exemptions = "Net N2", inflow_per_1000 = "In per 1k",
  outflow_per_1000 = "Out per 1k", median_home_value = "Home ($)",
  poverty_pct = "Poverty (%)", rent_share_of_income_pct = "Rent (%)",
  median_household_income = "Income ($)", unemployment_pct = "Unemployment (%)"
)

# Decide the map's color range and colors.
#   * Net measures (can be + or -): orange (loss) - white (0) - blue (gain),
#     with zero in the middle.
#   * Everything else: light blue (low) to dark blue (high).
build_map_color_scale <- function(values, is_diverging) {
  if (is_diverging) {
    largest_size <- max(abs(values), na.rm = TRUE)
    if (!is.finite(largest_size) || largest_size <= 0) largest_size <- 1
    list(min = -largest_size, max = largest_size,
         colors = list(c(0, "#b46228"), c(0.5, "#f0f4f8"), c(1, "#2459aa")))
  } else {
    finite_values <- values[is.finite(values)]
    lowest  <- if (length(finite_values)) min(finite_values) else 0
    highest <- if (length(finite_values)) max(finite_values) else 1
    if (lowest == highest) highest <- lowest + 1
    list(min = lowest, max = highest,
         colors = list(c(0, "#e6f0ff"), c(0.25, "#b8d1fa"), c(0.5, "#83b0f1"),
                       c(0.75, "#5287d7"), c(1, "#28559f")))
  }
}


# ---------------------------------------------------------------------------
# 10. TEXT NOTES SHOWN ABOVE CHARTS
# ---------------------------------------------------------------------------
# Each chart is preceded by a note saying which time period its data cover.

irs_definition_text <- paste(
  "One IRS exemption represents one person counted on a tax return.",
  "These counts estimate how many people moved between states.",
  "Net means incoming minus outgoing. Rates per 1,000 use each state's 2020 population.")

# One-line description of the period behind a measure.
describe_reporting_period <- function(metric_key, irs_period_code, acs_year) {
  if (metric_key %in% irs_metric_keys)
    return(paste("IRS migration:", irs_period_labels[[irs_period_code]]))
  if (metric_key %in% c("population_growth_pct", "pop_endpoints"))
    return("Population endpoints: 2020 and 2025")
  paste("ACS reference year:", acs_year,
        "(confirm survey product and coverage in source metadata)")
}

# The note box: period(s) in bold, then the IRS definition (if any IRS measure
# is involved), then an optional warning/explanation line.
build_period_note <- function(metric_keys, irs_period_code, acs_year, warning_text = NULL) {
  periods <- vapply(metric_keys, describe_reporting_period, character(1),
                    irs_period_code = irs_period_code, acs_year = acs_year,
                    USE.NAMES = FALSE)
  includes_irs_measure <- any(metric_keys %in% irs_metric_keys)

  div(class = "atlas-period-note",
      tags$strong(paste(unique(periods), collapse = " | ")),
      if (includes_irs_measure) tags$span(class = "atlas-definition", irs_definition_text),
      if (!is.null(warning_text)) tags$span(class = "atlas-warning", warning_text))
}

# The Trends tab ignores the sidebar year/period, so it has its own notes.
build_trend_period_note <- function(metric_key) {
  if (metric_key %in% irs_metric_keys) {
    div(class = "atlas-period-note",
        tags$strong("IRS periods: 2019–2020 through 2022–2023; x-axis shows period end year."),
        tags$span(class = "atlas-definition", irs_definition_text),
        tags$span(class = "atlas-warning",
                  "Open diamonds mark partial totals; hover for missing-route counts. Sidebar year selections do not filter this chart."))
  } else if (metric_key %in% c("pop_index", "acs_population")) {
    div(class = "atlas-period-note",
        tags$strong("ACS population: reference years 2019–2024."),
        " Each point uses Total Population from that year's ACS file. Percentage change uses the same state's ACS 2020 baseline. No ACS 2025 file was supplied. Estimates may cover overlapping survey periods; confirm product coverage before interpreting changes. Sidebar year selections do not filter this chart.")
  } else if (metric_key == "pop_endpoints") {
    div(class = "atlas-period-note",
        tags$strong("Population endpoints: 2020 and 2025 only."),
        " The connecting line joins the two observations; it does not show annual estimates.")
  } else {
    div(class = "atlas-period-note",
        tags$strong("ACS reference years: 2019–2024."),
        " Confirm survey coverage in source metadata; estimates may overlap. Dollar values use their survey-year bases. Sidebar year selections do not filter this chart.")
  }
}

# Team photo for the About tab. Looks for the picture in a few folders.
render_team_portrait <- function(filename, alt_text, alternate_filename = filename) {
  renderImage({
    folders_to_search <- c(data_folder, file.path(data_folder, "www"), dirname(data_folder))
    possible_paths <- c(file.path(folders_to_search, filename),
                        file.path(folders_to_search, alternate_filename))
    image_path <- possible_paths[file.exists(possible_paths)][1]
    validate(need(!is.na(image_path), paste("Portrait file missing:", filename)))
    list(src = normalizePath(image_path), alt = alt_text,
         contentType = if (grepl("[.]png$", image_path, ignore.case = TRUE)) "image/png" else "image/jpeg")
  }, deleteFile = FALSE)
}


# =============================================================================
# 11. THE SERVER
# =============================================================================
# Inputs  (input$...)  = what the person picks in the dropdowns.
# Outputs (output$...) = what we send back to be drawn on the page.
# A reactive() is a calculation that automatically re-runs when its inputs change.

server <- function(input, output, session) {

  # ===========================================================================
  # A. NAVIGATION (sidebar buttons switch pages)
  # ===========================================================================
  # Button id  ->  page it opens. (The page names match `value =` in atlas_ui.R.)
  navigation_buttons <- c(
    overview = "go_overview", explore = "go_explore", routes = "go_routes",
    compare  = "go_compare",  trends  = "go_trends",
    about    = "go_about",    methods = "go_methods"
  )
  for (page_name in names(navigation_buttons)) {
    local({
      page_to_open <- page_name
      observeEvent(input[[navigation_buttons[[page_to_open]]]], {
        updateTabsetPanel(session, "page", selected = page_to_open)
      })
    })
  }

  # Tell the browser which page is open so the sidebar can highlight its button.
  observe({
    req(input$page)
    session$sendCustomMessage("atlas-active-page", input$page)
  })


  # ===========================================================================
  # B. SHARED DATA (used by several tabs)
  # ===========================================================================

  # The state the person last clicked on the map (or chose in the route menu).
  clicked_state_abbr <- reactiveVal("SC")
  observeEvent(input$route_state, {
    if (input$route_state %in% state_population$abbr) clicked_state_abbr(input$route_state)
  }, ignoreInit = TRUE)

  # THE MAIN TABLE: one row per state, for the ACS year and IRS period chosen in
  # the top controls. Built by joining three tables on the state FIPS code:
  #   state_population  +  acs_by_state_year (one year)  +  irs_state_totals (one period)
  state_metrics <- reactive({
    acs_for_selected_year   <- filter(acs_by_state_year, acs_year == as.integer(input$acs_year))
    irs_for_selected_period <- filter(irs_state_totals, irs_period == input$irs_period)

    merged_table <- state_population %>%
      inner_join(acs_for_selected_year, by = "fips") %>%
      inner_join(irs_for_selected_period, by = "fips") %>%
      arrange(state)

    validate(need(nrow(merged_table) == 50, "Expected 50 states in the merged dataset."))
    merged_table
  })


  # ===========================================================================
  # C. OVERVIEW TAB  (atlas_ui.R: overview_tab)
  # ===========================================================================

  # Note above the map describing the period behind the chosen measure.
  output$map_period_note <- renderUI({
    metric_key <- input$map_metric
    explanation <- if (metric_key %in% diverging_metric_keys) {
      "Blue = net gains; orange = net losses; the scale is centered at zero."
    } else {
      "Darker blue indicates a larger value; read the legend for the selected range."
    }
    build_period_note(metric_key, input$irs_period, input$acs_year, explanation)
  })

  # --- The map: states colored by the measure picked in "Map measure" ---------
  output$state_map <- renderPlotly({
    metric_key <- input$map_metric
    req(metric_key)

    # Step 1: add the chosen measure to the state table as a column named `value`.
    map_data <- state_metrics()
    map_data$value <- get_metric_values(map_data, metric_key)
    req(nrow(map_data) == 50L)

    # Step 2: build the text shown when hovering over a state.
    metric_label <- metric_labels[[metric_key]]
    formatted_values <- vapply(map_data$value, format_metric_value, character(1),
                               metric_key = metric_key)
    map_data$hover_text <- paste0(
      "<b>", map_data$state, " (", map_data$abbr, ")</b>",
      "<br>", metric_label, ": <b>", formatted_values, "</b>",
      "<br>Reported net exemptions: ", format_signed(map_data$net_exemptions, 0),
      ifelse(map_data$has_missing_routes, " (partial)", ""),
      "<br>2020–2025 population growth: ",
      format_number(map_data$population_growth_pct, 2), "%"
    )

    # Step 3: choose the color range (two-color for net measures, blues otherwise).
    color_scale <- build_map_color_scale(map_data$value,
                                         is_diverging = metric_key %in% diverging_metric_keys)
    legend_title <- if (metric_key %in% names(map_legend_titles)) {
      map_legend_titles[[metric_key]]
    } else {
      "Value"
    }

    # Step 4: draw the states that have a value...
    states_with_value    <- filter(map_data, is.finite(value))
    states_without_value <- filter(map_data, !is.finite(value))

    state_map <- plot_ly(
      data = states_with_value,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~abbr,
      z = ~value,
      text = ~hover_text,
      customdata = ~abbr,
      key = ~abbr,
      hovertemplate = "%{text}<extra></extra>",
      colorscale = color_scale$colors,
      zmin = color_scale$min,
      zmax = color_scale$max,
      marker = list(line = list(color = "#ffffff", width = 0.85)),
      colorbar = list(
        title = list(text = legend_title, side = "top"),
        thickness = 15, len = 0.67, x = 1.01, y = 0.5,
        tickfont = list(size = 11, color = "#526372"),
        outlinewidth = 0
      ),
      source = "state_choropleth",
      showlegend = FALSE
    ) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = FALSE,
          showcoastlines = FALSE,
          showland = FALSE,
          bgcolor = "rgba(0,0,0,0)"
        ),
        margin = list(l = 0, r = 95, t = 8, b = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Arial, sans-serif", color = "#182c3d")
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE, scrollZoom = FALSE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"))

    # ...and paint any states with no data gray.
    if (nrow(states_without_value) > 0L) {
      state_map <- add_trace(
        state_map, data = states_without_value, inherit = FALSE,
        type = "choropleth", locationmode = "USA-states", locations = ~abbr,
        z = rep(0, nrow(states_without_value)), zmin = 0, zmax = 1,
        colorscale = list(c(0, "#c5cbd1"), c(1, "#c5cbd1")),
        showscale = FALSE, showlegend = FALSE,
        text = ~paste0(state, "<br>", metric_label, ": No data"),
        customdata = ~abbr, key = ~abbr,
        hovertemplate = "%{text}<extra></extra>",
        marker = list(line = list(color = "#ffffff", width = 0.85)))
    }
    state_map
  })

  # Small note under the map (gray = no data, partial-total warning).
  output$map_source_note <- renderUI({
    number_of_partial_states <- sum(state_metrics()$has_missing_routes)
    div(class = "atlas-map-note",
        tags$span(class = "atlas-missing-key", "Gray = no data; zero is a measured value."),
        tags$span(" Color limits adjust to the selected data; compare legend values, not shades, across years."),
        if (input$map_metric %in% irs_metric_keys && number_of_partial_states > 0)
          paste(number_of_partial_states,
                "states have partial IRS totals; hover over states for flags."))
  })

  # Clicking a state on the map selects it (the click payload may identify the
  # state in any of three fields, so check each one).
  observeEvent(event_data("plotly_click", source = "state_choropleth"), {
    click_info <- event_data("plotly_click", source = "state_choropleth")
    if (is.null(click_info) || nrow(click_info) == 0L) return()

    clicked_abbr <- NULL
    for (field_name in c("location", "customdata", "key")) {
      if (field_name %in% names(click_info)) {
        candidate <- as.character(click_info[[field_name]][1])
        if (length(candidate) == 1L && !is.na(candidate) &&
            candidate %in% state_population$abbr) {
          clicked_abbr <- candidate
          break
        }
      }
    }
    if (!is.null(clicked_abbr)) {
      clicked_state_abbr(clicked_abbr)
      updateSelectInput(session, "route_state", selected = clicked_abbr)
    }
  })

  # Summary strip for the clicked state, shown under the map.
  output$selected_details <- renderUI({
    clicked_state <- filter(state_metrics(), abbr == clicked_state_abbr())
    if (!nrow(clicked_state)) return(NULL)

    div(class = "atlas-selection",
        tags$span(class = "atlas-selection-label", "Selected state"),
        tags$b(paste0(clicked_state$state, " (", clicked_state$abbr, ")")), tags$br(),
        "Reported net IRS exemptions: ",
        tags$b(format_signed(clicked_state$net_exemptions, 0)), " · ",
        "2020–25 growth: ",
        tags$b(paste0(format_signed(clicked_state$population_growth_pct, 2), "%")),
        if (clicked_state$has_missing_routes)
          tags$span(" · Incomplete route data: ",
                    tags$b(clicked_state$missing_inflow_routes + clicked_state$missing_outflow_routes),
                    " suppressed/missing inbound or outbound records")
    )
  })

  # Four summary boxes: biggest gainer, biggest loser, fastest growth, etc.
  output$overview_kpis <- renderUI({
    states <- state_metrics()
    biggest_net_gain <- states[which.max(states$net_per_1000), ]
    biggest_net_loss <- states[which.min(states$net_per_1000), ]
    fastest_growing  <- states[which.max(states$population_growth_pct), ]

    div(class = "kpi-grid",
        stat_box("States with reported net gains",
                 paste0(sum(states$net_exemptions > 0), " / 50"),
                 irs_period_labels[[input$irs_period]]),
        stat_box("Largest reported net gain / 1,000",
                 format_signed(biggest_net_gain$net_per_1000), biggest_net_gain$state),
        stat_box("Largest reported net loss / 1,000",
                 format_signed(biggest_net_loss$net_per_1000), biggest_net_loss$state),
        stat_box("Fastest population growth",
                 paste0(format_number(fastest_growing$population_growth_pct, 2), "%"),
                 paste(fastest_growing$state, "· 2020–2025"))
    )
  })

  output$ranking_period_note <- renderUI({
    build_period_note("net_per_1000", input$irs_period, input$acs_year,
                      "* indicates partial IRS totals. Rankings use reported totals, including partial states.")
  })

  # Bar chart: the 6 states with the highest and the 6 with the lowest net migration.
  output$net_rank_plot <- renderPlot({
    states_ranked_high_to_low <- arrange(state_metrics(), desc(net_per_1000))
    top_and_bottom_states <- bind_rows(head(states_ranked_high_to_low, 6),
                                       tail(states_ranked_high_to_low, 6))
    top_and_bottom_states$label <- paste0(top_and_bottom_states$state,
                                          ifelse(top_and_bottom_states$has_missing_routes, " *", ""))
    top_and_bottom_states$gain_or_loss <- ifelse(top_and_bottom_states$net_per_1000 >= 0,
                                                 "Net gain", "Net loss")

    ggplot(top_and_bottom_states,
           aes(reorder(label, net_per_1000), net_per_1000, fill = gain_or_loss)) +
      geom_bar(stat = "identity", width = .7) + coord_flip() +
      scale_fill_manual(values = c("Net gain" = "#2459aa", "Net loss" = "#b46228")) +
      labs(x = NULL, y = metric_labels[["net_per_1000"]], caption = "* Partial IRS route data") +
      atlas_chart_theme() +
      theme(legend.position = "none", panel.grid.major.y = element_blank())
  })


  # ===========================================================================
  # D. EXPLORE FACTORS TAB  (atlas_ui.R: explore_factors_tab)
  # ===========================================================================

  # One row per state with the chosen factor (x_value) and outcome (y_value).
  # States missing either value are dropped.
  scatter_data <- reactive({
    plot_data <- state_metrics()
    plot_data$x_value <- get_metric_values(plot_data, input$x_metric)
    plot_data$y_value <- get_metric_values(plot_data, input$y_metric)
    plot_data <- filter(plot_data, is.finite(x_value), is.finite(y_value))

    plot_data$hover_text <- paste0(
      plot_data$state,
      "<br>", metric_labels[[input$x_metric]], ": ", round(plot_data$x_value, 2),
      "<br>", metric_labels[[input$y_metric]], ": ", round(plot_data$y_value, 2))
    plot_data
  })

  # The note above the scatterplot and the correlation table is the same.
  scatter_period_note_ui <- reactive({
    plot_data <- scatter_data()
    outcome_is_irs <- input$y_metric %in% irs_metric_keys

    warning_text <- paste("The ACS and IRS have different reporting windows.")
    if (outcome_is_irs) {
      warning_text <- paste(warning_text, sum(plot_data$has_missing_routes),
                            "Open diamond have partial IRS totals.; they remain in the fit and correlation.")
    }
    build_period_note(c(input$x_metric, input$y_metric), input$irs_period,
                      input$acs_year, warning_text)
  })
  output$scatter_period_note     <- renderUI(scatter_period_note_ui())
  output$correlation_period_note <- renderUI(scatter_period_note_ui())

  # Scatterplot: one dot per state, plus a dashed best-fit line.
  output$scatter_plot <- renderPlotly({
    plot_data <- scatter_data()
    validate(need(nrow(plot_data) >= 3 && sd(plot_data$x_value) > 0 && sd(plot_data$y_value) > 0,
                  "Not enough variation for a scatterplot."))

    outcome_is_irs <- input$y_metric %in% irs_metric_keys
    is_partial_state <- outcome_is_irs & plot_data$has_missing_routes   # open-diamond dots

    # Add the reporting periods (and partial-data warning) to the hover text.
    plot_data$hover_text <- paste0(
      plot_data$hover_text,
      "<br>", describe_reporting_period(input$x_metric, input$irs_period, input$acs_year),
      "<br>", describe_reporting_period(input$y_metric, input$irs_period, input$acs_year),
      if (outcome_is_irs)
        ifelse(is_partial_state,
               paste0("<br><b>Partial IRS totals:</b> ", plot_data$missing_inflow_routes,
                      " incoming and ", plot_data$missing_outflow_routes,
                      " outgoing routes missing/unreported"),
               "<br>All 50 inbound and 50 outbound N2 routes reported")
      else "")

    # Best-fit straight line: y = intercept + slope * x
    linear_fit <- lm(y_value ~ x_value, data = plot_data)
    fit_line <- data.frame(x_value = range(plot_data$x_value))
    fit_line$y_value <- predict(linear_fit, newdata = fit_line)

    scatter <- plot_ly(plot_data, x = ~x_value, y = ~y_value, text = ~hover_text,
                       customdata = ~abbr, type = "scatter", mode = "markers",
                       hoverinfo = "text",
                       marker = list(color = "#2459aa",
                                     symbol = ifelse(is_partial_state, "diamond-open", "circle"),
                                     size = 11, line = list(width = 1.5)),
                       source = "scatter", showlegend = FALSE)
    scatter <- add_lines(scatter, data = fit_line, x = ~x_value, y = ~y_value,
                         inherit = FALSE, hoverinfo = "skip", showlegend = FALSE,
                         line = list(color = "#b46228", dash = "dash", width = 3))

    layout(scatter,
           xaxis = list(title = metric_labels[[input$x_metric]], automargin = TRUE,
                        gridcolor = "#e4e9ed", zeroline = FALSE),
           yaxis = list(title = metric_labels[[input$y_metric]], automargin = TRUE,
                        gridcolor = "#e4e9ed", zeroline = FALSE),
           margin = list(l = 70, r = 15, b = 85, t = 15),
           paper_bgcolor = "white", plot_bgcolor = "white",
           font = list(family = "Arial, sans-serif", size = 12, color = "#182c3d")) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })

  # Clicking a dot selects that state.
  observeEvent(event_data("plotly_click", source = "scatter"), {
    click_info <- event_data("plotly_click", source = "scatter")
    if (!is.null(click_info) && "customdata" %in% names(click_info)) {
      clicked_abbr <- as.character(click_info$customdata[1])
      if (clicked_abbr %in% state_population$abbr) clicked_state_abbr(clicked_abbr)
    }
  })

  # Four boxes under the scatterplot: correlation r, R-squared, p-value, count.
  output$scatter_statistics <- renderUI({
    plot_data <- scatter_data()
    if (nrow(plot_data) < 4 || sd(plot_data$x_value) == 0 || sd(plot_data$y_value) == 0) {
      return(div(class = "atlas-alert", "Insufficient data for correlation."))
    }
    correlation_test <- cor.test(plot_data$x_value, plot_data$y_value, method = "pearson")
    correlation_r <- unname(correlation_test$estimate)

    strength_label <-
      if (abs(correlation_r) < .1) "Near-zero linear"
      else if (abs(correlation_r) < .3) "Weak"
      else if (abs(correlation_r) < .5) "Moderate"
      else "Relatively strong"

    div(class = "kpi-grid",
        stat_box("Pearson correlation (r)", format_signed(correlation_r, 3)),
        stat_box("Simple linear R²", format_number(correlation_r^2, 3)),
        stat_box("P-value", format.pval(correlation_test$p.value, digits = 3)),
        stat_box("States plotted", as.character(nrow(plot_data)), strength_label)
    )
  })

  # Table: how strongly EVERY factor correlates with the chosen outcome.
  output$correlation_table <- renderDT({
    states <- state_metrics()
    outcome_values <- get_metric_values(states, input$y_metric)
    factor_keys <- unname(x_axis_choices)

    correlations <- vapply(factor_keys, function(factor_key) {
      safe_correlation(get_metric_values(states, factor_key), outcome_values)
    }, numeric(1), USE.NAMES = FALSE)

    correlation_table <- data.frame(
      Factor = unname(metric_labels[factor_keys]),
      `Pearson r` = round(correlations, 3),
      check.names = FALSE
    )
    # Strongest correlation first, ignoring the + or - sign.
    correlation_table <- arrange(correlation_table, desc(abs(`Pearson r`)))
    datatable(correlation_table, rownames = FALSE,
              options = list(pageLength = 9, dom = "t", ordering = FALSE))
  })

  # Download the dots shown in the scatterplot.
  output$download_scatter <- downloadHandler(
    filename = function() paste0("scatter_", input$x_metric, "_vs_", input$y_metric, ".csv"),
    content = function(file) {
      download_data <- scatter_data()[, c("fips", "state", "abbr", "x_value", "y_value")]
      names(download_data)[4:5] <- c(input$x_metric, input$y_metric)
      write.csv(download_data, file, row.names = FALSE)
    }
  )


  # ===========================================================================
  # E. MIGRATION ROUTES TAB  (atlas_ui.R: migration_routes_tab)
  # ===========================================================================

  # All routes into (or out of) the chosen state in the chosen IRS period,
  # largest first. Adds `partner_state` = the state on the OTHER end of the route.
  routes_for_selected_state <- reactive({
    routes_in_period <- filter(irs_route_flows, irs_period == input$irs_period)
    selected_fips <- state_population$fips[match(input$route_state, state_population$abbr)]
    req(length(selected_fips) == 1L, !is.na(selected_fips))

    if (input$route_direction == "in") {
      routes <- filter(routes_in_period, destination_fips == selected_fips)
      routes$partner_state <- routes$origin_state
    } else {
      routes <- filter(routes_in_period, origin_fips == selected_fips)
      routes$partner_state <- routes$destination_state
    }
    arrange(routes, desc(exemptions))
  })

  # The 10 largest routes that have a reported count (for the bar chart).
  top_10_routes <- reactive({
    routes_for_selected_state() %>%
      filter(is.finite(exemptions)) %>%
      head(10)
  })

  # Four summary boxes for the chosen state.
  output$route_kpis <- renderUI({
    chosen_state <- filter(state_metrics(), abbr == input$route_state)
    if (!nrow(chosen_state)) return(NULL)

    div(class = "kpi-grid",
        stat_box("Incoming migration proxy (N2)", format_number(chosen_state$inflow_exemptions)),
        stat_box("Outgoing migration proxy (N2)", format_number(chosen_state$outflow_exemptions)),
        stat_box("Net reported exemptions", format_signed(chosen_state$net_exemptions, 0),
                 if (chosen_state$has_missing_routes) "Partial: missing/suppressed routes"
                 else "All 50 routes reported"),
        stat_box("Net per 1,000 (2020 base)", format_signed(chosen_state$net_per_1000, 2))
    )
  })

  output$route_chart_period_note <- renderUI({
    build_period_note("inflow_exemptions", input$irs_period, input$acs_year,
                      "Chart: ten largest routes with reported N2. Table: all available route records.")
  })
  output$route_table_period_note <- renderUI({
    build_period_note("inflow_exemptions", input$irs_period, input$acs_year,
                      "Missing values are shown as N/A. Omitted source pairs cannot appear as records.")
  })

  # Bar chart of the ten largest routes.
  output$route_bar <- renderPlot({
    largest_routes <- top_10_routes()
    validate(need(nrow(largest_routes) > 0, "No valid interstate route records available."))

    bar_color <- if (input$route_direction == "in") "#2459aa" else "#b46228"
    ggplot(largest_routes, aes(x = reorder(partner_state, exemptions), y = exemptions)) +
      geom_bar(stat = "identity", fill = bar_color, width = .75) + coord_flip() +
      labs(x = NULL, y = "Reported IRS exemptions (N2)") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
      atlas_chart_theme() +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(10, 15, 10, 5))
  })

  # Table of every route (including routes with missing counts).
  output$route_table <- renderDT({
    routes <- routes_for_selected_state()
    route_table <- data.frame(
      `From → To` = paste(routes$origin_state, "→", routes$destination_state),
      `Returns (N1)` = routes$tax_returns,
      `Exemptions (N2)` = routes$exemptions,
      `AGI ($ millions)` = round(routes$agi_thousands / 1000, 1),
      Status = ifelse(!is.finite(routes$exemptions), "N2 unreported",
                      ifelse(!is.finite(routes$tax_returns) | !is.finite(routes$agi_thousands),
                             "Other field unreported", "Reported")),
      check.names = FALSE)

    datatable(route_table, rownames = FALSE,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 50), scrollX = TRUE,
                             dom = "lftip", order = list(),
                             columnDefs = list(list(targets = "_all", defaultContent = "N/A"))))
  })


  # ===========================================================================
  # F. COMPARE STATES TAB  (atlas_ui.R: compare_states_tab)
  # ===========================================================================

  output$comparison_period_note <- renderUI({
    build_period_note(c("net_per_1000", "median_household_income", "population_growth_pct"),
                      input$irs_period, input$acs_year)
  })

  # Side-by-side table: one row per measure, one column per state.
  output$state_comparison <- renderDT({
    states <- state_metrics()
    state_one <- filter(states, abbr == input$state_a)
    state_two <- filter(states, abbr == input$state_b)
    req(nrow(state_one) == 1L, nrow(state_two) == 1L)

    comparison_table <- data.frame(
      Measure = unname(metric_labels[comparison_metric_keys]),
      state_one_values = vapply(comparison_metric_keys, function(metric_key)
        format_metric_value(state_one[[metric_key]], metric_key), character(1), USE.NAMES = FALSE),
      state_two_values = vapply(comparison_metric_keys, function(metric_key)
        format_metric_value(state_two[[metric_key]], metric_key), character(1), USE.NAMES = FALSE)
    )
    # Final row: how many IRS routes are missing for each state.
    missing_routes_row <- data.frame(
      Measure = "Suppressed/missing IRS route records (in+out)",
      state_one_values = as.character(state_one$missing_inflow_routes + state_one$missing_outflow_routes),
      state_two_values = as.character(state_two$missing_inflow_routes + state_two$missing_outflow_routes)
    )
    comparison_table <- rbind(comparison_table, missing_routes_row)

    # Use the state names as the column headings.
    names(comparison_table)[2:3] <- c(state_one$state, state_two$state)

    datatable(comparison_table, rownames = FALSE,
              options = list(pageLength = 18, dom = "t", ordering = FALSE))
  })


  # ===========================================================================
  # G. TRENDS TAB  (atlas_ui.R: trends_tab)
  # ===========================================================================

  output$trend_period_note <- renderUI(build_trend_period_note(input$trend_metric))

  # Data for the line chart. Always has columns: state, year, value
  # (plus extra columns that depend on which measure was chosen).
  trend_data <- reactive({
    chosen_abbrs <- c(input$trend_state_a, input$trend_state_b)
    metric_key <- input$trend_metric
    state_names <- select(state_population, fips, state, abbr)

    # (1) ACS population change: % change from each state's ACS 2020 population.
    if (metric_key == "pop_index") {
      acs_2020_population <- acs_by_state_year %>%
        filter(acs_year == 2020) %>%
        select(fips, baseline_population = acs_population)

      acs_with_change <- acs_by_state_year %>%
        left_join(acs_2020_population, by = "fips") %>%
        left_join(state_names, by = "fips") %>%
        filter(abbr %in% chosen_abbrs) %>%
        mutate(value = ifelse(is.finite(baseline_population) & baseline_population > 0,
                              100 * (acs_population / baseline_population - 1), NA_real_)) %>%
        arrange(state, acs_year)

      return(data.frame(state = acs_with_change$state, year = acs_with_change$acs_year,
                        value = acs_with_change$value,
                        population = acs_with_change$acs_population))
    }

    # (2) Population endpoints: only two real observations (2020 and 2025).
    if (metric_key == "pop_endpoints") {
      chosen_states <- filter(state_population, abbr %in% chosen_abbrs)
      year_2020_row <- data.frame(state = chosen_states$state, year = 2020, value = 0)
      year_2025_row <- data.frame(state = chosen_states$state, year = 2025,
                                  value = chosen_states$population_growth_pct)
      # No invented annual values between 2020 and 2025.
      return(bind_rows(year_2020_row, year_2025_row) %>% arrange(state, year))
    }

    # (3) IRS migration measures: one point per IRS period (plotted at its end year).
    if (metric_key %in% irs_metric_keys) {
      irs_for_chosen_states <- irs_state_totals %>%
        left_join(state_names, by = "fips") %>%
        filter(abbr %in% chosen_abbrs)

      return(data.frame(
        state = irs_for_chosen_states$state,
        year = unname(irs_period_end_year[irs_for_chosen_states$irs_period]),
        value = irs_for_chosen_states[[metric_key]],
        irs_period = irs_for_chosen_states$irs_period,
        has_missing_routes = irs_for_chosen_states$has_missing_routes,
        missing_inflow_routes = irs_for_chosen_states$missing_inflow_routes,
        missing_outflow_routes = irs_for_chosen_states$missing_outflow_routes
      ))
    }

    # (4) Any other ACS measure: one point per ACS year.
    acs_for_chosen_states <- acs_by_state_year %>%
      left_join(state_names, by = "fips") %>%
      filter(abbr %in% chosen_abbrs)
    data.frame(state = acs_for_chosen_states$state, year = acs_for_chosen_states$acs_year,
               value = acs_for_chosen_states[[metric_key]])
  })

  # Line chart comparing the two chosen states over time.
  output$trend_plot <- renderPlotly({
    metric_key <- input$trend_metric
    is_irs_metric <- metric_key %in% irs_metric_keys

    plot_data <- filter(trend_data(), is.finite(value))
    validate(need(nrow(plot_data) > 0, "No observations available for this indicator."))

    # Hover text depends on which kind of measure is shown.
    plot_data$partial <- if (is_irs_metric) plot_data$has_missing_routes else FALSE
    plot_data$tooltip <- paste0(plot_data$state, " · ", plot_data$year, ": ",
                                format_number(plot_data$value, 2))
    if (is_irs_metric) {
      plot_data$tooltip <- paste0(
        plot_data$state,
        "<br>IRS ", irs_period_labels[plot_data$irs_period],
        "<br>", metric_labels[[metric_key]], ": ", format_number(plot_data$value, 2),
        ifelse(plot_data$partial,
               paste0("<br>Partial totals: ", plot_data$missing_inflow_routes,
                      " incoming and ", plot_data$missing_outflow_routes,
                      " outgoing routes missing/unreported"),
               "<br>All inbound and outbound N2 routes reported"))
    } else if (metric_key == "pop_index") {
      plot_data$tooltip <- paste0(
        plot_data$state, "<br>ACS reference year: ", plot_data$year,
        "<br>Total population: ", format_number(plot_data$population),
        "<br>Change from ACS 2020: ", format_number(plot_data$value, 2), "%")
    } else if (metric_key == "pop_endpoints") {
      plot_data$tooltip <- paste0(plot_data$tooltip, "% change from 2020",
                                  "<br>Source: state_population_growth_clean.csv")
    } else {
      plot_data$tooltip <- paste0(
        plot_data$state, "<br>ACS reference year: ", plot_data$year,
        "<br>", metric_labels[[metric_key]], ": ", format_number(plot_data$value, 2))
    }
    plot_data <- arrange(plot_data, state, year)

    # Y-axis title.
    y_axis_title <- if (metric_key == "pop_index") {
      "ACS population change from ACS 2020 (%)"
    } else if (metric_key == "pop_endpoints") {
      "Population change from 2020 (%)"
    } else {
      metric_labels[[metric_key]]
    }

    trend_chart <- ggplot(plot_data, aes(year, value, color = state, group = state, text = tooltip)) +
      geom_line(linewidth = 1.1) + geom_point(aes(shape = partial), size = 3.2) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 5),
                         breaks = c(FALSE, TRUE),
                         labels = c("Complete routes", "Partial routes"),
                         name = "IRS coverage") +
      scale_color_manual(values = c("#2459aa", "#b46228")) +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      scale_y_continuous(labels = function(y_values)
        format(y_values, big.mark = ",", scientific = FALSE, trim = TRUE)) +
      labs(x = if (is_irs_metric) "IRS period end year" else "Year",
           y = y_axis_title, color = "State") +
      atlas_chart_theme() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    if (!any(plot_data$partial)) trend_chart <- trend_chart + guides(shape = "none")

    ggplotly(trend_chart, tooltip = "text", source = "trends") %>%
      layout(margin = list(l = 75, r = 15, b = 65, t = 15)) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })


  # ===========================================================================
  # H. ABOUT US TAB  (atlas_ui.R: about_tab)
  # ===========================================================================
  # Team photos. The id (left) must match the photo ids in atlas_ui.R.
  team_portraits <- list(
    photo_chacon  = list(name = "Lucas Chacon", file = "Head Portrait - Lucas Chacon.png",
                         alternate_file = "Head Portrait - Lucas Chacon (1).png"),
    photo_kim     = list(name = "Lucas Kim", file = "SPIA Portraits 6-12-26-4917.jpg",
                         alternate_file = "SPIA Portraits 6-12-26-4917.jpg"),
    photo_salazar = list(name = "Diana Salazar", file = "Diana_2.jpg",
                         alternate_file = "Diana_2.jpeg")
  )
  for (photo_id in names(team_portraits)) {
    local({
      portrait <- team_portraits[[photo_id]]
      output[[photo_id]] <- render_team_portrait(portrait$file, portrait$name,
                                                 portrait$alternate_file)
    })
  }


  # ===========================================================================
  # I. DATA & METHODS TAB  (atlas_ui.R: data_and_methods_tab)
  # ===========================================================================

  # One download link per original CSV: copies the file unchanged.
  for (csv_filename in source_files) {
    local({
      this_csv <- csv_filename
      output[[csv_download_id(this_csv)]] <- downloadHandler(
        filename = this_csv, contentType = "text/csv",
        content = function(file_path) {
          if (!file.copy(file.path(data_folder, this_csv), file_path, overwrite = TRUE))
            stop("Could not download ", this_csv)
        })
    })
  }

  # Export the main state table for the selected ACS year and IRS period.
  output$download_master <- downloadHandler(
    filename = function() paste0("american_migration_clean_", input$irs_period,
                                 "_ACS", input$acs_year, ".csv"),
    content = function(file_path) write.csv(state_metrics(), file_path, row.names = FALSE)
  )
}
