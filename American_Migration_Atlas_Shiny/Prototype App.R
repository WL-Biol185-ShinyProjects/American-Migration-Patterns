#Load's the app's tools
#The Shiny package creates the interactive app.
#The dplyr package organizes, filters, and combines data.
#The ggplot2 package makes charts.
# The ploty package adds interactive features such as hover information.
#The DT package displays searchable and sortable tables.
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# repo_candidates - - - stores possible folders where our data might be.
# unique() - - - removes repeated folder location so each appears only once.
# getwd() - - - returns the folder R is currently working in.
# file.path() - - - combines folder names into a complete location.
# if (...) stop(...) - - - stops the app with a message if no matching folder exists.
# repo_dir - - - stores the first matching folder as the location to use.
# read_source() - - - opens a requested CSV file from that folder.

# Store three possible locations where our CSV files might be:
# getwd(), American_Migration_Atlas_Shiny, and Atlas_Team_Photos_Ready
# the current working folder and two named subfolders.
# Remove duplicate locations and save the result as repo_candidates.
# This step only lists possible locations; it does not check or open them.
repo_candidates <- unique(c(
  getwd(),
  file.path(getwd(), "American_Migration_Atlas_Shiny"),
  file.path(getwd(), "Atlas_Team_Photos_Ready")
))
# repo_hits - - - stores the folders where the expected CSV was actually found.
# repo_candidates - - - keeps only the folders whose checks returned TRUE.
# file.exists - - - checks each complete location and returns TRUE if the file exists or FALSE if it does not.

# Check each possible folder for state_population_growth_clean.csv.
# Keep only the folder locations where that file exists.
# Store those matching locations as repo_hits.
# This checks for the file but does NOT open or read it.
repo_hits <- repo_candidates[file.exists(file.path(repo_candidates,
                                                   "state_population_growth_clean.csv"))]

# Count how many matching folders were found.
# If the count is 0, stop the app and explain where to put the CSVs.
if (length(repo_hits) == 0L) {
  stop("Could not locate state_population_growth_clean.csv. Keep the supplied CSVs " ,
       "beside app.R or in American_Migration_Atlas_Shiny.")
}
# repo_dir - - - The folder location the app will use to find its data
# repo_hits[1] - - - selects the first folder where the population .csv was found.
# normalizePath() - - - writes out that folder's full address on the computer.
# mustWork = TRUE - - - require R to successfully find that location; otherwise, report an error.
repo_dir <- normalizePath(repo_hits[1], mustWork = TRUE)

# Create a reusable function called read_source.
# filename is a placeholder for whichever CSV name we give it.
# Everything inside { } belongs to this function
read_source <- function(filename) {
# path - - - a name that stores the file's location on my computer. (i.e. the file's address)
# file.path - - - joins the folder location and filename to build that address.
# if - - - only do this if the condition is true
# file.exists  - - - checks whether a file exists at the given address. It answers TRUE for yes or FALSE for no.
# Adding the ! reverses the answer, so !file.exists(path) checks whether the file is missing.
# stop() - - - stops the code and displays an error message explaining the problem.
# data - - - the name I give to the table after reading it from the CSV. It holds the file's contents inside R.
# UTF-8-BOM - - - tells R how to read the file's text correctly. UTF-8 is a text format; BOM is a special marker that
# may appear at the beginning of the file.
# na.strings - - - a setting that tells R which text entries should be treated as missing information.
  path <- file.path(repo_dir, filename)
  if (!file.exists(path)) stop("Missing CSV in your app folder: ", filename)
  data <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8-BOM", na.strings = c("NA", "", "N/A"))
# column_names - - - a name that stores the table's column headings, such as "State" or "Total.Population".
# names() - - - gets or changes those column headings. 
# names(data) - - - retrieves them
# names(data) <- replaces them
# blank_names - - - stores a TRUE or FALSE for each heading. TRUE means the heading is missing, empty, 
# or only spaces and needs a replacement.
# is.na() - - -  checks whether something is missing. It returns TRUE for a missing value and FALSE otherwise. 
# trimws - - -  removes spaces from the beginning and end of text. Here, it helps recognize a heading containing only spaces as empty.
# paste0() - - - joins pieces of text together without adding spaces. For example, paste0("unnamed_column_", 3) produces
# "unnamed_column_3".
# which() - - - tells you the positions where a condition is TRUE. If the second and fifth headings are blank,
# which(blank_names) returns 2 and 5.
# make.unique() - - - makes repeated names different by adding numbered endings. With sep = "_", two headings
# called "State" become "State" and "State_1".
  column_names <- names(data)
  blank_names <- is.na(column_names) | trimws(column_names) == ""
  column_names[blank_names] <- paste0("unnamed_column_", which(blank_names))
  names(data) <- make.unique(column_names, sep = "_")
  data
}
# as_number() - - - a function our code creates to turn text into numbers. 
# It removes dollar signs, commas, percent signs, and spaces first.
as_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[$,% ]", "", as.character(x))))
}

# canon_flips() - - - a function our code creates to put state identification codes into a consistent two-digit format.
# For example, 1 becomes "01", while 45 stays "45".
canon_fips <- function(x) sprintf("%02d", as.integer(as_number(x)))

# period_labels - - - stores readable year labels for the IRS period codes. 
# For example, it connects "1920" with "2019-2020" so the app can display a clear reporting period.
period_labels <- c("1920" = "2019–2020", "2021" = "2020–2021",
                   "2122" = "2021–2022", "2223" = "2022–2023")

# (Left off re-commenting here!!) This population-estimates file has 2020 and 2025 endpoints.
# ACS population for reference years 2019–2024 is loaded separately below.
raw_pop <- read_source("state_population_growth_clean.csv")
stopifnot(all(c("STATE", "NAME", "POPESTIMATE2020", "POPESTIMATE2025",
                "population_growth_pct") %in% names(raw_pop)))
pop <- filter(raw_pop, NAME %in% state.name)
pop <- data.frame(
  fips = canon_fips(pop$STATE), state = pop$NAME,
  abbr = state.abb[match(pop$NAME, state.name)],
  population_2020 = as_number(pop$POPESTIMATE2020),
  population_2025 = as_number(pop$POPESTIMATE2025),
  population_growth_pct = as_number(pop$population_growth_pct),
  stringsAsFactors = FALSE
)
pop <- arrange(pop, state)
stopifnot(nrow(pop) == 50L, !anyDuplicated(pop$fips), !anyNA(pop))
stopifnot(all(abs(pop$population_growth_pct -
                    100 * (pop$population_2025 - pop$population_2020) / pop$population_2020) < 0.011))

# Lessons 4, 6, and 10: one readable function cleans each yearly table.
# The 2024 export uses different column names, so name those differences directly.
acs_filename <- function(year) {
  filename <- paste0("ACS_", year, "_state_new.csv")
  if (year == 2024 && !file.exists(file.path(repo_dir, filename))) {
    filename <- "ACS_2024_state_new(1).csv"
  }
  filename
}

load_acs_year <- function(year) {
  filename <- acs_filename(year)
  raw <- read_source(filename)
  raw$fips <- canon_fips(raw$FIPS)
  raw <- filter(raw, fips %in% pop$fips)
  stopifnot(nrow(raw) == 50L, !anyDuplicated(raw$fips))
  
  income_column <- paste0(
    "Median Household Income (In ", year, " Inflation Adjusted Dollars)"
  )
  labor_column <- "Civilian Population in Labor Force 16 Years and Over:"
  home_column <- "Median Value"
  rent_share_column <- paste0(
    "Median Gross Rent as a Percentage of Household ",
    " Income in the Past 12 Months (Dollars)"
  )
  poverty_total_column <- "Population for Whom Poverty Status Is Determined:"
  poverty_prefix <- "Population for Whom Poverty Status Is Determined: "
  
  if (year == 2024) {
    labor_column <- "Civilian Population in Labor Force 16 Years and Over"
    home_column <- "Median House Value"
    rent_share_column <- "Median Gross Rent as a Percentage of Household Income"
    poverty_total_column <- "Population for Whom Poverty Status Is Determined"
    poverty_prefix <- paste0(poverty_prefix, "Ratio of Income to Poverty Level ")
  }
  # read.csv(check.names = FALSE) preserves spaces; also accept R-style dots.
  population_column <- "Total Population"
  if (!population_column %in% names(raw)) population_column <- "Total.Population"
  required_columns <- c(
    population_column, income_column, labor_column, home_column, rent_share_column,
    poverty_total_column, paste0(poverty_prefix, c("Under .50", ".50 to .74", ".75 to .99")),
    "Median Gross Rent", "Average Commute to Work (In Min)",
    "Civilian Population in Labor Force 16 Years and Over: Unemployed"
  )
  if (!all(required_columns %in% names(raw))) {
    stop("An expected column is missing from ", filename)
  }
  
  # [[name]] selects a column whose name is stored in a variable (lesson 5).
  poverty_below <- as_number(raw[[paste0(poverty_prefix, "Under .50")]]) +
    as_number(raw[[paste0(poverty_prefix, ".50 to .74")]]) +
    as_number(raw[[paste0(poverty_prefix, ".75 to .99")]])
  labor_force <- as_number(raw[[labor_column]])
  poverty_total <- as_number(raw[[poverty_total_column]])
  unemployed <- as_number(
    raw[["Civilian Population in Labor Force 16 Years and Over: Unemployed"]]
  )
  data.frame(
    fips = raw$fips, acs_year = as.integer(year),
    acs_population = as_number(raw[[population_column]]),
    median_income = as_number(raw[[income_column]]),
    median_home_value = as_number(raw[[home_column]]),
    median_rent = as_number(raw[["Median Gross Rent"]]),
    median_rent_pct = as_number(raw[[rent_share_column]]),
    unemployment_pct = ifelse(is.finite(labor_force) & labor_force > 0,
                              100 * unemployed / labor_force, NA_real_),
    poverty_pct = ifelse(is.finite(poverty_total) & poverty_total > 0,
                         100 * poverty_below / poverty_total, NA_real_),
    commute_minutes = as_number(raw[["Average Commute to Work (In Min)"]])
  )
}
acs_all <- data.frame()
for (year in 2019:2024) {
  year_data <- load_acs_year(year)
  acs_all <- bind_rows(acs_all, year_data)
}
stopifnot(nrow(acs_all) == 300L)

# The paired inflow and outflow files are two representations of the SAME
# migration routes, so do NOT add the two files together.
# Use incoming routes as one canonical list, and validate the paired outgoing
# file's origin/destination keys and N1/N2 counts for parity.
load_irs_period <- function(period) {
  in_csv <- read_source(paste0("state_inflow_clean_", period, ".csv"))
  out_csv <- read_source(paste0("state_outflow_clean_", period, ".csv"))
  required <- c("year", "origin_state", "destination_state", "origin_fips",
                "destination_fips", "tax_returns", "exemptions", "agi_thousands")
  stopifnot(all(required %in% names(in_csv)), all(required %in% names(out_csv)))
  for (d in list(in_csv, out_csv)) {
    if (anyDuplicated(paste(d$origin_fips, d$destination_fips)))
      stop("Duplicate origin-destination pairs in IRS file")
  }
  a_key <- paste(canon_fips(in_csv$origin_fips), canon_fips(in_csv$destination_fips))
  b_key <- paste(canon_fips(out_csv$origin_fips), canon_fips(out_csv$destination_fips))
  if (!setequal(a_key, b_key)) stop("Inflow and outflow route pairs differ in ", period)
  b <- out_csv[match(a_key, b_key), , drop = FALSE]
  for (field in c("tax_returns", "exemptions")) {
    x <- as_number(in_csv[[field]])
    y <- as_number(b[[field]])
    if (!isTRUE(all.equal(x, y, check.attributes = FALSE)))
      stop("IRS inflow/outflow counts disagree for ", field, " in ", period)
  }
  # Two historic pairs have AGI differences of $1,000 between source views.
  # Canonical inflow AGI is retained without double-counting either view.
  flows <- data.frame(
    period = period,
    period_label = unname(period_labels[[period]]),
    origin_state = in_csv$origin_state,
    destination_state = in_csv$destination_state,
    origin_fips = canon_fips(in_csv$origin_fips),
    destination_fips = canon_fips(in_csv$destination_fips),
    returns = as_number(in_csv$tax_returns),
    individuals = as_number(in_csv$exemptions),
    agi_thousands = as_number(in_csv$agi_thousands),
    stringsAsFactors = FALSE
  )
  # D.C. is in the source flow universe; it is a legitimate migration partner.
  # Limit map/summary metrics to the 50 states using pop$fips.
  all_geographies <- union(flows$origin_fips, flows$destination_fips)
  stopifnot(length(all_geographies) == 51L)
  # Lesson 7: summarize each state's incoming and outgoing routes separately.
  incoming <- flows %>%
    group_by(destination_fips) %>%
    summarize(
      inflow_returns = sum(returns, na.rm = TRUE),
      inflow_people = sum(individuals, na.rm = TRUE),
      missing_in = 50L - sum(is.finite(individuals))
    )
  outgoing <- flows %>%
    group_by(origin_fips) %>%
    summarize(
      outflow_returns = sum(returns, na.rm = TRUE),
      outflow_people = sum(individuals, na.rm = TRUE),
      missing_out = 50L - sum(is.finite(individuals))
    )
  # Lesson 10: keep the 50 states, with D.C. still counted as a route partner.
  summary <- pop %>%
    select(fips, population_2020) %>%
    left_join(incoming, by = c("fips" = "destination_fips")) %>%
    left_join(outgoing, by = c("fips" = "origin_fips"))
  
  # A completely absent state has zero reported totals and 50 missing routes.
  summary$inflow_returns[is.na(summary$inflow_returns)] <- 0
  summary$outflow_returns[is.na(summary$outflow_returns)] <- 0
  summary$inflow_people[is.na(summary$inflow_people)] <- 0
  summary$outflow_people[is.na(summary$outflow_people)] <- 0
  summary$missing_in[is.na(summary$missing_in)] <- 50L
  summary$missing_out[is.na(summary$missing_out)] <- 50L
  summary$period <- period
  # Lesson 6: calculate new variables with mutate().
  summary <- summary %>%
    mutate(
      net_people = inflow_people - outflow_people,
      net_rate = 1000 * net_people / population_2020,
      in_rate = 1000 * inflow_people / population_2020,
      out_rate = 1000 * outflow_people / population_2020,
      incomplete = (missing_in + missing_out) > 0L
    ) %>%
    select(fips, period, inflow_returns, outflow_returns,
           inflow_people, outflow_people, net_people, net_rate,
           in_rate, out_rate, missing_in, missing_out, incomplete) %>%
    as.data.frame()
  list(totals = summary, flows = flows)
}
periods <- c("1920", "2021", "2122", "2223")
irs_totals <- data.frame()
irs_flows <- data.frame()
for (period in periods) {
  period_data <- load_irs_period(period)
  irs_totals <- bind_rows(irs_totals, period_data$totals)
  irs_flows <- bind_rows(irs_flows, period_data$flows)
}
stopifnot(nrow(irs_totals) == 200L)

# State abbreviations, FIPS, and all original state names remain unmodified.

# --------------- UI HELPERS / METRIC DEFINITIONS ---------------
metric_labels <- c(
  net_rate = "Net migration proxy per 1,000",
  in_rate = "Incoming migration proxy per 1,000",
  out_rate = "Outgoing migration proxy per 1,000",
  net_people = "Net migration proxy (N2)",
  inflow_people = "Incoming migration proxy (N2)",
  outflow_people = "Outgoing migration proxy (N2)",
  population_growth_pct = "Population growth, 2020–2025 (%)",
  acs_population = "ACS total population (people)",
  median_income = "Median household income ($)",
  median_home_value = "Median home value ($)",
  home_income_ratio = "Median home value / household income",
  median_rent = "Median gross rent ($/month)",
  median_rent_pct = "ACS median gross rent / household income (%)",
  unemployment_pct = "ACS unemployment rate (%)",
  poverty_pct = "ACS poverty rate (%)",
  commute_minutes = "Average commute (minutes)"
)
pretty_choices <- function(keys) {
  stats::setNames(keys, unname(metric_labels[keys]))
}
x_choices <- pretty_choices(c(
  "median_income", "median_home_value", "home_income_ratio",
  "median_rent", "median_rent_pct", "unemployment_pct", "poverty_pct",
  "commute_minutes"
))
y_choices <- pretty_choices(c(
  "net_rate", "in_rate", "out_rate", "net_people", "population_growth_pct"
))
map_choices <- pretty_choices(c(
  "net_rate", "net_people", "in_rate", "out_rate",
  "population_growth_pct", "median_income", "median_home_value",
  "unemployment_pct", "poverty_pct", "median_rent_pct"
))
trend_choices <- c(
  "Population change: ACS 2019–2024 (2020 baseline)" = "pop_index",
  "Total population: ACS 2019–2024" = "acs_population",
  "Population change: 2020–2025 endpoints" = "pop_endpoints",
  pretty_choices(c("net_people", "net_rate", "inflow_people", "outflow_people",
                   "median_income", "median_home_value", "median_rent",
                   "median_rent_pct", "unemployment_pct", "poverty_pct",
                   "commute_minutes"))
)
state_choices <- stats::setNames(pop$abbr, pop$state)

add_metric <- function(d, key) {
  if (key == "home_income_ratio") {
    d$value <- d$median_home_value / d$median_income
  } else {
    d$value <- d[[key]]
  }
  d$value[!is.finite(d$value)] <- NA_real_
  d
}
fmt_num <- function(x, digits = 0) {
  ifelse(is.na(x), "N/A", formatC(x, format = "f", digits = digits,
                                  big.mark = ","))
}
fmt_signed <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", paste0(ifelse(x > 0, "+", ""), fmt_num(x, digits)))
}
fmt_metric <- function(x, key) {
  if (is.na(x)) return("N/A")
  if (key %in% c("median_income", "median_home_value", "median_rent"))
    return(paste0("$", fmt_num(x)))
  if (key %in% c("population_growth_pct", "median_rent_pct",
                 "unemployment_pct", "poverty_pct"))
    return(paste0(fmt_num(x, 2), "%"))
  if (key %in% c("net_people", "inflow_people", "outflow_people"))
    return(fmt_signed(x, 0))
  fmt_signed(x, 2)
}

ui_card <- function(title, ..., subtitle = NULL) {
  tags$section(class = "atlas-card", h2(title),
               if (!is.null(subtitle)) p(class = "atlas-sub", subtitle), ...)
}
stat_box <- function(label, value, detail = "") {
  div(class = "stat-card", div(class = "stat-label", label),
      div(class = "stat-value", value), div(class = "stat-sub", detail))
}

# Serve the original portrait files from the project. Never delete the originals.
portrait_output <- function(filename, name, alternate = filename) {
  renderImage({
    folders <- c(repo_dir, file.path(repo_dir, "www"), dirname(repo_dir))
    paths <- c(file.path(folders, filename), file.path(folders, alternate))
    path <- paths[file.exists(paths)][1]
    validate(need(!is.na(path), paste("Portrait file missing:", filename)))
    list(src = normalizePath(path), alt = name,
         contentType = if (grepl("[.]png$", path, ignore.case = TRUE)) "image/png" else "image/jpeg")
  }, deleteFile = FALSE)
}

team_card <- function(photo_id, name, discipline, bio, linkedin) {
  div(class = "team-card",
      div(class = "team-photo-shell",
          imageOutput(photo_id, width = "100%", height = "100%")
      ),
      h3(class = "team-name", name),
      div(class = "team-discipline", discipline),
      p(class = "team-bio", bio),
      tags$a(class = "team-link", href = linkedin,
             target = "_blank", rel = "noopener noreferrer",
             "View LinkedIn profile")
  )
}

# VISUAL SETTINGS: research-atlas direction.
# Georgia is used only for the existing project name; data use Arial/sans-serif.
# Open sections and 8/16/24/32px spacing organize the analytical pages.
# The map header, selected-state strip, and sidebar groups are ordinary Shiny tags.
# Edit CSS here; keep calculations in the data and server sections unchanged.
app_css <- "
/* Skip link becomes visible when reached by keyboard. */
.atlas-skip-link {position:absolute;left:12px;top:-100px;z-index:2000;padding:10px 16px;background:white;color:#173f7a;border:2px solid #2459aa;}
.atlas-skip-link:focus {top:12px;}
/* Edit these tokens to change the visual system in one place. */
:root {
  --ink: #182c3d;
  --muted: #526372;
  --accent: #2459aa;
  --accent-dark: #173f7a;
  --line: #d5dde4;
  --soft: #f3f6f8;
  --warning: #875019;
  --space-1: 4px;
  --space-2: 8px;
  --space-3: 12px;
  --space-4: 16px;
  --space-6: 24px;
  --space-8: 32px;
  --space: var(--space-2);
  --section-space: var(--space-6);
}
body {background:#fff;color:var(--ink);font-family:Arial,Helvetica,sans-serif;font-size:14px;line-height:1.55;}
.atlas-shell {display:flex;min-height:100vh;margin:0 -15px;align-items:stretch;}
.atlas-sidebar {width:220px;min-width:220px;padding:24px 16px;background:var(--soft);border-right:1px solid var(--line);position:sticky;top:0;height:100vh;overflow-y:auto;}
.atlas-brand {font-family:Georgia,serif;font-size:22px;line-height:1.25;font-weight:700;padding:0 8px 24px;color:var(--ink);}
.atlas-brand small {font-family:Arial,Helvetica,sans-serif;display:block;font-size:12px;font-weight:400;color:var(--muted);margin-top:8px;}
.atlas-sidebar .action-button {width:100%;min-height:42px;text-align:left;border:0;border-left:3px solid transparent;border-radius:0;background:transparent;color:var(--muted);padding:9px 10px;margin:2px 0;font-size:14px;font-weight:400;box-shadow:none;}
.atlas-sidebar .action-button:hover {background:#e4ebf2;color:var(--ink);}
.atlas-sidebar .action-button.atlas-active {background:#e0eafa;color:var(--accent-dark);border-left-color:var(--accent);font-weight:700;}
.atlas-sidebar hr {border-color:var(--line);margin:24px 0 20px;}
.atlas-sidebar label {color:var(--ink);font-size:12px;font-weight:700;}
.atlas-sidebar .form-group {margin-bottom:16px;}
.atlas-side-note {font-size:12px;color:var(--muted);line-height:1.6;}
/* Limit the working canvas so charts and captions remain visually connected. */
.atlas-main {flex:1;min-width:0;max-width:1320px;padding:32px 32px 48px;}
.atlas-nav-label {font-size:11px;font-weight:700;letter-spacing:.6px;text-transform:uppercase;color:var(--muted);margin:20px 10px 6px;}
.atlas-brand + .atlas-nav-label {margin-top:0;}
.atlas-main h1 {border-bottom:2px solid var(--ink);padding-bottom:12px;}
.atlas-card.atlas-map-section {border-top:0;padding-top:0;}
.atlas-map-header {display:flex;gap:24px;align-items:flex-end;justify-content:space-between;margin:0 0 12px;}
.atlas-map-heading {flex:1;min-width:0;}
.atlas-map-heading .atlas-sub {margin-bottom:0;max-width:58ch;}
.atlas-map-header .atlas-map-controls {flex:0 1 340px;min-width:240px;margin:0;}
.atlas-map-header .form-group {margin-bottom:0;width:100%!important;}
.atlas-selection {border-top:1px solid var(--line);border-bottom:1px solid var(--line);background:var(--soft);padding:12px 16px;font-size:13px;margin-top:12px;}
.atlas-selection-label {display:block;color:var(--muted);font-size:11px;font-weight:700;letter-spacing:.5px;text-transform:uppercase;margin-bottom:4px;}
.atlas-summary-heading {font-size:13px!important;color:var(--muted);margin:0 0 12px!important;}
.atlas-map-section .atlas-period-note {border-bottom:1px solid var(--line);padding-bottom:12px;}
.atlas-main .atlas-control-row {padding:12px 0;border-bottom:1px solid var(--line);}
.atlas-main .atlas-card p:not(.atlas-sub),.atlas-card ul {max-width:90ch;}

.atlas-main h1 {font-size:26px;line-height:1.25;font-weight:700;letter-spacing:-.4px;margin:0 0 8px;}
.atlas-main h2,.atlas-main h3 {font-size:17px;line-height:1.4;font-weight:700;margin:0 0 8px;}
.atlas-lede {color:var(--muted);max-width:85ch;font-size:14px;margin:0 0 24px;}
.atlas-sub {color:var(--muted);font-size:13px;margin:0 0 16px;}
/* Sections share the page surface. A single rule marks each new question. */
.atlas-card {min-width:0;background:#fff;border:0;border-top:1px solid var(--line);border-radius:0;box-shadow:none;padding:20px 0 8px;margin:0 0 24px;}
.kpi-grid {display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:0;margin:0 0 24px;border-top:1px solid var(--line);border-bottom:1px solid var(--line);}
.stat-card {min-width:0;min-height:0;border:0;border-right:1px solid var(--line);border-radius:0;box-shadow:none;background:#fff;padding:16px;margin:0;}
.stat-card:first-child {padding-left:0;}
.stat-card:last-child {border-right:0;}
.stat-label {font-size:12px;line-height:1.45;color:var(--muted);font-weight:400;}
.stat-value {font-size:23px;line-height:1.25;font-weight:700;color:var(--ink);margin:6px 0;font-variant-numeric:tabular-nums;}
.stat-sub {font-size:12px;color:var(--muted);}
.two-panels,.two-even {display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:32px;align-items:start;}
.two-panels > *,.two-even > * {min-width:0;}
.atlas-control-row {display:flex;gap:16px;align-items:flex-end;flex-wrap:wrap;margin:8px 0;}
.atlas-control-row .form-group {min-width:180px;flex:1;}
.shiny-input-container {max-width:100%;}
label {font-size:13px;font-weight:700;}
.form-control,.selectize-input {border:1px solid #8798a7;border-radius:3px;box-shadow:none;color:var(--ink);background:#fff;font-size:14px;}
.selectize-input {min-height:38px;padding:8px 10px;}
.selectize-dropdown {border-color:#8798a7;border-radius:3px;font-size:14px;}
.selectize-dropdown .active {background:#e0eafa;color:var(--accent-dark);}
.selectize-dropdown .selected {font-weight:700;}
.btn-default,.btn-atlas {background:#fff;color:var(--accent-dark);border:1px solid #8798a7;border-radius:3px;padding:8px 12px;min-height:38px;white-space:normal;box-shadow:none;font-size:13px;font-weight:700;}
.btn-default:hover,.btn-atlas:hover {background:#eaf0f8;border-color:var(--accent);color:var(--accent-dark);}
.btn-default:active,.btn-atlas:active {background:#d9e6f8;box-shadow:none;}
/* Visible keyboard focus, independent of selected or hover states. */
a:focus-visible,button:focus-visible,input:focus-visible,select:focus-visible,
.atlas-sidebar .action-button:focus-visible,.selectize-input.focus {
  outline:3px solid var(--accent);outline-offset:3px;box-shadow:none;
}
.btn:disabled,.btn.disabled,.btn[aria-disabled='true'],input:disabled,select:disabled,
.selectize-control.disabled .selectize-input {background:#edf0f3!important;color:#596775!important;border-color:#bac4ce!important;opacity:1;cursor:not-allowed;box-shadow:none;}
.atlas-alert {font-size:13px;line-height:1.6;background:var(--soft);border:0;border-left:3px solid #8798a7;border-radius:0;padding:12px 16px;color:var(--muted);margin-top:16px;}
.atlas-badge {font-size:12px;color:var(--muted);display:inline-block;margin-top:8px;}
.atlas-period-note {font-size:13px;line-height:1.5;color:var(--ink);margin:8px 0 16px;overflow-wrap:anywhere;}
.atlas-period-note strong {font-weight:700;}
.atlas-definition {display:block;font-size:12px;color:var(--muted);margin-top:4px;max-width:95ch;}
.atlas-warning {display:block;font-size:12px;color:var(--warning);margin-top:6px;}
.atlas-map-controls {max-width:380px;margin:12px 0 0;}
.atlas-map-note {font-size:12px;color:var(--muted);margin:8px 0;}
.js-plotly-plot .plotly .modebar {opacity:1;}
/* Tables use the same rules, type, and numerical rhythm as the charts. */
.atlas-main .dataTables_wrapper {width:100%;overflow-x:auto;font-size:13px;padding:4px 0 12px;}
.atlas-main table.dataTable {width:100%!important;border-collapse:collapse!important;border-bottom:1px solid var(--line)!important;font-variant-numeric:tabular-nums;}
.atlas-main table.dataTable thead th {background:var(--soft);color:var(--ink);font-size:12px;font-weight:700;border-bottom:1px solid #8798a7;padding:10px 12px;vertical-align:bottom;}
.atlas-main table.dataTable tbody td {padding:10px 12px;border-top:1px solid #e4e9ed;vertical-align:top;}
.atlas-main table.dataTable tbody tr {background:#fff;}
.atlas-main table.dataTable tbody tr:nth-child(even) {background:#f7f9fb;}
.atlas-main table.dataTable tbody tr:hover {background:#eaf0f8;}
.atlas-main table.dataTable th:not(:first-child),.atlas-main table.dataTable td:not(:first-child) {text-align:right;}
#route_table table.dataTable th:last-child,#route_table table.dataTable td:last-child {text-align:left;}
.atlas-main .dataTables_filter input,.atlas-main .dataTables_length select {border:1px solid #8798a7;border-radius:3px;padding:6px;background:#fff;color:var(--ink);}
.atlas-main .dataTables_info,.atlas-main .dataTables_length,.atlas-main .dataTables_filter {color:var(--muted);font-size:12px;margin-bottom:12px;}
.atlas-main .dataTables_paginate .paginate_button {border-radius:3px!important;background:#fff!important;color:var(--ink)!important;}
.atlas-main .dataTables_paginate .paginate_button.current {border-color:var(--accent)!important;background:#e0eafa!important;color:var(--accent-dark)!important;font-weight:700;}
.atlas-main .dataTables_paginate .paginate_button:hover {border-color:var(--accent)!important;background:#eaf0f8!important;color:var(--ink)!important;}
.atlas-main .dataTables_paginate .paginate_button.disabled {color:#596775!important;background:#edf0f3!important;border-color:transparent!important;cursor:not-allowed;}
.about-eyebrow {font-size:12px;font-weight:700;color:var(--muted);margin:0 0 8px;}
.team-grid {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:32px;margin:24px 0 32px;}
.team-card {display:flex;flex-direction:column;align-items:flex-start;padding:20px 0 0;border-top:1px solid var(--line);background:#fff;min-height:0;}
.team-photo-shell {width:112px;height:112px;border-radius:50%;overflow:hidden;background:var(--soft);margin:0 0 16px;}
.team-photo-shell img {width:100%;height:100%;display:block;object-fit:cover;object-position:center;}
.team-card .team-name {font-size:18px;margin:0 0 4px;}
.team-discipline {font-size:12px;color:var(--muted);font-weight:700;margin-bottom:12px;}
.team-bio {font-size:14px;color:var(--muted);line-height:1.6;}
.team-link {margin-top:auto;color:var(--accent-dark);font-size:13px;font-weight:700;text-decoration:underline;text-underline-offset:3px;padding:8px 0;}
.team-link:hover {color:var(--accent);text-decoration-thickness:2px;}
.atlas-card li {margin-bottom:8px;}
/* Shiny accepts the pixel fallback; the browser handles responsive CSS. */
#state_map {height:470px!important;height:clamp(360px, 43vw, 560px)!important;}
@media(max-width:1150px) {
  .two-even,.two-panels {grid-template-columns:minmax(0,1fr);gap:0;}
  .kpi-grid {grid-template-columns:repeat(2,minmax(0,1fr));}
  .stat-card:nth-child(2) {border-right:0;}
  .stat-card:nth-child(n+3) {border-top:1px solid var(--line);}
  .stat-card:nth-child(3) {padding-left:0;}
  .team-grid {grid-template-columns:repeat(2,minmax(0,1fr));gap:24px;}
}
@media(max-width:760px) {
  .atlas-shell {display:block;}
  .atlas-sidebar {position:relative;width:100%;min-width:0;height:auto;border-right:0;border-bottom:1px solid var(--line);padding:16px;}
  .atlas-brand {font-size:20px;padding:0 0 12px;}
  .atlas-brand br {display:none;}
  .atlas-brand small {margin-top:4px;}
  .atlas-sidebar .action-button {display:inline-block;width:auto;min-height:44px;margin:2px;padding:10px;}
  .atlas-sidebar hr {margin:16px 0;}
  .atlas-sidebar .form-group {display:inline-block;vertical-align:top;width:calc(50% - 10px);margin-right:8px;}
  .atlas-main {padding:24px 16px 32px;}
  .atlas-main h1 {font-size:23px;}
  .atlas-map-header {display:block;}
  .atlas-map-header .atlas-map-controls {min-width:0;max-width:none;margin-top:16px;}
  .atlas-nav-label {margin:12px 2px 4px;}
  .atlas-main .atlas-control-row {padding:8px 0;}
  .atlas-control-row {gap:12px;}
  .atlas-control-row .form-group {min-width:100%;margin-bottom:8px;}
  .atlas-lede {margin-bottom:20px;}
  #state_map {height:420px!important;}
  .atlas-sidebar .atlas-nav-label {display:none;}
  .atlas-sidebar .action-button {font-size:12px;padding:8px;}
  .atlas-sidebar .atlas-brand {padding-bottom:8px;}
  .atlas-sidebar {padding:12px 16px;}
  .team-grid {grid-template-columns:1fr;}
  .stat-value {font-size:21px;}
}
.atlas-main {font-variant-numeric:tabular-nums;}
.atlas-page-header .atlas-lede {margin-bottom:var(--space-3);}
.atlas-reporting-controls {display:grid;grid-template-columns:1fr 1fr 1.5fr;gap:var(--space-4);align-items:end;padding:var(--space-3) 0;margin-bottom:var(--space-2);border-bottom:1px solid var(--line);}
.atlas-reporting-controls .form-group {width:100%;margin:0;min-width:0;}
.atlas-reporting-controls > div {min-width:0;}
.atlas-main .atlas-card {margin-bottom:var(--section-space);padding-top:var(--space-4);}
.atlas-main .atlas-map-section {padding-top:0;}
.atlas-map-section .atlas-period-note {border:0;padding:0;margin:var(--space-2) 0;}
.atlas-definition,.atlas-warning,.atlas-map-note,.atlas-sub {font-size:12px;line-height:1.5;}
.atlas-map-note {margin:var(--space-2) 0;}
.atlas-missing-key::before {content:'';display:inline-block;width:12px;height:12px;background:#c5cbd1;border:1px solid #526372;margin-right:var(--space-2);vertical-align:middle;}
@media(max-width:760px) {
  .atlas-reporting-controls {grid-template-columns:1fr 1fr;gap:var(--space-3);}
  .atlas-reporting-controls > .shiny-panel-conditional {grid-column:1 / -1;}
  .atlas-main {padding-top:var(--space-4);}
}
@media(max-width:380px) {.atlas-reporting-controls {grid-template-columns:1fr;}}
.factor-key {display:flex;flex-wrap:wrap;align-items:center;gap:8px 20px;padding:10px 0;border-block:1px solid var(--line);font-size:13px;}
.factor-key span {display:inline-flex;align-items:center;gap:8px;}
.factor-key i {display:inline-block;flex:none;}
.key-dot {width:11px;height:11px;background:#2459aa;border-radius:50%;}
.key-partial {width:10px;height:10px;border:2px solid #2459aa;transform:rotate(45deg);}
.key-fit {width:24px;border-top:3px dashed #b46228;}
.atlas-file-list {padding-left:20px;overflow-wrap:anywhere;}
.atlas-main details summary {cursor:pointer;font-weight:600;padding:5px 0;}
@media(prefers-reduced-motion:reduce) {*,*::before,*::after {scroll-behavior:auto!important;transition:none!important;}}
"

atlas_definition <- paste(
  "One IRS exemption represents one person counted on a tax return.",
  "These counts estimate how many people moved between states.",
  "Net means incoming minus outgoing. Rates per 1,000 use each state's 2020 population.")
exemption_example <- paste(
  "Two parents and their two children move from Virginia to South Carolina.",
  "All four are listed on one tax return.",
  "For this move, the IRS migration data count 1 tax return and 4 exemptions, one for each person.")

partial_irs_heading <- "What does a partial IRS total mean?"
partial_irs_text <- paste(
  "At least one route into or out of the state has no reported count. The total adds up the routes we have.",
  "For example, if the Virginia-to-South Carolina count is missing, we don’t know how many people made that move. Missing does not mean zero.")

# Link only the original CSVs the app loads. Use the same 2024 filename rule.
source_groups <- list(
  "Population estimates" = "state_population_growth_clean.csv",
  "ACS surveys" = vapply(2019:2024, acs_filename, character(1)),
  "IRS inflows" = paste0("state_inflow_clean_", names(period_labels), ".csv"),
  "IRS outflows" = paste0("state_outflow_clean_", names(period_labels), ".csv")
)
source_files <- unname(unlist(source_groups))
csv_link_id <- function(filename) paste0("source_csv_", match(filename, source_files))
source_links <- function(files) {
  tags$ul(class = "atlas-file-list", lapply(files, function(filename) {
    tags$li(downloadLink(csv_link_id(filename), filename))
  }))
}
atlas_irs_keys <- c("net_rate", "in_rate", "out_rate", "net_people",
                    "inflow_people", "outflow_people")


# Shared chart styling. Colors and typography change here; data do not.
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

# --------------- SHINY SERVER: REACTIVE DATA AND PLOTS ---------------
server <- function(input, output, session) {
  output$photo_chacon <- portrait_output("Head Portrait - Lucas Chacon.png", "Lucas Chacon",
                                         "Head Portrait - Lucas Chacon (1).png")
  output$photo_kim <- portrait_output("SPIA Portraits 6-12-26-4917.jpg", "Lucas Kim")
  output$photo_salazar <- portrait_output("Diana_2.jpg", "Diana Salazar", "Diana_2.jpeg")
  
  # Each download copies the original file, retaining its rows and headings.
  for (filename in source_files) {
    local({
      csv_name <- filename
      output[[csv_link_id(csv_name)]] <- downloadHandler(
        filename = csv_name, contentType = "text/csv",
        content = function(path) {
          if (!file.copy(file.path(repo_dir, csv_name), path, overwrite = TRUE))
            stop("Could not download ", csv_name)
        })
    })
  }
  
  observeEvent(input$go_overview, {
    updateTabsetPanel(session, "page", selected = "overview")
  })
  observeEvent(input$go_explore, {
    updateTabsetPanel(session, "page", selected = "explore")
  })
  observeEvent(input$go_routes, {
    updateTabsetPanel(session, "page", selected = "routes")
  })
  observeEvent(input$go_compare, {
    updateTabsetPanel(session, "page", selected = "compare")
  })
  observeEvent(input$go_trends, {
    updateTabsetPanel(session, "page", selected = "trends")
  })
  observeEvent(input$go_about, {
    updateTabsetPanel(session, "page", selected = "about")
  })
  observeEvent(input$go_methods, {
    updateTabsetPanel(session, "page", selected = "methods")
  })
  
  selected_state <- reactiveVal("SC")
  observeEvent(input$route_state, {
    if (input$route_state %in% pop$abbr) selected_state(input$route_state)
  }, ignoreInit = TRUE)
  
  # Lessons 6 and 10: filter the years, then join tables by state FIPS.
  view_data <- reactive({
    selected_acs <- filter(acs_all, acs_year == as.integer(input$acs_year))
    selected_irs <- filter(irs_totals, period == input$irs_period)
    d <- pop %>%
      inner_join(selected_acs, by = "fips") %>%
      inner_join(selected_irs, by = "fips") %>%
      arrange(state)
    validate(need(nrow(d) == 50, "Expected 50 states in the merged dataset."))
    d
  })
  
  output$overview_kpis <- renderUI({
    d <- view_data()
    highest <- d[which.max(d$net_rate), ]
    lowest <- d[which.min(d$net_rate), ]
    fastest <- d[which.max(d$population_growth_pct), ]
    div(class = "kpi-grid",
        stat_box("States with reported net gains",
                 paste0(sum(d$net_people > 0), " / 50"),
                 unname(period_labels[[input$irs_period]])),
        stat_box("Largest reported net gain / 1,000", fmt_signed(highest$net_rate), highest$state),
        stat_box("Largest reported net loss / 1,000", fmt_signed(lowest$net_rate), lowest$state),
        stat_box("Fastest population growth", paste0(fmt_num(fastest$population_growth_pct, 2), "%"),
                 paste(fastest$state, "· 2020–2025"))
    )
  })
  
  # Genuine state-border choropleth; no manually positioned square tiles.
  # Blue ramp matches the reference; net migration uses a centered two-sided scale.
  output$state_map <- renderPlotly({
    d <- add_metric(view_data(), input$map_metric)
    req(nrow(d) == 50L, input$map_metric)
    
    metric_label <- unname(metric_labels[[input$map_metric]])
    hover_values <- character(nrow(d))
    for (i in seq_len(nrow(d))) {
      hover_values[i] <- fmt_metric(d$value[i], input$map_metric)
    }
    d$map_hover <- paste0(
      "<b>", d$state, " (", d$abbr, ")</b>",
      "<br>", metric_label, ": <b>",
      hover_values,
      "</b>",
      "<br>Reported net exemptions: ", fmt_signed(d$net_people, 0),
      ifelse(d$incomplete, " (partial)", ""),
      "<br>2020–2025 population growth: ",
      fmt_num(d$population_growth_pct, 2), "%"
    )
    
    # A diverging palette is appropriate when positive/negative direction matters.
    is_net <- input$map_metric %in% c("net_rate", "net_people")
    if (is_net) {
      spread <- max(abs(d$value), na.rm = TRUE)
      if (!is.finite(spread) || spread <= 0) spread <- 1
      scale_min <- -spread
      scale_max <- spread
      map_colors <- list(
        c(0, "#b46228"), c(0.5, "#f0f4f8"), c(1, "#2459aa")
      )
    } else {
      values <- d$value[is.finite(d$value)]
      scale_min <- if (length(values)) min(values) else 0
      scale_max <- if (length(values)) max(values) else 1
      if (scale_min == scale_max) scale_max <- scale_min + 1
      map_colors <- list(
        c(0, "#e6f0ff"),
        c(0.25, "#b8d1fa"),
        c(0.5, "#83b0f1"),
        c(0.75, "#5287d7"),
        c(1, "#28559f")
      )
    }
    
    legend_title <- switch(
      input$map_metric,
      "net_rate" = "Net per 1k",
      "population_growth_pct" = "Growth (%)",
      "net_people" = "Net N2", "in_rate" = "In per 1k",
      "out_rate" = "Out per 1k", "median_home_value" = "Home ($)",
      "poverty_pct" = "Poverty (%)", "median_rent_pct" = "Rent (%)",
      "median_income" = "Income ($)",
      "unemployment_pct" = "Unemployment (%)",
      "Value"
    )
    
    missing_states <- filter(d, !is.finite(value))
    map <- plot_ly(
      data = filter(d, is.finite(value)),
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~abbr,
      z = ~value,
      text = ~map_hover,
      customdata = ~abbr,
      key = ~abbr,
      hovertemplate = "%{text}<extra></extra>",
      colorscale = map_colors,
      zmin = scale_min,
      zmax = scale_max,
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
    if (nrow(missing_states) > 0L) {
      map <- add_trace(map, data = missing_states, inherit = FALSE,
                       type = "choropleth", locationmode = "USA-states", locations = ~abbr,
                       z = rep(0, nrow(missing_states)), zmin = 0, zmax = 1,
                       colorscale = list(c(0, "#c5cbd1"), c(1, "#c5cbd1")),
                       showscale = FALSE, showlegend = FALSE,
                       text = ~paste0(state, "<br>", metric_label, ": No data"),
                       customdata = ~abbr, key = ~abbr,
                       hovertemplate = "%{text}<extra></extra>",
                       marker = list(line = list(color = "#ffffff", width = 0.85)))
    }
    map
  })
  
  output$map_source_note <- renderUI({
    n <- sum(view_data()$incomplete)
    div(class = "atlas-map-note",
        tags$span(class = "atlas-missing-key", "Gray = no data; zero is a measured value."),
        tags$span(" Color limits adjust to the selected data; compare legend values, not shades, across years."),
        if (input$map_metric %in% atlas_irs_keys && n > 0)
          paste(n, "states have partial IRS totals; hover over states for flags."))
  })
  
  # The existing selected-state summary and route selector keep working.
  # Choropleth click payloads can identify the state by location or customdata.
  observeEvent(event_data("plotly_click", source = "state_choropleth"), {
    e <- event_data("plotly_click", source = "state_choropleth")
    if (is.null(e) || nrow(e) == 0L) return()
    
    code <- NULL
    for (field in c("location", "customdata", "key")) {
      if (field %in% names(e)) {
        candidate <- as.character(e[[field]][1])
        if (length(candidate) == 1L && !is.na(candidate) &&
            candidate %in% pop$abbr) {
          code <- candidate
          break
        }
      }
    }
    if (!is.null(code)) {
      selected_state(code)
      updateSelectInput(session, "route_state", selected = code)
    }
  })
  
  output$selected_details <- renderUI({
    d <- view_data()
    s <- filter(d, abbr == selected_state())
    if (!nrow(s)) return(NULL)
    div(class = "atlas-selection",
        tags$span(class = "atlas-selection-label", "Selected state"),
        tags$b(paste0(s$state, " (", s$abbr, ")")), tags$br(),
        "Reported net IRS exemptions: ", tags$b(fmt_signed(s$net_people, 0)), " · ",
        "2020–25 growth: ", tags$b(paste0(fmt_signed(s$population_growth_pct, 2), "%")),
        if (s$incomplete) tags$span(" · Incomplete route data: ",
                                    tags$b(s$missing_in + s$missing_out), " suppressed/missing inbound or outbound records")
    )
  })
  
  output$net_rank_plot <- renderPlot({
    d <- view_data()
    d <- arrange(d, desc(net_rate))
    take <- bind_rows(head(d, 6), tail(d, 6))
    take$label <- paste0(take$state, ifelse(take$incomplete, " *", ""))
    take$kind <- ifelse(take$net_rate >= 0, "Net gain", "Net loss")
    ggplot(take, aes(reorder(label, net_rate), net_rate, fill = kind)) +
      geom_bar(stat = "identity", width = .7) + coord_flip() +
      scale_fill_manual(values = c("Net gain" = "#2459aa", "Net loss" = "#b46228")) +
      labs(x = NULL, y = metric_labels[["net_rate"]], caption = "* Partial IRS route data") +
      atlas_chart_theme() +
      theme(legend.position = "none", panel.grid.major.y = element_blank())
  })
  
  scatter_data <- reactive({
    d <- add_metric(view_data(), input$x_metric)
    names(d)[names(d) == "value"] <- "x"
    d <- add_metric(d, input$y_metric)
    names(d)[names(d) == "value"] <- "y"
    d <- filter(d, is.finite(x), is.finite(y))
    d$tip <- paste0(d$state, "<br>", metric_labels[[input$x_metric]],
                    ": ", round(d$x, 2), "<br>", metric_labels[[input$y_metric]],
                    ": ", round(d$y, 2))
    d
  })
  
  output$scatter_plot <- renderPlotly({
    d <- scatter_data()
    validate(need(nrow(d) >= 3 && sd(d$x) > 0 && sd(d$y) > 0,
                  "Not enough variation for a scatterplot."))
    partial <- (input$y_metric %in% atlas_irs_keys) & d$incomplete
    d$tip <- paste0(d$tip, "<br>", atlas_period(input$x_metric), "<br>", atlas_period(input$y_metric),
                    if (input$y_metric %in% atlas_irs_keys)
                      ifelse(partial, paste0("<br><b>Partial IRS totals:</b> ", d$missing_in,
                                             " incoming and ", d$missing_out, " outgoing routes missing/unreported"),
                             "<br>All 50 inbound and 50 outbound N2 routes reported") else "")
    fit <- lm(y ~ x, data = d)
    line_d <- data.frame(x = range(d$x))
    line_d$y <- predict(fit, newdata = line_d)
    p <- plot_ly(d, x = ~x, y = ~y, text = ~tip, customdata = ~abbr,
                 type = "scatter", mode = "markers", hoverinfo = "text",
                 marker = list(color = "#2459aa",
                               symbol = ifelse(partial, "diamond-open", "circle"), size = 11,
                               line = list(width = 1.5)), source = "scatter", showlegend = FALSE)
    p <- add_lines(p, data = line_d, x = ~x, y = ~y, inherit = FALSE,
                   hoverinfo = "skip", showlegend = FALSE,
                   line = list(color = "#b46228", dash = "dash", width = 3))
    layout(p, xaxis = list(title = metric_labels[[input$x_metric]], automargin = TRUE,
                           gridcolor = "#e4e9ed", zeroline = FALSE),
           yaxis = list(title = metric_labels[[input$y_metric]], automargin = TRUE,
                        gridcolor = "#e4e9ed", zeroline = FALSE),
           margin = list(l = 70, r = 15, b = 85, t = 15),
           paper_bgcolor = "white", plot_bgcolor = "white",
           font = list(family = "Arial, sans-serif", size = 12, color = "#182c3d")) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  observeEvent(event_data("plotly_click", source = "scatter"), {
    e <- event_data("plotly_click", source = "scatter")
    if (!is.null(e) && "customdata" %in% names(e)) {
      code <- as.character(e$customdata[1])
      if (code %in% pop$abbr) selected_state(code)
    }
  })
  
  output$scatter_statistics <- renderUI({
    d <- scatter_data()
    if (nrow(d) < 4 || sd(d$x) == 0 || sd(d$y) == 0) {
      return(div(class = "atlas-alert", "Insufficient data for correlation."))
    }
    test <- cor.test(d$x, d$y, method = "pearson")
    r <- unname(test$estimate)
    label <- if (abs(r) < .1) "Near-zero linear" else if (abs(r) < .3) "Weak" else if (abs(r) < .5) "Moderate" else "Relatively strong"
    div(class = "kpi-grid",
        stat_box("Pearson correlation (r)", fmt_signed(r, 3)),
        stat_box("Simple linear R²", fmt_num(r^2, 3)),
        stat_box("P-value", format.pval(test$p.value, digits = 3)),
        stat_box("States plotted", as.character(nrow(d)), label)
    )
  })
  
  output$correlation_table <- renderDT({
    d <- view_data()
    k <- input$y_metric
    ys <- add_metric(d, k)$value
    all_x <- unname(x_choices)
    cor_vals <- rep(NA_real_, length(all_x))
    for (i in seq_along(all_x)) {
      xs <- add_metric(d, all_x[i])$value
      good <- is.finite(xs) & is.finite(ys)
      if (sum(good) >= 4 && sd(xs[good]) > 0 && sd(ys[good]) > 0) {
        cor_vals[i] <- cor(xs[good], ys[good])
      }
    }
    tbl <- data.frame(
      Factor = unname(metric_labels[all_x]),
      `Pearson r` = round(cor_vals, 3),
      `Absolute r` = round(abs(cor_vals), 3), check.names = FALSE
    )
    tbl <- arrange(tbl, desc(`Absolute r`))
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 9, dom = "t", ordering = FALSE))
  })
  
  output$download_scatter <- downloadHandler(
    filename = function() paste0("scatter_", input$x_metric, "_vs_", input$y_metric, ".csv"),
    content = function(file) {
      d <- scatter_data()[, c("fips", "state", "abbr", "x", "y")]
      names(d)[4:5] <- c(input$x_metric, input$y_metric)
      write.csv(d, file, row.names = FALSE)
    }
  )
  
  route_data <- reactive({
    d <- atlas_all_routes()
    d %>% filter(is.finite(individuals)) %>% head(10)
  })
  
  output$route_kpis <- renderUI({
    s <- view_data()
    s <- filter(s, abbr == input$route_state)
    if (!nrow(s)) return(NULL)
    div(class = "kpi-grid",
        stat_box("Incoming migration proxy (N2)", fmt_num(s$inflow_people)),
        stat_box("Outgoing migration proxy (N2)", fmt_num(s$outflow_people)),
        stat_box("Net reported exemptions", fmt_signed(s$net_people, 0),
                 if (s$incomplete) "Partial: missing/suppressed routes" else "All 50 routes reported"),
        stat_box("Net per 1,000 (2020 base)", fmt_signed(s$net_rate, 2))
    )
  })
  
  output$route_bar <- renderPlot({
    d <- route_data()
    validate(need(nrow(d) > 0, "No valid interstate route records available."))
    ggplot(d, aes(x = reorder(other_state, individuals), y = individuals)) +
      geom_bar(stat = "identity", fill = if (input$route_direction == "in") "#2459aa" else "#b46228",
               width = .75) + coord_flip() +
      labs(x = NULL, y = "Reported IRS exemptions (N2)") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
      atlas_chart_theme() +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(10, 15, 10, 5))
  })
  
  output$route_table <- renderDT({
    d <- atlas_all_routes()
    tbl <- data.frame(
      `From → To` = paste(d$origin_state, "→", d$destination_state),
      `Returns (N1)` = d$returns, `Exemptions (N2)` = d$individuals,
      `AGI ($ millions)` = round(d$agi_thousands / 1000, 1),
      Status = ifelse(!is.finite(d$individuals), "N2 unreported",
                      ifelse(!is.finite(d$returns) | !is.finite(d$agi_thousands),
                             "Other field unreported", "Reported")), check.names = FALSE)
    datatable(tbl, rownames = FALSE, options = list(pageLength = 10,
                                                    lengthMenu = c(10, 25, 50), scrollX = TRUE, dom = "lftip", order = list(),
                                                    columnDefs = list(list(targets = "_all", defaultContent = "N/A"))))
  })
  
  output$state_comparison <- renderDT({
    d <- view_data()
    a <- filter(d, abbr == input$state_a)
    b <- filter(d, abbr == input$state_b)
    req(nrow(a) == 1L, nrow(b) == 1L)
    keys <- c("net_rate", "in_rate", "out_rate", "net_people",
              "inflow_people", "outflow_people", "population_growth_pct",
              "median_income", "median_home_value", "median_rent",
              "median_rent_pct", "unemployment_pct", "poverty_pct",
              "commute_minutes")
    first_values <- character(length(keys))
    second_values <- character(length(keys))
    for (i in seq_along(keys)) {
      first_values[i] <- fmt_metric(a[[keys[i]]], keys[i])
      second_values[i] <- fmt_metric(b[[keys[i]]], keys[i])
    }
    tbl <- data.frame(
      Measure = unname(metric_labels[keys]),
      `First state` = first_values,
      `Second state` = second_values,
      check.names = FALSE
    )
    tbl <- rbind(tbl, data.frame(
      Measure = "Suppressed/missing IRS route records (in+out)",
      `First state` = as.character(a$missing_in + a$missing_out),
      `Second state` = as.character(b$missing_in + b$missing_out),
      check.names = FALSE
    ))
    names(tbl)[2:3] <- c(a$state, b$state)
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 18, dom = "t", ordering = FALSE))
  })
  
  trend_data <- reactive({
    chosen <- c(input$trend_state_a, input$trend_state_b)
    key <- input$trend_metric
    if (key == "pop_index") {
      baseline <- acs_all %>%
        filter(acs_year == 2020) %>%
        select(fips, baseline_population = acs_population)
      survey <- acs_all %>%
        left_join(baseline, by = "fips") %>%
        left_join(select(pop, fips, state, abbr), by = "fips") %>%
        filter(abbr %in% chosen) %>%
        mutate(value = ifelse(is.finite(baseline_population) & baseline_population > 0,
                              100 * (acs_population / baseline_population - 1), NA_real_)) %>%
        arrange(state, acs_year)
      return(data.frame(state = survey$state, year = survey$acs_year,
                        value = survey$value, population = survey$acs_population))
    }
    if (key == "pop_endpoints") {
      states <- filter(pop, abbr %in% chosen)
      baseline <- data.frame(state = states$state, year = 2020, value = 0)
      endpoint <- data.frame(state = states$state, year = 2025,
                             value = states$population_growth_pct)
      # Only two measured endpoints; no invented annual observations.
      return(bind_rows(baseline, endpoint) %>% arrange(state, year))
    }
    if (key %in% c("net_people", "net_rate", "inflow_people", "outflow_people")) {
      migration <- irs_totals %>%
        left_join(select(pop, fips, state, abbr), by = "fips") %>%
        filter(abbr %in% chosen)
      end_years <- c("1920" = 2020L, "2021" = 2021L,
                     "2122" = 2022L, "2223" = 2023L)
      return(data.frame(
        state = migration$state,
        year = unname(end_years[migration$period]),
        value = migration[[key]], incomplete = migration$incomplete
      ))
    }
    survey <- acs_all %>%
      left_join(select(pop, fips, state, abbr), by = "fips") %>%
      filter(abbr %in% chosen)
    data.frame(state = survey$state, year = survey$acs_year,
               value = survey[[key]])
  })
  
  output$trend_plot <- renderPlotly({
    d <- trend_data()
    d <- filter(d, is.finite(value))
    validate(need(nrow(d) > 0, "No observations available for this indicator."))
    key <- input$trend_metric
    d$partial <- FALSE
    d$tooltip <- paste0(d$state, " · ", d$year, ": ", fmt_num(d$value, 2))
    if (key %in% atlas_irs_keys) {
      m <- left_join(irs_totals, select(pop, fips, state), by = "fips")
      ends <- c("1920" = 2020L, "2021" = 2021L, "2122" = 2022L, "2223" = 2023L)
      m$year <- unname(ends[m$period])
      idx <- match(paste(d$state, d$year), paste(m$state, m$year))
      stopifnot(!anyNA(idx))
      d$partial <- m$incomplete[idx]
      d$tooltip <- paste0(d$state, "<br>IRS ", unname(period_labels[m$period[idx]]),
                          "<br>", metric_labels[[key]], ": ", fmt_num(d$value, 2),
                          ifelse(d$partial, paste0("<br>Partial totals: ", m$missing_in[idx],
                                                   " incoming and ", m$missing_out[idx], " outgoing routes missing/unreported"),
                                 "<br>All inbound and outbound N2 routes reported"))
    } else if (key == "pop_index") {
      d$tooltip <- paste0(d$state, "<br>ACS reference year: ", d$year,
                          "<br>Total population: ", fmt_num(d$population),
                          "<br>Change from ACS 2020: ", fmt_num(d$value, 2), "%")
    } else if (key == "pop_endpoints") {
      d$tooltip <- paste0(d$tooltip, "% change from 2020",
                          "<br>Source: state_population_growth_clean.csv")
    } else {
      d$tooltip <- paste0(d$state, "<br>ACS reference year: ", d$year,
                          "<br>", metric_labels[[key]], ": ", fmt_num(d$value, 2))
    }
    d <- arrange(d, state, year)
    p <- ggplot(d, aes(year, value, color = state, group = state, text = tooltip)) +
      geom_line(linewidth = 1.1) + geom_point(aes(shape = partial), size = 3.2) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 5),
                         breaks = c(FALSE, TRUE), labels = c("Complete routes", "Partial routes"),
                         name = "IRS coverage") +
      scale_color_manual(values = c("#2459aa", "#b46228")) +
      scale_x_continuous(breaks = sort(unique(d$year))) +
      labs(x = if (key %in% atlas_irs_keys) "IRS period end year" else "Year",
           y = if (key == "pop_index") "ACS population change from ACS 2020 (%)" else
             if (key == "pop_endpoints") "Population change from 2020 (%)" else metric_labels[[key]],
           color = "State") + atlas_chart_theme() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    if (!any(d$partial)) p <- p + guides(shape = "none")
    ggplotly(p, tooltip = "text", source = "trends") %>%
      layout(margin = list(l = 75, r = 15, b = 65, t = 15)) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  
  output$download_master <- downloadHandler(
    filename = function() paste0("american_migration_clean_", input$irs_period,
                                 "_ACS", input$acs_year, ".csv"),
    content = function(file) write.csv(view_data(), file, row.names = FALSE)
  )
  # Reporting-period notes and full route records
  
  observe({
    req(input$page)
    session$sendCustomMessage("atlas-active-page", input$page)
  })
  atlas_period <- function(key) {
    if (key %in% atlas_irs_keys)
      return(paste("IRS migration:", unname(period_labels[[input$irs_period]])))
    if (key %in% c("population_growth_pct", "pop_endpoints"))
      return("Population endpoints: 2020 and 2025")
    paste("ACS reference year:", input$acs_year,
          "(confirm survey product and coverage in source metadata)")
  }
  atlas_note <- function(keys, extra = NULL) {
    has_irs <- any(keys %in% atlas_irs_keys)
    periods <- character(length(keys))
    for (i in seq_along(keys)) {
      periods[i] <- atlas_period(keys[i])
    }
    div(class = "atlas-period-note",
        tags$strong(paste(unique(periods), collapse = " | ")),
        if (has_irs) tags$span(class = "atlas-definition", atlas_definition),
        if (!is.null(extra)) tags$span(class = "atlas-warning", extra))
  }
  atlas_scatter_note <- reactive({
    d <- scatter_data()
    has_irs <- input$y_metric %in% atlas_irs_keys
    extra <- paste("These sources have different reporting windows; the factor is not necessarily measured before migration.",
                   "Associations do not establish causation.")
    if (has_irs) extra <- paste(extra, sum(d$incomplete),
                                "plotted states have partial IRS totals. Open diamonds mark these states; they remain in the fit and correlation.")
    atlas_note(c(input$x_metric, input$y_metric), extra)
  })
  output$atlas_scatter_period <- renderUI(atlas_scatter_note())
  output$atlas_corr_period <- renderUI(atlas_scatter_note())
  output$atlas_map_period <- renderUI({
    key <- input$map_metric
    explanation <- if (key %in% c("net_rate", "net_people"))
      "Blue = net gains; orange = net losses; the scale is centered at zero." else
        "Darker blue indicates a larger value; read the legend for the selected range."
    atlas_note(key, explanation)
  })
  output$atlas_rank_period <- renderUI(atlas_note("net_rate",
                                                  "* indicates partial IRS totals. Rankings use reported totals, including partial states."))
  output$atlas_route_period <- renderUI(atlas_note("inflow_people",
                                                   "Chart: ten largest routes with reported N2. Table: all available route records."))
  output$atlas_table_period <- renderUI(atlas_note("inflow_people",
                                                   "Missing values are shown as N/A. Omitted source pairs cannot appear as records."))
  output$atlas_compare_period <- renderUI(atlas_note(
    c("net_rate", "median_income", "population_growth_pct")))
  output$atlas_trend_period <- renderUI({
    key <- input$trend_metric
    if (key %in% atlas_irs_keys) {
      div(class = "atlas-period-note", tags$strong("IRS periods: 2019–2020 through 2022–2023; x-axis shows period end year."),
          tags$span(class = "atlas-definition", atlas_definition),
          tags$span(class = "atlas-warning", "Open diamonds mark partial totals; hover for missing-route counts. Sidebar year selections do not filter this chart."))
    } else if (key %in% c("pop_index", "acs_population")) {
      div(class = "atlas-period-note", tags$strong("ACS population: reference years 2019–2024."),
          " Each point uses Total Population from that year's ACS file. Percentage change uses the same state's ACS 2020 baseline. No ACS 2025 file was supplied. Estimates may cover overlapping survey periods; confirm product coverage before interpreting changes. Sidebar year selections do not filter this chart.")
    } else if (key == "pop_endpoints") {
      div(class = "atlas-period-note", tags$strong("Population endpoints: 2020 and 2025 only."),
          " The connecting line joins the two observations; it does not show annual estimates.")
    } else {
      div(class = "atlas-period-note", tags$strong("ACS reference years: 2019–2024."),
          " Confirm survey coverage in source metadata; estimates may overlap. Dollar values use their survey-year bases. Sidebar year selections do not filter this chart.")
    }
  })
  
  # The full table and top-ten chart now have deliberately separate datasets.
  atlas_all_routes <- reactive({
    d <- filter(irs_flows, period == input$irs_period)
    f <- pop$fips[match(input$route_state, pop$abbr)]
    req(length(f) == 1L, !is.na(f))
    if (input$route_direction == "in") {
      d <- filter(d, destination_fips == f)
      d$other_state <- d$origin_state
    } else {
      d <- filter(d, origin_fips == f)
      d$other_state <- d$destination_state
    }
    arrange(d, desc(individuals))
  })
  
}


ui <- fluidPage(
  tags$head(tags$title("American Migration Atlas — R Shiny"),
            tags$style(HTML(app_css)),
            tags$script(HTML("$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('atlas-active-page', function(page) {
    $('.atlas-sidebar .action-button').removeClass('atlas-active').removeAttr('aria-current');
    $('#go_' + page).addClass('atlas-active').attr('aria-current', 'page');
  });
});
(function() {
  var timer;
  function fitAtlasMap() {
    var el = document.getElementById('state_map');
    if (!el || !el.data || !window.Plotly || !el.offsetWidth) return;
    var narrow = el.clientWidth < 620;
    var update = {
      'margin.r': narrow ? 8 : 95,
      'margin.b': narrow ? 80 : 0,
      'margin.t': 8,
      'height': el.clientHeight,
      'width': el.clientWidth
    };
    Plotly.relayout(el, update);
    Plotly.restyle(el, {
      'colorbar.orientation': narrow ? 'h' : 'v',
      'colorbar.x': narrow ? 0.5 : 1.01,
      'colorbar.y': narrow ? -0.12 : 0.5,
      'colorbar.xanchor': narrow ? 'center' : 'left',
      'colorbar.yanchor': narrow ? 'top' : 'middle',
      'colorbar.len': narrow ? 0.85 : 0.67,
      'colorbar.thickness': narrow ? 12 : 15
    }, [0]);
  }
  function scheduleMap() { clearTimeout(timer); timer = setTimeout(fitAtlasMap, 120); }
  $(window).on('resize', scheduleMap);
  $(document).on('shiny:value', function(e) {
    if (e.name === 'state_map') scheduleMap();
  });
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'page') scheduleMap();
  });
})();"))),
  tags$a(href = "#atlas_main", class = "atlas-skip-link", "Skip to main content"),
  div(class = "atlas-shell",
      div(class = "atlas-sidebar", role = "navigation", "aria-label" = "Atlas navigation and data selection",
          div(class = "atlas-brand", "American ", tags$br(), "Migration Atlas"),
          div(class = "atlas-nav-label", "Explore the data"),
          actionButton("go_overview", "Overview", class = "atlas-active",
                       "aria-current" = "page"),
          actionButton("go_explore", "Explore factors"),
          actionButton("go_routes", "Migration routes"),
          actionButton("go_compare", "Compare states"),
          actionButton("go_trends", "Trends"),
          div(class = "atlas-nav-label", "About the project"),
          actionButton("go_about", "About us"),
          actionButton("go_methods", "Data & methods")
      ),
      div(class = "atlas-main", id = "atlas_main", role = "main", tabindex = "-1",
          conditionalPanel("input.page == 'overview'",
                           tags$header(class = "atlas-page-header",
                                       h1("U.S. migration overview"),
                                       p(class = "atlas-lede", "Explore state-level migration and population change. Select a measure, then click a state for details."))),
          conditionalPanel("input.page != 'about' && input.page != 'methods'",
                           div(class = "atlas-reporting-controls", role = "group",
                               "aria-label" = "Reporting periods and map measure",
                               selectInput("irs_period", "IRS migration period",
                                           choices = stats::setNames(names(period_labels), period_labels), selected = "2223"),
                               selectInput("acs_year", "ACS reference year",
                                           choices = as.character(2019:2024), selected = "2024"),
                               conditionalPanel("input.page == 'overview'",
                                                selectInput("map_metric", "Map measure",
                                                            choices = map_choices, selected = "net_rate")))),
          tabsetPanel(id = "page", type = "hidden",
                      tabPanel(title = "Overview", value = "overview",
                               tags$section(class = "atlas-card atlas-map-section",
                                            uiOutput("atlas_map_period"),
                                            plotlyOutput("state_map", height = "470px"),
                                            uiOutput("map_source_note"),
                                            conditionalPanel("input.map_metric == 'net_rate' || input.map_metric == 'net_people' || input.map_metric == 'in_rate' || input.map_metric == 'out_rate'",
                                                             tags$details(tags$summary("Example: 1 return, 4 exemptions"), p(exemption_example))),
                                            uiOutput("selected_details")
                               ),
                               h2(class = "atlas-summary-heading", "National summary · selected reporting periods"),
                               uiOutput("overview_kpis"),
                               ui_card("States gaining & losing",
                                       subtitle = "Six highest and six lowest reported net migration proxies. * marks partial route data.",
                                       uiOutput("atlas_rank_period"),
                                       plotOutput("net_rank_plot", height = "440px")
                               )
                      ),
                      tabPanel(title = "Explore factors", value = "explore",
                               h1("Explore state-level relationships"),
                               p(class = "atlas-lede", "Choose a state-level factor and an outcome. Check the reporting periods below; points represent states, not individual movers."),
                               ui_card("Compare a factor and an outcome",
                                       div(class = "atlas-control-row",
                                           selectInput("x_metric", "X-axis: state-level factor",
                                                       choices = x_choices, selected = "median_rent_pct"),
                                           selectInput("y_metric", "Y-axis: outcome",
                                                       choices = y_choices, selected = "net_rate")
                                       ),
                                       uiOutput("atlas_scatter_period"),
                                       plotlyOutput("scatter_plot", height = "460px"),
                                       div(class = "factor-key", role = "group", "aria-label" = "Scatterplot legend",
                                           tags$strong("How to read the plot"),
                                           tags$span(tags$i(class = "key-dot", "aria-hidden" = "true"), "One state"),
                                           conditionalPanel("input.y_metric == 'net_rate' || input.y_metric == 'in_rate' || input.y_metric == 'out_rate' || input.y_metric == 'net_people'",
                                                            tags$span(tags$i(class = "key-partial", "aria-hidden" = "true"), "Open diamond = partial IRS data")),
                                           tags$span(tags$i(class = "key-fit", "aria-hidden" = "true"), "Dashed line = linear fit")),
                                       conditionalPanel("input.y_metric == 'net_rate' || input.y_metric == 'in_rate' || input.y_metric == 'out_rate' || input.y_metric == 'net_people'",
                                                        tags$details(tags$summary(partial_irs_heading), p(partial_irs_text))),
                                       uiOutput("scatter_statistics"),
                                       downloadButton("download_scatter", "Download plotted state data (CSV)"),
                                       div(class = "atlas-alert", "A correlation is an association, not proof that an economic condition caused people to move.")
                               ),
                               ui_card("Compare correlations across factors",
                                       subtitle = "Ranked by absolute Pearson correlation for the currently selected Y-axis outcome.",
                                       uiOutput("atlas_corr_period"),
                                       DTOutput("correlation_table")
                               )
                      ),
                      tabPanel(title = "Migration routes", value = "routes",
                               h1("Interstate migration routes"),
                               p(class = "atlas-lede", "Explore which states sent or received exemptions represented on IRS interstate tax-return records. The partner list also includes Washington, D.C."),
                               ui_card("Explore interstate moves",
                                       div(class = "atlas-control-row",
                                           selectInput("route_state", "State", choices = state_choices,
                                                       selected = "SC"),
                                           selectInput("route_direction", "Direction",
                                                       choices = c("Incoming: origin states" = "in",
                                                                   "Outgoing: destination states" = "out"))
                                       ),
                                       uiOutput("route_kpis")
                               ),
                               div(class = "two-even",
                                   ui_card("Top 10 interstate routes", uiOutput("atlas_route_period"),
                                           plotOutput("route_bar", height = "460px")),
                                   ui_card("All available route records",
                                           subtitle = "N1 = tax returns; N2 = exemptions (a proxy for represented persons); AGI in millions of dollars.",
                                           uiOutput("atlas_table_period"),
                                           DTOutput("route_table")
                                   )
                               ),
                               div(class = "atlas-alert", "Routes include Washington, D.C. as a partner. Suppressed/omitted pairs are not counted as zero; states with incomplete route data are flagged in the summary.")
                      ),
                      tabPanel(title = "Compare states", value = "compare",
                               h1("Compare two states"),
                               p(class = "atlas-lede", "Compare reported migration flows, 2020–2025 growth, and ACS socioeconomic measures; housing-cost burden is not included in this data release."),
                               ui_card("Side-by-side comparison",
                                       div(class = "atlas-control-row",
                                           selectInput("state_a", "First state", state_choices, selected = "FL"),
                                           selectInput("state_b", "Second state", state_choices, selected = "CA")
                                       ),
                                       uiOutput("atlas_compare_period"),
                                       DTOutput("state_comparison")
                               )
                      ),
                      tabPanel(title = "Trends", value = "trends",
                               h1("Changes over time"),
                               p(class = "atlas-lede", "Compare ACS population and other measures for reference years 2019–2024, four IRS migration periods, or the separate 2020–2025 population endpoints."),
                               ui_card("Time-series comparison",
                                       div(class = "atlas-control-row",
                                           selectInput("trend_metric", "Measure", choices = trend_choices,
                                                       selected = "net_people"),
                                           selectInput("trend_state_a", "First state", state_choices, selected = "FL"),
                                           selectInput("trend_state_b", "Second state", state_choices, selected = "CA")
                                       ),
                                       uiOutput("atlas_trend_period"),
                                       plotlyOutput("trend_plot", height = "480px"),
                                       div(class = "atlas-alert", "ACS estimates may represent overlapping multi-year periods and monetary values use their own survey-year inflation bases. ACS population is available for reference years 2019–2024; the separate population-estimates file contains 2020 and 2025 endpoints. IRS per-1,000 values use the fixed 2020 population denominator.")
                               )
                      ),
                      tabPanel(title = "About Us", value = "about",
                               div(class = "about-eyebrow", "THE PEOPLE BEHIND THE DATA"),
                               h1("Meet the team"),
                               p(class = "atlas-lede",
                                 "We are students at Washington and Lee University bringing together interests in economics, neuroscience, engineering and computer science to investigate patterns of migration across the United States."),
                               div(class = "team-grid",
                                   team_card(
                                     "photo_kim", "Lucas Kim", "Economics · Class of 2027",
                                     paste0(
                                       "Lucas studies economics with a minor in Latin American and Caribbean Studies. ",
                                       "His interests in environmental economics and public policy offer perspective ",
                                       "on economic opportunity and regional conditions."
                                     ),
                                     "https://www.linkedin.com/in/lucas-kim-2b321a28a/"
                                   ),
                                   team_card(
                                     "photo_chacon", "Lucas Chacon", "Neuroscience · Class of 2027",
                                     paste0(
                                       "Lucas is a Neuroscience major on the pre-dental track and an NCAA varsity swimmer ",
                                       "at Washington and Lee University. He hopes to bring his interests in science ",
                                       "and service together as a dentist by expanding access to quality dental care ",
                                       "in underserved communities."
                                     ),
                                     "https://www.linkedin.com/in/lucas-chacon-0812a3360/"
                                   ),
                                   team_card(
                                     "photo_salazar", "Diana Salazar", "Integrated Engineering & Computer Science · Class of 2027",
                                     paste0(
                                       "Diana studies integrated engineering and computer science. She has applied ",
                                       "data analysis and visual reporting to city procurement records and workforce ",
                                       "demographics."
                                     ),
                                     "https://www.linkedin.com/in/diana-salazar-a93a8a265/"
                                   )
                               ),
                               ui_card("Why we built American Migration Atlas",
                                       p("As part of our BIOL 1185 Visualizing Big Data project, we investigate where U.S. populations are changing and how interstate migration relates to rent, home values, employment, income and other state-level conditions."),
                                       p("Our visualizations use publicly available Census, American Community Survey (ACS) and IRS data. We distinguish population growth from interstate migration and treat correlations as exploratory associations, not explanations of individual decisions."),
                                       tags$span(class = "atlas-badge", "Washington and Lee University · BIOL 1185")
                               )
                      ),
                      tabPanel(title = "Data & methods", value = "methods",
                               h1("Data, definitions & limitations"),
                               p(class = "atlas-lede",
                                 "All figures are recalculated from the 15 clean CSV files included in this project. The app uses no older housing table or annual population data that are not present in the new release."),
                               div(class = "two-even",
                                   ui_card("Files and reporting windows",
                                           p("Download the original project CSVs by filename. The export below uses the selected reporting periods."),
                                           lapply(names(source_groups), function(group) {
                                             tags$details(tags$summary(group), source_links(source_groups[[group]]))
                                           }),
                                           downloadButton("download_master", "Export selected state dataset (CSV)")
                                   ),
                                   ui_card("How we calculate metrics",
                                           h3("What does an IRS exemption count?"),
                                           p("In IRS migration data, each exemption represents one person listed on a tax return."),
                                           p(exemption_example),
                                           h3(partial_irs_heading), p(partial_irs_text),
                                           h3("How to read the numbers"),
                                           tags$ul(
                                             tags$li(tags$b("Net migration"), " = incoming exemptions − outgoing exemptions."),
                                             tags$li(tags$b("Rate per 1,000"), " = (incoming, outgoing, or net exemptions ÷ the state’s 2020 population) × 1,000.",
                                                     p("Example: The reported counts show 50 more people moved in than out. In a state with 10,000 residents in 2020, that’s a net gain of 5 per 1,000.")),
                                             tags$li(tags$b("Population growth (%)"), " = (2025 population − 2020 population) ÷ 2020 population × 100."),
                                             tags$li(tags$b("ACS population change (%)"), " = population change since 2020 ÷ 2020 population × 100. Both values come from ACS."),
                                             tags$li(tags$b("Unemployment (%)"), " = unemployed people ÷ civilian labor force × 100."),
                                             tags$li(tags$b("Poverty (%)"), " = people below the poverty line ÷ people with known poverty status × 100."),
                                             tags$li(tags$b("Home-to-income ratio"), " = median home value ÷ median household income."),
                                             tags$li(tags$b("Rent share (%)"), " = the median share of household income spent on rent, as reported by ACS.")
                                           )
                                   )
                               ),
                               ui_card("Important interpretation and data-quality notes",
                                       tags$ul(
                                         tags$li("Both IRS inflow and outflow files describe the same routes. We analyze each pair once rather than add both source files."),
                                         tags$li("Migration entries use the source field 'exemptions' (N2), not a complete count of all people moving. Some pairs are suppressed or omitted, so reported state totals can be incomplete."),
                                         tags$li("The route universe includes 50 states and Washington, D.C.; maps and state comparisons cover the 50 states only."),
                                         tags$li("AGI is measured in thousands of dollars. Some paired IRS files differ by $1,000 of AGI for a few historic routes; the inflow view is used consistently."),
                                         tags$li("IRS rates retain the fixed 2020 population-estimate denominator for consistency. ACS population is shown separately and is not substituted into these rates."),
                                         tags$li("ACS Total Population supplies reference-year observations for 2019–2024. Its percentage change uses ACS 2020 as the baseline. The separate 2020–2025 endpoint comparison uses the population-estimates file; these sources are not spliced into a single series."),
                                         tags$li("ACS extracts may use overlapping multi-year samples, and survey-year dollar units vary. Confirm the Census product when interpreting change over time."),
                                         tags$li("Comparing ACS attributes with migration does not reveal why individuals relocated or prove causal relationships."),
                                         tags$li("The old standalone 2024 housing-cost-burden source was not supplied with this new dataset; that metric has been removed.")
                                       )
                               )
                      )
          )
      )
  )
)

shinyApp(ui = ui, server = server)
