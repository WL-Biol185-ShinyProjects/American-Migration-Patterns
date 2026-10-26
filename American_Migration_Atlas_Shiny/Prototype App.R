# AMERICAN MIGRATION ATLAS — R SHINY EDITION
# BIOL 1185 | Reads the ORIGINAL CSV files in this repository.
# No pre-cleaned/master CSV, HTML, JavaScript, database or external API required.
# Install packages once: install.packages(c("shiny", "plotly", "ggplot2", "DT"))
# In RStudio, open this file from the repository folder and click Run App.

required_packages <- c("shiny", "plotly", "ggplot2", "DT")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install missing packages first: install.packages(c(",
       paste(shQuote(missing_packages), collapse = ", "), "))")
}

library(shiny)
library(plotly)
library(ggplot2)
library(DT)

# --------------- READ THE REPOSITORY'S ORIGINAL CSV FILES ---------------
# shiny::runApp() sets the app directory as the working directory.
repo_dir <- getwd()
if (!file.exists(file.path(repo_dir, "Population Change by State Level.csv"))) {
  stop("CSV files not found. Put app.R directly inside the extracted ",
       "American-Migration-Patterns-main folder and run it from there.")
}

read_source <- function(name) {
  f <- file.path(repo_dir, name)
  if (!file.exists(f)) stop("Missing original CSV: ", name)
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM")
}

as_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[$,%]", "", as.character(x))))
}

# Exact Census geography identifiers. Custom alphabetical ranks 01-50 are
# DISPLAY-ONLY, not replacement FIPS codes for merging datasets.
raw_pop <- read_source("Population Change by State Level.csv")
pop <- raw_pop[raw_pop$NAME %in% state.name, , drop = FALSE]
pop$fips <- sprintf("%02d", as.integer(pop$STATE))
pop$state <- pop$NAME
pop$abbr <- state.abb[match(pop$state, state.name)]
for (yr in 2020:2025) {
  pop[[paste0("population_", yr)]] <-
    as_number(pop[[paste0("POPESTIMATE", yr)]])
}
pop$population_growth_pct <- 100 *
  (pop$population_2025 - pop$population_2020) / pop$population_2020
pop <- pop[order(pop$state), c(
  "fips", "state", "abbr", paste0("population_", 2020:2025),
  "population_growth_pct"
)]
pop$state_number <- sprintf("%02d", seq_len(nrow(pop)))
stopifnot(nrow(pop) == 50L, !anyDuplicated(pop$fips))

# Read one ACS file per year. Column headings change between ACS exports;
# grep patterns explicitly match each year's original headings.
find_col <- function(data, regex, label) {
  hits <- grep(regex, names(data), value = TRUE, ignore.case = TRUE)
  if (length(hits) != 1L) {
    stop("Expected one column for ", label, "; found ", length(hits),
         ". Matched: ", paste(hits, collapse = " | "))
  }
  as_number(data[[hits]])
}

load_acs_year <- function(year) {
  d <- read_source(sprintf("ACS_%s_state_new.csv", year))
  fips <- sprintf("%02d", as.integer(as_number(d$FIPS)))
  keep <- fips %in% pop$fips
  d <- d[keep, , drop = FALSE]
  fips <- fips[keep]
  
  poverty_cols <- grep(
    "^Population for Whom Poverty Status Is Determined.*(Under \\.50|\\.50 to \\.74|\\.75 to \\.99)$",
    names(d), value = TRUE, ignore.case = TRUE
  )
  if (length(poverty_cols) != 3L) {
    stop("Poverty bands not found in ACS_", year, "_state_new.csv")
  }
  under_poverty <- rowSums(sapply(d[poverty_cols], as_number), na.rm = FALSE)
  in_labor_force <- find_col(
    d, "^Civilian Population in Labor Force 16 Years and Over:?$", "labor force"
  )
  poverty_universe <- find_col(
    d, "^Population for Whom Poverty Status Is Determined:?$", "poverty universe"
  )
  
  out <- data.frame(
    fips = fips,
    acs_year = as.integer(year),
    median_income = find_col(d, "^Median Household Income", "household income"),