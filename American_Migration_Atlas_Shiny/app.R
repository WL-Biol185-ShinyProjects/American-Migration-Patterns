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
    median_home_value = find_col(d, "^Median (House )?Value$", "home value"),
    median_rent = find_col(d, "^Median Gross Rent$", "gross rent"),
    median_rent_pct = find_col(
      d, "^Median Gross Rent as a Percentage", "median renter cost/income"
    ),
    unemployment_pct = 100 * find_col(
      d, "^Civilian Population in Labor Force 16 Years and Over: Unemployed$",
      "unemployed residents"
    ) / in_labor_force,
    poverty_pct = 100 * under_poverty / poverty_universe,
    commute_minutes = find_col(d, "^Average Commute to Work", "commute"),
    stringsAsFactors = FALSE
  )
  out$unemployment_pct[!is.finite(out$unemployment_pct)] <- NA_real_
  out$poverty_pct[!is.finite(out$poverty_pct)] <- NA_real_
  stopifnot(nrow(out) == 50L, !anyDuplicated(out$fips))
  out
}
acs_all <- do.call(rbind, lapply(2019:2024, load_acs_year))

# The housing table is 13 rows by 105 columns, with one "STATE!!Estimate"
# per geography. Over 50% rows are PART OF the "Over 30 Percent" rows;
# never count them twice.
raw_housing <- read_source(
  "Housing Costs as a % of Household Income in the Past 12 Months.csv"
)
housing_labels <- trimws(gsub("\u00a0", " ", raw_housing[[1]]))
over30_rows <- which(housing_labels == "Over 30 Percent")
not_calculated_rows <- which(housing_labels == "Not Calculated")
stopifnot(length(over30_rows) == 3L, length(not_calculated_rows) == 3L)

housing <- data.frame(
  fips = pop$fips,
  housing_burden_pct_2024 = vapply(seq_len(nrow(pop)), function(i) {
    col <- paste0(pop$state[i], "!!Estimate")
    if (!col %in% names(raw_housing)) stop("Housing state missing: ", col)
    v <- as_number(raw_housing[[col]])
    denominator <- v[1] - sum(v[not_calculated_rows])
    if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
    100 * sum(v[over30_rows]) / denominator
  }, numeric(1)),
  stringsAsFactors = FALSE
)

# IRS migration: "Total Migration-US" EXCLUDES foreign and in-state moves.
# N1 is matched federal tax returns; N2 is represented individuals.
# AGI is summed income in THOUSANDS of dollars.
load_irs_period <- function(period, start_year) {
  inflow <- read_source(paste0("stateinflow", period, ".csv"))
  outflow <- read_source(paste0("stateoutflow", period, ".csv"))

  tin <- inflow[
    grepl(" Total Migration-US$", inflow$y1_state_name), , drop = FALSE
  ]
  tout <- outflow[
    grepl(" Total Migration-US$", outflow$y2_state_name), , drop = FALSE
  ]
  t_in <- data.frame(
    fips = sprintf("%02d", as.integer(tin$y2_statefips)),
    inflow_returns = as_number(tin$n1),
    inflow_people = as_number(tin$n2)
  )
  t_out <- data.frame(
    fips = sprintf("%02d", as.integer(tout$y1_statefips)),
    outflow_returns = as_number(tout$n1),
    outflow_people = as_number(tout$n2)
  )
  totals <- merge(t_in, t_out, by = "fips")
  totals <- merge(totals, pop[, c("fips", paste0("population_", start_year))],
                  by = "fips")
  names(totals)[names(totals) == paste0("population_", start_year)] <-
    "base_population"
  totals <- totals[totals$fips %in% pop$fips, , drop = FALSE]
  totals$period <- period
  totals$net_people <- totals$inflow_people - totals$outflow_people
  totals$net_rate <- 1000 * totals$net_people / totals$base_population
  totals$in_rate <- 1000 * totals$inflow_people / totals$base_population
  totals$out_rate <- 1000 * totals$outflow_people / totals$base_population
  stopifnot(nrow(totals) == 50L)

  # Use the inflow origin-destination detail to avoid double counting the
  # same pair already repeated from the outflow perspective.
  from <- sprintf("%02d", as.integer(inflow$y1_statefips))
  to <- sprintf("%02d", as.integer(inflow$y2_statefips))
  n2 <- as_number(inflow$n2)
  n1 <- as_number(inflow$n1)
  valid <- from %in% pop$fips & to %in% pop$fips & from != to &
           is.finite(n2) & n2 >= 0 & is.finite(n1) & n1 >= 0
  flows <- data.frame(
    period = period,
    origin_fips = from[valid],
    destination_fips = to[valid],
    returns = n1[valid],
    individuals = n2[valid],
    agi_thousands = as_number(inflow$AGI[valid]),
    stringsAsFactors = FALSE
  )
  flows$origin_state <- pop$state[match(flows$origin_fips, pop$fips)]
  flows$destination_state <- pop$state[match(flows$destination_fips, pop$fips)]
  list(totals = totals, flows = flows)
}
irs_2122 <- load_irs_period("2122", 2021)
irs_2223 <- load_irs_period("2223", 2022)
irs_totals <- rbind(irs_2122$totals, irs_2223$totals)
irs_flows <- rbind(irs_2122$flows, irs_2223$flows)

# Schematic 50-state tile map coordinates, not true geographic polygons.
tile_spec <- "WA:1:0 MT:2:0 ND:3:0 MN:4:0 WI:5:0 MI:6:0 VT:9:0 ME:10:0
OR:1:1 ID:2:1 SD:3:1 IA:4:1 IL:5:1 IN:6:1 OH:7:1 PA:8:1 NY:9:1 NH:10:1
CA:0:2 NV:1:2 WY:2:2 NE:3:2 MO:4:2 KY:5:2 WV:6:2 VA:7:2 MD:8:2 NJ:9:2 MA:10:2
AZ:1:3 UT:2:3 CO:3:3 KS:4:3 AR:5:3 TN:6:3 NC:7:3 SC:8:3 DE:9:3 CT:10:3 RI:11:3
NM:2:4 OK:3:4 LA:4:4 MS:5:4 AL:6:4 GA:7:4 TX:3:5 FL:8:5 HI:0:6 AK:0:7"
tile_parts <- strsplit(strsplit(trimws(tile_spec), "[[:space:]]+")[[1]], ":")
tiles <- data.frame(
  abbr = vapply(tile_parts, `[[`, character(1), 1),
  x = as.numeric(vapply(tile_parts, `[[`, character(1), 2)),
  y = as.numeric(vapply(tile_parts, `[[`, character(1), 3))
)
stopifnot(nrow(tiles) == 50L)

# --------------- UI HELPERS / METRIC DEFINITIONS ---------------
metric_labels <- c(
  net_rate = "Net IRS migration per 1,000",
  in_rate = "Inmigration per 1,000",
  out_rate = "Outmigration per 1,000",
  population_growth_pct = "Population growth 2020–2025 (%)",
  housing_burden_pct_2024 = "Housing-cost burden 2024 (%)",
  median_income = "Median household income ($)",
  median_home_value = "Median home value ($)",
  home_income_ratio = "Median home value / income",
  median_rent = "Median gross rent ($/month)",
  median_rent_pct = "Median renter housing cost / income (%)",
  unemployment_pct = "ACS unemployment rate (%)",
  poverty_pct = "Poverty rate (%)",
  commute_minutes = "Average commute (minutes)"
)
pretty_choices <- function(keys) {
  stats::setNames(keys, unname(metric_labels[keys]))
}
x_choices <- pretty_choices(c(
  "housing_burden_pct_2024", "median_income", "median_home_value",
  "home_income_ratio", "median_rent", "median_rent_pct", "unemployment_pct",
  "poverty_pct", "commute_minutes"
))
y_choices <- pretty_choices(c(
  "net_rate", "in_rate", "out_rate", "population_growth_pct"
))
map_choices <- pretty_choices(c(
  "net_rate", "population_growth_pct", "housing_burden_pct_2024",
  "median_income", "unemployment_pct"
))
trend_choices <- c(
  "Population change since 2020 (%)" = "pop_index",
  pretty_choices(c("median_income", "median_home_value", "median_rent",
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
  if (key %in% c("median_income", "median_home_value", "median_rent")) {
    return(paste0("$", fmt_num(x)))
  }
  if (key %in% c("population_growth_pct", "housing_burden_pct_2024",
                "median_rent_pct", "unemployment_pct", "poverty_pct")) {
    return(paste0(fmt_num(x, 2), "%"))
  }
  fmt_signed(x, 2)
}

ui_card <- function(title, ..., subtitle = NULL) {
  div(class = "atlas-card", h3(title),
      if (!is.null(subtitle)) p(class = "atlas-sub", subtitle), ...)
}
stat_box <- function(label, value, detail = "") {
  div(class = "stat-card", div(class = "stat-label", label),
      div(class = "stat-value", value), div(class = "stat-sub", detail))
}

app_css <- "
body {background:#f5f7f7;color:#1d3440;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}
.atlas-shell {display:flex;min-height:100vh;margin:-15px;}
.atlas-sidebar {width:240px;min-width:240px;padding:26px 16px;background:#112a39;color:#e1eeee;position:sticky;top:0;height:100vh;overflow:auto;}
.atlas-brand {font-weight:800;font-size:21px;line-height:1.2;color:white;padding:8px 8px 24px;}
.atlas-brand small {display:block;color:#a2c2c8;font-weight:600;font-size:11px;letter-spacing:1px;margin-top:10px;}
.atlas-sidebar .action-button {background:transparent;border:0;border-radius:10px;color:#e1eeee;text-align:left;width:100%;padding:12px 12px;margin:3px 0;box-shadow:none;font-weight:600;}
.atlas-sidebar .action-button:hover,.atlas-sidebar .action-button:focus {background:#244353;color:white;}
.atlas-sidebar label {color:#c1d3d6!important;font-size:12px;}
.atlas-sidebar select {background:#fff;}
.atlas-sidebar .selectize-input {color:#223c48;}
.atlas-sidebar hr {border-color:#36505e;}
.atlas-main {flex:1;min-width:0;padding:28px 35px 48px;}
.atlas-main h1 {font-size:34px;font-weight:800;letter-spacing:-1px;margin:5px 0 8px;}
.atlas-main h3 {font-size:18px;font-weight:750;margin-top:0;margin-bottom:8px;}
.atlas-lede,.atlas-sub {color:#6d8490;margin-bottom:20px;line-height:1.5;}
.atlas-sub {font-size:12px;margin:-3px 0 16px;}
.atlas-card,.stat-card {background:white;border:1px solid #e2eaea;border-radius:15px;padding:22px;margin:0 0 18px;box-shadow:0 2px 12px rgba(10,36,44,.03);}
.kpi-grid {display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin-bottom:18px;}
.stat-card {min-height:112px;margin-bottom:0;}
.stat-label {font-size:12px;color:#667f8a;font-weight:600;}
.stat-value {font-size:25px;font-weight:800;color:#173d49;margin-top:7px;}
.stat-sub {font-size:11px;color:#8499a0;margin-top:6px;}
.two-panels {display:grid;grid-template-columns:minmax(0,1.55fr) minmax(320px,1fr);gap:17px;align-items:start;}
.two-even {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:17px;align-items:start;}
.shiny-input-container {max-width:100%;}
.form-control {border-radius:9px;border-color:#d3e0e3;}
.btn-atlas,.btn-default {border-radius:9px;}
.atlas-alert {background:#f2f7f6;border-left:3px solid #208b84;padding:12px 14px;color:#506c78;font-size:12px;border-radius:5px;margin-top:10px;}
.atlas-badge {font-size:11px;font-weight:700;color:#0a6b68;background:#e9f4f2;display:inline-block;border-radius:20px;padding:4px 8px;}
.atlas-control-row {display:flex;gap:15px;align-items:flex-end;flex-wrap:wrap;}
.atlas-control-row .form-group {min-width:190px;flex:1;}
.atlas-main table.dataTable thead th {font-size:12px;color:#526d78;}
.atlas-main .dataTables_wrapper {font-size:12px;}
@media(max-width:1050px) {.kpi-grid{grid-template-columns:repeat(2,1fr)}.two-panels,.two-even{grid-template-columns:1fr}}
@media(max-width:700px) {.atlas-shell{display:block}.atlas-sidebar{position:relative;width:100%;height:auto;min-width:0}.atlas-main{padding:18px}.atlas-main h1{font-size:27px}.kpi-grid{grid-template-columns:repeat(2,1fr)}}
"

ui <- fluidPage(
  tags$head(tags$title("American Migration Atlas — R Shiny"),
            tags$style(HTML(app_css))),
  div(class = "atlas-shell",
    div(class = "atlas-sidebar",
      div(class = "atlas-brand", "↗ American", tags$br(), "Migration Atlas",
          tags$small("BIOL 1185 · R SHINY")),
      actionButton("go_overview", "▦  Overview"),
      actionButton("go_explore", "⌁  Explore factors"),
      actionButton("go_routes", "⇄  Migration routes"),
      actionButton("go_compare", "▥  Compare states"),
      actionButton("go_trends", "↗  Trends"),
      actionButton("go_methods", "ⓘ  Data & methods"),
      tags$hr(),
      selectInput("irs_period", "IRS MIGRATION PERIOD",
                  choices = c("2022–2023" = "2223", "2021–2022" = "2122"),
                  selected = "2223"),
      selectInput("acs_year", "ACS REFERENCE YEAR",
                  choices = as.character(2019:2024), selected = "2022"),
      tags$small("50 states · CSV-based · Runs locally in RStudio")
    ),
    div(class = "atlas-main",
      tabsetPanel(id = "page", type = "hidden",
        tabPanel(title = "Overview", value = "overview",
          h1("Where is America moving?"),
          p(class = "atlas-lede", "Explore population change and matched IRS interstate migration. Click a state tile and switch the metric or reporting period."),
          uiOutput("overview_kpis"),
          div(class = "two-panels",
            ui_card("United States, state by state",
              subtitle = "A clickable state tile cartogram, not a geographic boundary map.",
              selectInput("map_metric", NULL, choices = map_choices,
                          selected = "net_rate"),
              plotlyOutput("state_map", height = "440px"),
              uiOutput("selected_details")
            ),
            ui_card("States gaining & losing",
              subtitle = "Net IRS migration rate per 1,000 residents.",
              plotOutput("net_rank_plot", height = "560px")
            )
          )
        ),
        tabPanel(title = "Explore factors", value = "explore",
          h1("Explore the relationships"),
          p(class = "atlas-lede", "Choose a potential explanatory factor and a migration or population outcome. Points represent states—not individual movers."),
          ui_card("Ask your own research question",
            div(class = "atlas-control-row",
              selectInput("x_metric", "X-axis: explanatory factor",
                          choices = x_choices, selected = "housing_burden_pct_2024"),
              selectInput("y_metric", "Y-axis: outcome",
                          choices = y_choices, selected = "net_rate")
            ),
            plotlyOutput("scatter_plot", height = "460px"),
            uiOutput("scatter_statistics"),
            downloadButton("download_scatter", "Download plotted state data (CSV)"),
            div(class = "atlas-alert", "A correlation is an association, not proof that an economic condition caused people to move.")
          ),
          ui_card("Compare correlations across factors",
            subtitle = "Ranked by absolute Pearson correlation for the currently selected Y-axis outcome.",
            DTOutput("correlation_table")
          )
        ),
        tabPanel(title = "Migration routes", value = "routes",
          h1("Follow the migration routes"),
          p(class = "atlas-lede", "Discover the leading states sending or receiving individuals represented on IRS matched returns."),
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
            ui_card("Top 10 interstate routes", plotOutput("route_bar", height = "460px")),
            ui_card("Detailed route records",
              subtitle = "N1 = matched returns; N2 = represented individuals; AGI in millions of dollars.",
              DTOutput("route_table")
            )
          ),
          div(class = "atlas-alert", "IRS U.S.-migration totals include Washington, D.C. The route table shows flows between the 50 states only; totals and visible routes need not sum exactly.")
        ),
        tabPanel(title = "Compare states", value = "compare",
          h1("Compare two states"),
          p(class = "atlas-lede", "Compare migration flows, population growth, housing costs, and socioeconomic conditions."),
          ui_card("Side-by-side comparison",
            div(class = "atlas-control-row",
              selectInput("state_a", "First state", state_choices, selected = "FL"),
              selectInput("state_b", "Second state", state_choices, selected = "CA")
            ),
            DTOutput("state_comparison")
          )
        ),
        tabPanel(title = "Trends", value = "trends",
          h1("Track changes over time"),
          p(class = "atlas-lede", "Compare Census population changes from 2020–2025 and ACS socioeconomic indicators from 2019–2024."),
          ui_card("Time-series comparison",
            div(class = "atlas-control-row",
              selectInput("trend_metric", "Measure", choices = trend_choices,
                          selected = "pop_index"),
              selectInput("trend_state_a", "First state", state_choices, selected = "FL"),
              selectInput("trend_state_b", "Second state", state_choices, selected = "CA")
            ),
            plotlyOutput("trend_plot", height = "480px"),
            div(class = "atlas-alert", "Annual ACS dollar estimates are adjusted to their respective survey years, not a common inflation base. Treat changes in nominal-looking values cautiously.")
          )
        ),
        tabPanel(title = "Data & methods", value = "methods",
          h1("Data, definitions & limitations"),
          p(class = "atlas-lede", "Every measurement is derived at runtime from your repository's original CSV files."),
          div(class = "two-even",
            ui_card("Source files",
              tags$ul(
                tags$li(tags$b("Census population estimates 2020–2025:"),
                        " Population Change by State Level.csv"),
                tags$li(tags$b("American Community Survey 5-year estimates (2020–2024) from Social Explorer:"),
                        " ACS_2019_state_new.csv through ACS_2024_state_new.csv"),
                tags$li(tags$b("2024 housing cost burden:"),
                        " Housing Costs as a % of Household Income in the Past 12 Months.csv"),
                tags$li(tags$b("IRS migration 2021–22 and 2022–23:"),
                        " stateinflow2122.csv, stateoutflow2122.csv, stateinflow2223.csv, stateoutflow2223.csv")
              ),
              downloadButton("download_master", "Export currently selected master data (CSV)")
            ),
            ui_card("Calculated fields",
              tags$ul(
                tags$li("Population growth (%) = (2025 population − 2020 population) ÷ 2020 population × 100."),
                tags$li("Net IRS migration = incoming N2 − outgoing N2."),
                tags$li("IRS rate per 1,000 = N2 ÷ population in beginning IRS year × 1,000."),
                tags$li("Housing burden (%) = households spending over 30% of income on housing ÷ households with calculable housing costs × 100."),
                tags$li("ACS unemployment (%) = unemployed civilians ÷ civilian labor force × 100."),
                tags$li("Poverty (%) = persons below 1.00 poverty-income ratio ÷ poverty-universe population × 100."),
                tags$li("Home/income ratio = median home value ÷ median household income.")
              )
            )
          ),
          ui_card("Interpret carefully",
            tags$ul(
              tags$li("IRS N1 counts matched federal tax returns; N2 is individuals represented on those returns, not all movers."),
              tags$li("AGI is adjusted gross income reported in thousands of dollars in source IRS CSVs."),
              tags$li("State population growth also reflects births, deaths, and international migration—not only interstate movement."),
              tags$li("ACS socioeconomic estimates may be collected after the IRS movement period; temporal associations do not demonstrate causation."),
              tags$li("2019–2024 ACS inputs may not represent exactly comparable products or survey windows; verify before making strong time-trend claims."),
              tags$li("Some 2021–22 route observations have suppressed IRS values; these are omitted rather than interpreted as negative movers."),
              tags$li("Federal FIPS identifiers (e.g., Arizona 04) are preserved for joins; alphabetical 01–50 IDs are display-only."),
              tags$li("No tax-policy, political, climate, or individual motivations are supplied by these files.")
            )
          )
        )
      )
    )
  )
)

# --------------- SHINY SERVER: REACTIVE DATA AND PLOTS ---------------
server <- function(input, output, session) {
  for (page_name in c("overview", "explore", "routes", "compare", "trends", "methods")) {
    local({
      tab <- page_name
      observeEvent(input[[paste0("go_", tab)]], {
        updateTabsetPanel(session, "page", selected = tab)
      })
    })
  }

  selected_state <- reactiveVal("SC")
  observeEvent(input$route_state, {
    if (input$route_state %in% pop$abbr) selected_state(input$route_state)
  }, ignoreInit = TRUE)

  view_data <- reactive({
    a <- acs_all[acs_all$acs_year == as.integer(input$acs_year), , drop = FALSE]
    m <- irs_totals[irs_totals$period == input$irs_period, , drop = FALSE]
    d <- Reduce(function(x, y) merge(x, y, by = "fips"),
                list(pop, a, housing, m))
    d <- d[order(d$state), , drop = FALSE]
    validate(need(nrow(d) == 50, "Expected 50 states in the merged dataset."))
    d
  })

  output$overview_kpis <- renderUI({
    d <- view_data()
    highest <- d[which.max(d$net_rate), ]
    lowest <- d[which.min(d$net_rate), ]
    fastest <- d[which.max(d$population_growth_pct), ]
    div(class = "kpi-grid",
      stat_box("States with positive net IRS inflow",
               paste0(sum(d$net_people > 0), " / 50"),
               paste0(substr(input$irs_period, 1, 2), "–", substr(input$irs_period, 3, 4))),
      stat_box("Largest net gain / 1,000", fmt_signed(highest$net_rate), highest$state),
      stat_box("Largest net loss / 1,000", fmt_signed(lowest$net_rate), lowest$state),
      stat_box("Fastest population growth", paste0(fmt_num(fastest$population_growth_pct, 2), "%"),
               paste(fastest$state, "· 2020–2025"))
    )
  })

  output$state_map <- renderPlotly({
    d <- add_metric(view_data(), input$map_metric)
    d <- merge(d, tiles, by = "abbr")
    d$tip <- paste0(d$state, "<br>", metric_labels[[input$map_metric]],
                    ": ", vapply(seq_len(nrow(d)), function(i)
                      fmt_metric(d$value[i], input$map_metric), character(1)))
    plot_ly(
      d, x = ~x, y = ~y, text = ~abbr, customdata = ~abbr,
      hovertext = ~tip, hovertemplate = "%{hovertext}<extra></extra>",
      type = "scatter", mode = "markers+text", textposition = "middle center",
      textfont = list(color = "white", size = 11, family = "Arial"),
      color = ~value, colors = c("#db8567", "#ecf2f0", "#0d817c"),
      marker = list(size = 35, symbol = "square",
                    line = list(color = "white", width = 2)),
      source = "state_tile", showlegend = FALSE
    ) %>% layout(
      xaxis = list(visible = FALSE, range = c(-0.6, 11.6), fixedrange = TRUE),
      yaxis = list(visible = FALSE, range = c(7.65, -0.6), fixedrange = TRUE),
      margin = list(l = 0, r = 10, t = 4, b = 0),
      paper_bgcolor = "white", plot_bgcolor = "white"
    )
  })

  observeEvent(event_data("plotly_click", source = "state_tile"), {
    e <- event_data("plotly_click", source = "state_tile")
    if (!is.null(e) && "customdata" %in% names(e)) {
      code <- as.character(e$customdata[1])
      if (code %in% pop$abbr) {
        selected_state(code)
        updateSelectInput(session, "route_state", selected = code)
      }
    }
  })

  output$selected_details <- renderUI({
    d <- view_data()
    s <- d[d$abbr == selected_state(), , drop = FALSE]
    if (!nrow(s)) return(NULL)
    div(class = "atlas-alert",
      tags$b(paste0(s$state, " (", s$abbr, ")")), tags$br(),
      "Net IRS migrants: ", tags$b(fmt_signed(s$net_people, 0)), " · ",
      "2020–25 growth: ", tags$b(paste0(fmt_signed(s$population_growth_pct, 2), "%")),
      " · 2024 housing burden: ",
      tags$b(paste0(fmt_num(s$housing_burden_pct_2024, 1), "%"))
    )
  })

  output$net_rank_plot <- renderPlot({
    d <- view_data()[order(view_data()$net_rate, decreasing = TRUE), ]
    take <- rbind(head(d, 6), tail(d, 6))
    take$kind <- ifelse(take$net_rate >= 0, "Net gain", "Net loss")
    ggplot(take, aes(x = reorder(state, net_rate), y = net_rate,
                     fill = kind)) +
      geom_col(width = .7) + coord_flip() +
      scale_fill_manual(values = c("Net gain" = "#16887e", "Net loss" = "#dd8766")) +
      labs(x = NULL, y = "Net migration per 1,000 residents") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", panel.grid.major.y = element_blank(),
            plot.margin = margin(12, 16, 12, 5))
  })

  scatter_data <- reactive({
    d <- add_metric(view_data(), input$x_metric)
    names(d)[names(d) == "value"] <- "x"
    d <- add_metric(d, input$y_metric)
    names(d)[names(d) == "value"] <- "y"
    d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
    d$tip <- paste0(d$state, "<br>", metric_labels[[input$x_metric]],
                    ": ", round(d$x, 2), "<br>", metric_labels[[input$y_metric]],
                    ": ", round(d$y, 2))
    d
  })

  output$scatter_plot <- renderPlotly({
    d <- scatter_data()
    validate(need(nrow(d) >= 3 && sd(d$x) > 0 && sd(d$y) > 0,
                  "Not enough variation for a scatterplot."))
    fit <- lm(y ~ x, data = d)
    line_d <- data.frame(x = range(d$x))
    line_d$y <- predict(fit, newdata = line_d)
    marker_colors <- ifelse(d$abbr == selected_state(), "#da7b55", "#16887e")
    p <- plot_ly(
      data = d, x = ~x, y = ~y, text = ~tip, customdata = ~abbr,
      type = "scatter", mode = "markers", hoverinfo = "text",
      marker = list(color = marker_colors, size = 10,
                    line = list(color = "white", width = 1)),
      source = "scatter", showlegend = FALSE
    )
    p <- add_lines(p, data = line_d, x = ~x, y = ~y,
                   inherit = FALSE, hoverinfo = "skip", showlegend = FALSE,
                   line = list(color = "#dd8766", dash = "dash", width = 3))
    layout(p,
      xaxis = list(title = metric_labels[[input$x_metric]], zeroline = FALSE),
      yaxis = list(title = metric_labels[[input$y_metric]], zeroline = FALSE),
      margin = list(l = 90, r = 20, b = 80, t = 15),
      paper_bgcolor = "white", plot_bgcolor = "white"
    )
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
    cor_vals <- vapply(all_x, function(x) {
      xs <- add_metric(d, x)$value
      good <- is.finite(xs) & is.finite(ys)
      if (sum(good) < 4 || sd(xs[good]) == 0 || sd(ys[good]) == 0)
        return(NA_real_)
      cor(xs[good], ys[good])
    }, numeric(1))
    tbl <- data.frame(
      Factor = unname(metric_labels[all_x]),
      `Pearson r` = round(cor_vals, 3),
      `Absolute r` = round(abs(cor_vals), 3), check.names = FALSE
    )
    tbl <- tbl[order(-tbl[["Absolute r"]]), , drop = FALSE]
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
    d <- irs_flows[irs_flows$period == input$irs_period, , drop = FALSE]
    s <- pop$fips[match(input$route_state, pop$abbr)]
    if (input$route_direction == "in") {
      d <- d[d$destination_fips == s, , drop = FALSE]
      d$other_state <- d$origin_state
    } else {
      d <- d[d$origin_fips == s, , drop = FALSE]
      d$other_state <- d$destination_state
    }
    head(d[order(d$individuals, decreasing = TRUE), , drop = FALSE], 10)
  })

  output$route_kpis <- renderUI({
    s <- view_data()
    s <- s[s$abbr == input$route_state, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    div(class = "kpi-grid",
      stat_box("Incoming individuals (N2)", fmt_num(s$inflow_people)),
      stat_box("Outgoing individuals (N2)", fmt_num(s$outflow_people)),
      stat_box("Net represented individuals", fmt_signed(s$net_people, 0)),
      stat_box("Net rate per 1,000", fmt_signed(s$net_rate, 2))
    )
  })

  output$route_bar <- renderPlot({
    d <- route_data()
    validate(need(nrow(d) > 0, "No valid interstate route records available."))
    ggplot(d, aes(x = reorder(other_state, individuals), y = individuals)) +
      geom_col(fill = if (input$route_direction == "in") "#16887e" else "#dd8766",
               width = .75) + coord_flip() +
      labs(x = NULL, y = "Individuals represented on IRS returns (N2)") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank(),
            plot.margin = margin(10, 15, 10, 5))
  })

  output$route_table <- renderDT({
    d <- route_data()
    tbl <- data.frame(
      `From → To` = paste(d$origin_state, "→", d$destination_state),
      `Returns (N1)` = d$returns,
      `Individuals (N2)` = d$individuals,
      `AGI ($ millions)` = round(d$agi_thousands / 1000, 1),
      check.names = FALSE
    )
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, dom = "t"))
  })

  output$state_comparison <- renderDT({
    d <- view_data()
    a <- d[d$abbr == input$state_a, , drop = FALSE]
    b <- d[d$abbr == input$state_b, , drop = FALSE]
    keys <- c("net_rate", "in_rate", "out_rate", "net_people",
              "population_growth_pct", "housing_burden_pct_2024",
              "median_income", "median_home_value", "median_rent",
              "median_rent_pct", "unemployment_pct", "poverty_pct",
              "commute_minutes")
    label <- c(unname(metric_labels[c("net_rate", "in_rate", "out_rate")]),
               "Net IRS represented individuals", unname(metric_labels[keys[5:13]]))
    show_val <- function(d, k) {
      if (k == "net_people") return(fmt_signed(d$net_people, 0))
      fmt_metric(d[[k]], k)
    }
    tbl <- data.frame(
      Measure = label,
      `First state` = vapply(keys, function(k) show_val(a, k), character(1)),
      `Second state` = vapply(keys, function(k) show_val(b, k), character(1)),
      check.names = FALSE
    )
    names(tbl)[2:3] <- c(a$state, b$state)
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 15, dom = "t", ordering = FALSE))
  })

  trend_data <- reactive({
    chosen <- c(input$trend_state_a, input$trend_state_b)
    s <- pop[pop$abbr %in% chosen, , drop = FALSE]
    if (input$trend_metric == "pop_index") {
      do.call(rbind, lapply(seq_len(nrow(s)), function(i) {
        data.frame(state = s$state[i], year = 2020:2025,
          value = 100 * (as.numeric(unlist(s[i, paste0("population_", 2020:2025)], use.names = FALSE)) /
                           s$population_2020[i] - 1))
      }))
    } else {
      a <- merge(acs_all, pop[, c("fips", "state", "abbr")], by = "fips")
      a <- a[a$abbr %in% chosen, , drop = FALSE]
      data.frame(state = a$state, year = a$acs_year,
                 value = a[[input$trend_metric]])
    }
  })

  output$trend_plot <- renderPlotly({
    d <- trend_data()
    d <- d[is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 0, "No matching annual values available."))
    d$tooltip <- paste0(d$state, " · ", d$year, ": ",
                        round(d$value, 2),
                        if (input$trend_metric == "pop_index") "%" else "")
    p <- ggplot(d, aes(x = year, y = value, color = state, group = state,
                       text = tooltip)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.9) +
      scale_color_manual(values = c("#16887e", "#dd8766")) +
      scale_x_continuous(breaks = sort(unique(d$year))) +
      labs(x = "Year", y = if (input$trend_metric == "pop_index")
             "Population change from 2020 (%)" else
               metric_labels[[input$trend_metric]], color = "State") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text", source = "trends") %>%
      layout(margin = list(l = 80, r = 15, b = 55, t = 15),
             paper_bgcolor = "white", plot_bgcolor = "white")
  })

  output$download_master <- downloadHandler(
    filename = function() paste0("american_migration_", input$irs_period,
                                 "_ACS", input$acs_year, ".csv"),
    content = function(file) write.csv(view_data(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
