# AMERICAN MIGRATION ATLAS — REVISED FOR THE TEAM'S CLEAN CSV DATA
# BIOL 1185 | Washington and Lee University
# Files read: 1 population growth, 6 ACS years, 8 IRS flows.
# The original uploaded dataset is included in the accompanying ZIP.
# Required packages: shiny, plotly, ggplot2, DT
# Run with RStudio's "Run App" after extracting the entire package.
# IRS route counts below are the fields named "exemptions" (N2 proxy)
# and not a Census count of every person who moved.

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

# -------------------- LOAD THE 15 PROVIDED CLEAN CSV FILES --------------------
repo_dir <- getwd()
read_source <- function(filename) {
  path <- file.path(repo_dir, filename)
  if (!file.exists(path)) stop("Missing CSV in your app folder: ", filename)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM", na.strings = c("NA", "", "N/A"))
}
as_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[$,% ]", "", as.character(x))))
}
canon_fips <- function(x) sprintf("%02d", as.integer(as_number(x)))
period_labels <- c("1920" = "2019–2020", "2021" = "2020–2021",
                   "2122" = "2021–2022", "2223" = "2022–2023")

# The new population file has only 2020 and 2025 endpoints, NOT 2021–2024.
raw_pop <- read_source("state_population_growth_clean.csv")
stopifnot(all(c("STATE", "NAME", "POPESTIMATE2020", "POPESTIMATE2025",
                "population_growth_pct") %in% names(raw_pop)))
pop <- raw_pop[raw_pop$NAME %in% state.name, , drop = FALSE]
pop <- data.frame(
  fips = canon_fips(pop$STATE), state = pop$NAME,
  abbr = state.abb[match(pop$NAME, state.name)],
  population_2020 = as_number(pop$POPESTIMATE2020),
  population_2025 = as_number(pop$POPESTIMATE2025),
  population_growth_pct = as_number(pop$population_growth_pct),
  stringsAsFactors = FALSE
)
pop <- pop[order(pop$state), , drop = FALSE]
stopifnot(nrow(pop) == 50L, !anyDuplicated(pop$fips), !anyNA(pop))
stopifnot(all(abs(pop$population_growth_pct -
                    100 * (pop$population_2025 - pop$population_2020) / pop$population_2020) < 0.011))

# Column names across the six ACS exports differ; identify each metric
# by a precise column match rather than positional assumptions.
find_col <- function(d, regex, label) {
  hits <- grep(regex, names(d), value = TRUE, ignore.case = TRUE, perl = TRUE)
  if (length(hits) != 1L)
    stop("ACS column error for ", label, ": ", length(hits),
         " matches (", paste(hits, collapse = " | "), ")")
  as_number(d[[hits]])
}

load_acs_year <- function(year) {
  filename <- sprintf("ACS_%d_state_new.csv", year)
  if (!file.exists(file.path(repo_dir, filename)) && year == 2024L) {
    filename <- "ACS_2024_state_new(1).csv"  # Original uploaded filename
  }
  d <- read_source(filename)
  d$fips_key <- canon_fips(d$FIPS)
  d <- d[d$fips_key %in% pop$fips, , drop = FALSE]
  stopifnot(nrow(d) == 50L, !anyDuplicated(d$fips_key))
  
  poverty_cols <- grep(
    "^Population for Whom Poverty Status Is Determined:.*(Under \\.50|\\.50 to \\.74|\\.75 to \\.99)$",
    names(d), value = TRUE, ignore.case = TRUE
  )
  if (length(poverty_cols) != 3L) {
    stop("Expected 3 below-poverty categories in ", filename,
         "; found ", length(poverty_cols))
  }
  poverty_below <- rowSums(sapply(d[poverty_cols], as_number), na.rm = FALSE)
  labor_force <- find_col(d,
                          "^Civilian Population in Labor Force 16 Years and Over:?$", "labor force")
  poverty_total <- find_col(d,
                            "^Population for Whom Poverty Status Is Determined:?$", "poverty total")
  safe_pct <- function(num, den) ifelse(is.finite(den) & den > 0,
                                        100 * num / den, NA_real_)
  
  out <- data.frame(
    fips = d$fips_key, acs_year = as.integer(year),
    median_income = find_col(d, "^Median Household Income", "median income"),
    median_home_value = find_col(d,
                                 "^(Median Value|Median House Value)$", "median home value"),
    median_rent = find_col(d, "^Median Gross Rent$", "median rent"),
    median_rent_pct = find_col(d,
                               "^Median Gross Rent as a Percentage", "median rent as % of income"),
    unemployment_pct = safe_pct(find_col(d,
                                         "^Civilian Population in Labor Force 16 Years and Over: Unemployed$",
                                         "unemployed"), labor_force),
    poverty_pct = safe_pct(poverty_below, poverty_total),
    commute_minutes = find_col(d, "^Average Commute to Work", "commute time"),
    stringsAsFactors = FALSE
  )
  out
}
acs_all <- do.call(rbind, lapply(2019:2024, load_acs_year))
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
  summary <- do.call(rbind, lapply(seq_len(nrow(pop)), function(i) {
    f <- pop$fips[i]
    arrivals <- flows[flows$destination_fips == f, , drop = FALSE]
    departures <- flows[flows$origin_fips == f, , drop = FALSE]
    # Each geography has 50 possible partners, including Washington, D.C.
    missing_in <- 50L - sum(is.finite(arrivals$individuals))
    missing_out <- 50L - sum(is.finite(departures$individuals))
    p_in <- sum(arrivals$individuals, na.rm = TRUE)
    p_out <- sum(departures$individuals, na.rm = TRUE)
    data.frame(
      fips = f, period = period,
      inflow_returns = sum(arrivals$returns, na.rm = TRUE),
      outflow_returns = sum(departures$returns, na.rm = TRUE),
      inflow_people = p_in, outflow_people = p_out,
      net_people = p_in - p_out,
      # Fixed denominator: only 2020/2025 population endpoints were provided.
      net_rate = 1000 * (p_in - p_out) / pop$population_2020[i],
      in_rate = 1000 * p_in / pop$population_2020[i],
      out_rate = 1000 * p_out / pop$population_2020[i],
      missing_in = missing_in, missing_out = missing_out,
      incomplete = (missing_in + missing_out) > 0L,
      stringsAsFactors = FALSE
    )
  }))
  list(totals = summary, flows = flows)
}
periods <- c("1920", "2021", "2122", "2223")
irs_data <- lapply(periods, load_irs_period)
irs_totals <- do.call(rbind, lapply(irs_data, `[[`, "totals"))
irs_flows <- do.call(rbind, lapply(irs_data, `[[`, "flows"))
stopifnot(nrow(irs_totals) == 200L)

# State abbreviations, FIPS, and all original state names remain unmodified.

# --------------- UI HELPERS / METRIC DEFINITIONS ---------------
metric_labels <- c(
  net_rate = "Net IRS exemptions per 1,000 (2020 population base)",
  in_rate = "Incoming exemptions per 1,000 (2020 base)",
  out_rate = "Outgoing exemptions per 1,000 (2020 base)",
  net_people = "Net reported IRS exemptions (N2)",
  inflow_people = "Incoming IRS exemptions (N2)",
  outflow_people = "Outgoing IRS exemptions (N2)",
  population_growth_pct = "Population growth, 2020–2025 (%)",
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
  "Population change: 2020 vs. 2025 only" = "pop_index",
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
  div(class = "atlas-card", h3(title),
      if (!is.null(subtitle)) p(class = "atlas-sub", subtitle), ...)
}
stat_box <- function(label, value, detail = "") {
  div(class = "stat-card", div(class = "stat-label", label),
      div(class = "stat-value", value), div(class = "stat-sub", detail))
}

# Team biographies and headshots. Images are provided under www/ in the download
# package. R Shiny serves www/ files automatically.
team_card <- function(photo_src, name, discipline, bio, linkedin) {
  div(class = "team-card",
      div(class = "team-photo-shell",
          tags$img(class = "team-photo", src = photo_src, alt = name)
      ),
      h3(class = "team-name", name),
      div(class = "team-discipline", discipline),
      p(class = "team-bio", bio),
      tags$a(class = "team-link", href = linkedin,
             target = "_blank", rel = "noopener noreferrer",
             "View LinkedIn profile ↗")
  )
}

app_css <- "
body {background:#f5f7f7;color:#1d3440;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}
.atlas-shell {display:flex;min-height:100vh;margin:-15px;align-items:stretch;}
.atlas-sidebar {width:238px;min-width:238px;padding:25px 15px 20px;background:#112a39;color:#e1eeee;position:sticky;top:0;height:100vh;overflow-y:auto;z-index:1000;}
.atlas-brand {font-weight:800;font-size:22px;line-height:1.14;color:white;padding:7px 8px 23px;}
.atlas-brand small {display:block;color:#a2c2c8;font-weight:600;font-size:11px;letter-spacing:1px;margin-top:10px;}
.atlas-sidebar .action-button {background:transparent;border:0;border-radius:10px;color:#e1eeee;text-align:left;width:100%;padding:12px;margin:2px 0;box-shadow:none;font-weight:650;}
.atlas-sidebar .action-button:hover,.atlas-sidebar .action-button:focus {background:#244353;color:white;}
.atlas-sidebar label {color:#c1d3d6!important;font-size:11px;font-weight:750;letter-spacing:.35px;}
.atlas-sidebar .form-group {margin-bottom:14px;}
.atlas-sidebar .selectize-input {color:#223c48;}
.atlas-sidebar hr {border-color:#36505e;margin:16px 0;}
.atlas-sidebar .atlas-side-note {color:#a2c2c8;font-size:11px;line-height:1.6;}
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
/* USA choropleth: clear inputs and a generous full-width geographic canvas. */
.atlas-map-controls {max-width:390px;margin:3px 0 0;}
.atlas-map-controls label {font-size:11px;font-weight:800;letter-spacing:.6px;color:#567482;}
.atlas-map-note {font-size:12px;color:#6d8490;margin:0 0 9px;line-height:1.5;}
.atlas-map-note strong {color:#244355;}
.js-plotly-plot .plotly .modebar {opacity:.35;transition:opacity .15s;}
.js-plotly-plot .plotly .modebar:hover {opacity:1;}


/* About Us: three original teammate portraits, precisely circular-clipped. */
.about-eyebrow {font-size:11px;letter-spacing:2px;font-weight:800;text-transform:uppercase;color:#19857f;margin:0 0 8px;}
.team-grid {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:18px;margin:10px 0 22px;align-items:stretch;}
.team-card {background:white;border:1px solid #e2eaea;border-radius:15px;box-shadow:0 2px 12px rgba(10,36,44,.03);padding:27px;display:flex;flex-direction:column;min-height:420px;align-items:flex-start;}
.team-photo-shell {width:170px;height:170px;border-radius:50%;overflow:hidden;border:6px solid #f5f7f7;box-shadow:0 2px 12px rgba(10,36,44,.08);background:#eef3f4;margin:0 auto 22px;align-self:center;}
.team-photo {width:100%;height:100%;display:block;object-fit:cover;object-position:center center;}
.team-card .team-name {font-size:22px;font-weight:800;margin:0 0 4px;letter-spacing:-.6px;color:#193c47;}
.team-discipline {color:#15807a;font-size:12px;font-weight:800;letter-spacing:.2px;margin-bottom:12px;}
.team-card .team-bio {font-size:13px;line-height:1.75;color:#5c7581;margin-bottom:20px;}
.team-link {margin-top:auto;display:inline-block;align-self:flex-start;background:#0c7872;color:white!important;text-decoration:none!important;border-radius:9px;padding:11px 14px;font-size:12px;font-weight:750;}
.team-link:hover,.team-link:focus {background:#085e5a;color:white!important;}
@media(max-width:1120px){.team-grid{grid-template-columns:1fr 1fr;}}
@media(max-width:780px){.team-grid{grid-template-columns:1fr;}.team-card{min-height:0;}}
@media(max-width:700px) {.atlas-shell{display:block}.atlas-sidebar{position:relative;width:100%;height:auto;min-width:0;padding:16px}.atlas-main{padding:18px}.atlas-main h1{font-size:27px}.kpi-grid{grid-template-columns:repeat(2,1fr)}}
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
          actionButton("go_about", "◉  About us"),
          actionButton("go_methods", "ⓘ  Data & methods"),
          tags$hr(),
          selectInput("irs_period", "IRS MIGRATION PERIOD",
                      choices = stats::setNames(names(period_labels), period_labels),
                      selected = "2223"),
          selectInput("acs_year", "ACS REFERENCE YEAR",
                      choices = as.character(2019:2024), selected = "2024"),
          div(class = "atlas-side-note",
              "50 states · Four IRS migration periods · Six ACS reference years")
      ),
      div(class = "atlas-main",
          tabsetPanel(id = "page", type = "hidden",
                      tabPanel(title = "Overview", value = "overview",
                               h1("Where is America moving?"),
                               p(class = "atlas-lede",
                                 "Explore population change and matched IRS interstate migration. Select a metric or click a state on the interactive U.S. map."),
                               uiOutput("overview_kpis"),
                               ui_card("United States, state by state",
                                       subtitle = "A geographic U.S. choropleth. Darker blue indicates higher values; coral indicates negative reported migration balances.",
                                       div(class = "atlas-map-controls",
                                           selectInput("map_metric", "COLOR STATES BY",
                                                       choices = map_choices, selected = "net_rate")
                                       ),
                                       plotlyOutput("state_map", height = "550px"),
                                       uiOutput("map_source_note"),
                                       uiOutput("selected_details")
                               ),
                               ui_card("States gaining & losing",
                                       subtitle = "Reported net IRS exemptions per 1,000 people, normalized to 2020 population.",
                                       plotOutput("net_rank_plot", height = "440px")
                               )
                      ),
                      tabPanel(title = "Explore factors", value = "explore",
                               h1("Explore the relationships"),
                               p(class = "atlas-lede", "Choose a potential explanatory factor and a migration or population outcome. Points represent states—not individual movers."),
                               ui_card("Ask your own research question",
                                       div(class = "atlas-control-row",
                                           selectInput("x_metric", "X-axis: explanatory factor",
                                                       choices = x_choices, selected = "median_rent_pct"),
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
                                   ui_card("Top 10 interstate routes", plotOutput("route_bar", height = "460px")),
                                   ui_card("Detailed route records",
                                           subtitle = "N1 = tax returns; N2 = exemptions (a proxy for represented persons); AGI in millions of dollars.",
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
                                       DTOutput("state_comparison")
                               )
                      ),
                      tabPanel(title = "Trends", value = "trends",
                               h1("Track changes over time"),
                               p(class = "atlas-lede", "Compare four annual IRS migration periods, ACS reference years 2019–2024, or the two available population estimates (2020 and 2025)."),
                               ui_card("Time-series comparison",
                                       div(class = "atlas-control-row",
                                           selectInput("trend_metric", "Measure", choices = trend_choices,
                                                       selected = "net_people"),
                                           selectInput("trend_state_a", "First state", state_choices, selected = "FL"),
                                           selectInput("trend_state_b", "Second state", state_choices, selected = "CA")
                                       ),
                                       plotlyOutput("trend_plot", height = "480px"),
                                       div(class = "atlas-alert", "ACS estimates may represent overlapping multi-year periods and monetary values use their own survey-year inflation bases. Population is available for 2020 and 2025 only. IRS per-1,000 values use the fixed 2020 population denominator.")
                               )
                      ),
                      tabPanel(title = "About Us", value = "about",
                               div(class = "about-eyebrow", "THE PEOPLE BEHIND THE DATA"),
                               h1("Meet the team"),
                               p(class = "atlas-lede",
                                 "We are students at Washington and Lee University bringing together interests in economics, neuroscience, engineering and computer science to investigate patterns of migration across the United States."),
                               div(class = "team-grid",
                                   team_card(
                                     "lucas-kim.png", "Lucas Kim", "Economics · Class of 2027",
                                     paste0(
                                       "Lucas studies economics with a minor in Latin American and Caribbean Studies. ",
                                       "His interests in environmental economics and public policy offer perspective ",
                                       "on economic opportunity and regional conditions."
                                     ),
                                     "https://www.linkedin.com/in/lucas-kim-2b321a28a/"
                                   ),
                                   team_card(
                                     "lucas-chacon.png", "Lucas Chacon", "Neuroscience · Class of 2027",
                                     paste0(
                                       "Lucas studies neuroscience and is a varsity swimmer. His behavioral science ",
                                       "research includes human behavior, research data analysis and visualization ",
                                       "using RStudio."
                                     ),
                                     "https://www.linkedin.com/in/lucas-chacon-0812a3360/"
                                   ),
                                   team_card(
                                     "diane-salazar.png", "Diane Salazar", "Integrated Engineering & Computer Science · Class of 2027",
                                     paste0(
                                       "Diane studies integrated engineering and computer science. She has applied ",
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
                                           tags$ul(
                                             tags$li(tags$b("Population endpoints 2020 and 2025: "),
                                                     "state_population_growth_clean.csv"),
                                             tags$li(tags$b("ACS reference years 2019–2024: "),
                                                     "ACS_2019_state_new.csv through ACS_2024_state_new(1).csv"),
                                             tags$li(tags$b("IRS periods 2019–20, 2020–21, 2021–22, 2022–23: "),
                                                     "state_inflow_clean_*.csv and state_outflow_clean_*.csv")
                                           ),
                                           downloadButton("download_master", "Export selected state dataset (CSV)")
                                   ),
                                   ui_card("How we calculate metrics",
                                           tags$ul(
                                             tags$li("Net IRS exemptions (N2) = reported incoming exemptions minus reported outgoing exemptions."),
                                             tags$li("Rates per 1,000 = IRS exemptions divided by the state's 2020 population, multiplied by 1,000. All four IRS periods use this same fixed base."),
                                             tags$li("Population growth (%) = (2025 population − 2020 population) / 2020 population × 100. The supplied growth column is rounded."),
                                             tags$li("ACS unemployment (%) = unemployed civilians / civilian labor force × 100."),
                                             tags$li("ACS poverty (%) = respondents in three below-1.00 poverty-ratio bands / relevant population × 100."),
                                             tags$li("Home-to-income ratio = median home value / median household income."),
                                             tags$li("The median gross rent percentage is an ACS field; it is not equivalent to a 30%-plus housing-cost burden rate.")
                                           )
                                   )
                               ),
                               ui_card("Important interpretation and data-quality notes",
                                       tags$ul(
                                         tags$li("Both IRS inflow and outflow files describe the same routes. We analyze each pair once rather than add both source files."),
                                         tags$li("Migration entries use the source field 'exemptions' (N2), not a complete count of all people moving. Some pairs are suppressed or omitted, so reported state totals can be incomplete."),
                                         tags$li("The route universe includes 50 states and Washington, D.C.; maps and state comparisons cover the 50 states only."),
                                         tags$li("AGI is measured in thousands of dollars. Some paired IRS files differ by $1,000 of AGI for a few historic routes; the inflow view is used consistently."),
                                         tags$li("We have no population denominators for 2019, 2021, 2022 or 2023. Rate per 1,000 is therefore a 2020-population-normalized comparison, not an official year-specific rate."),
                                         tags$li("Only 2020 and 2025 population estimates are available. No intermediate annual population points are fabricated."),
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

# --------------- SHINY SERVER: REACTIVE DATA AND PLOTS ---------------
server <- function(input, output, session) {
  for (page_name in c("overview", "explore", "routes", "compare", "trends", "about", "methods")) {
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
                list(pop, a, m))
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
    d$map_hover <- paste0(
      "<b>", d$state, " (", d$abbr, ")</b>",
      "<br>", metric_label, ": <b>",
      vapply(seq_len(nrow(d)), function(i)
        fmt_metric(d$value[i], input$map_metric), character(1)),
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
        c(0, "#d98878"), c(0.5, "#f0f4f8"), c(1, "#2459aa")
      )
    } else {
      values <- d$value[is.finite(d$value)]
      req(length(values) > 0L)
      scale_min <- min(values)
      scale_max <- max(values)
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
    
    plot_ly(
      data = d,
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
        tickfont = list(size = 11, color = "#556b7a"),
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
        font = list(family = "Arial, sans-serif", color = "#254252")
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE, scrollZoom = FALSE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"))
  })
  
  output$map_source_note <- renderUI({
    label <- if (input$map_metric %in% c("median_income", "median_home_value",
                                         "unemployment_pct", "poverty_pct",
                                         "median_rent_pct")) {
      paste0("ACS 5-year period: ", as.integer(input$acs_year) - 4L,
             "–", input$acs_year,
             " (survey estimate, not a monthly BLS figure)")
    } else if (input$map_metric == "population_growth_pct") {
      "Population growth reference: 2020–2025"
    } else {
      paste0("IRS migration reference: ", period_labels[[input$irs_period]],
             "; rates use 2020 population base")
    }
    n_partial <- sum(view_data()$incomplete)
    div(class = "atlas-map-note",
        tags$strong("Data period: "), label,
        " · Click any state for details.",
        if (n_partial > 0L) paste0(" · Data caution: ", n_partial,
                                   " states have at least one unreported IRS route in this selected period."))
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
    s <- d[d$abbr == selected_state(), , drop = FALSE]
    if (!nrow(s)) return(NULL)
    div(class = "atlas-alert",
        tags$b(paste0(s$state, " (", s$abbr, ")")), tags$br(),
        "Reported net IRS exemptions: ", tags$b(fmt_signed(s$net_people, 0)), " · ",
        "2020–25 growth: ", tags$b(paste0(fmt_signed(s$population_growth_pct, 2), "%")),
        if (s$incomplete) tags$span(" · Incomplete route data: ",
                                    tags$b(s$missing_in + s$missing_out), " suppressed/missing inbound or outbound records")
    )
  })
  
  output$net_rank_plot <- renderPlot({
    d <- view_data()
    d <- d[order(d$net_rate, decreasing = TRUE), ]
    take <- rbind(head(d, 6), tail(d, 6))
    take$kind <- ifelse(take$net_rate >= 0, "Net gain", "Net loss")
    ggplot(take, aes(x = reorder(state, net_rate), y = net_rate,
                     fill = kind)) +
      geom_col(width = .7) + coord_flip() +
      scale_fill_manual(values = c("Net gain" = "#16887e", "Net loss" = "#dd8766")) +
      labs(x = NULL, y = "Net IRS exemptions per 1,000 (2020 population base)") +
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
    head(d[is.finite(d$individuals) & is.finite(d$returns), , drop = FALSE][
      order(-d$individuals[is.finite(d$individuals) & is.finite(d$returns)]),
      , drop = FALSE], 10)
  })
  
  output$route_kpis <- renderUI({
    s <- view_data()
    s <- s[s$abbr == input$route_state, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    div(class = "kpi-grid",
        stat_box("Incoming IRS exemptions (N2)", fmt_num(s$inflow_people)),
        stat_box("Outgoing IRS exemptions (N2)", fmt_num(s$outflow_people)),
        stat_box("Net reported exemptions", fmt_signed(s$net_people, 0),
                 if (s$incomplete) "Partial: missing/suppressed routes" else "All 50 routes reported"),
        stat_box("Net per 1,000 (2020 base)", fmt_signed(s$net_rate, 2))
    )
  })
  
  output$route_bar <- renderPlot({
    d <- route_data()
    validate(need(nrow(d) > 0, "No valid interstate route records available."))
    ggplot(d, aes(x = reorder(other_state, individuals), y = individuals)) +
      geom_col(fill = if (input$route_direction == "in") "#16887e" else "#dd8766",
               width = .75) + coord_flip() +
      labs(x = NULL, y = "Reported IRS exemptions (N2)") +
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
      `Exemptions (N2)` = d$individuals,
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
    req(nrow(a) == 1L, nrow(b) == 1L)
    keys <- c("net_rate", "in_rate", "out_rate", "net_people",
              "inflow_people", "outflow_people", "population_growth_pct",
              "median_income", "median_home_value", "median_rent",
              "median_rent_pct", "unemployment_pct", "poverty_pct",
              "commute_minutes")
    show_val <- function(d, k) fmt_metric(d[[k]], k)
    tbl <- data.frame(
      Measure = unname(metric_labels[keys]),
      `First state` = vapply(keys, function(k) show_val(a, k), character(1)),
      `Second state` = vapply(keys, function(k) show_val(b, k), character(1)),
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
      # We only have 2020/2025; do not invent intervening values.
      s <- pop[pop$abbr %in% chosen, , drop = FALSE]
      return(do.call(rbind, lapply(seq_len(nrow(s)), function(i) {
        data.frame(state = s$state[i], year = c(2020, 2025),
                   value = c(0, s$population_growth_pct[i]))
      })))
    }
    if (key %in% c("net_people", "net_rate", "inflow_people", "outflow_people")) {
      m <- merge(irs_totals, pop[, c("fips", "state", "abbr")], by = "fips")
      m <- m[m$abbr %in% chosen, , drop = FALSE]
      ends <- c("1920" = 2020L, "2021" = 2021L,
                "2122" = 2022L, "2223" = 2023L)
      return(data.frame(state = m$state, year = unname(ends[m$period]),
                        value = m[[key]], incomplete = m$incomplete,
                        stringsAsFactors = FALSE))
    }
    a <- merge(acs_all, pop[, c("fips", "state", "abbr")], by = "fips")
    a <- a[a$abbr %in% chosen, , drop = FALSE]
    data.frame(state = a$state, year = a$acs_year, value = a[[key]],
               stringsAsFactors = FALSE)
  })
  
  output$trend_plot <- renderPlotly({
    d <- trend_data()
    d <- d[is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 0, "No observations available for this indicator."))
    d$tooltip <- paste0(d$state, " · ", d$year, ": ",
                        fmt_num(d$value, 2),
                        if (input$trend_metric == "pop_index") "%" else "")
    p <- ggplot(d, aes(x = year, y = value, color = state, group = state,
                       text = tooltip)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.9) +
      scale_color_manual(values = c("#16887e", "#dd8766")) +
      scale_x_continuous(breaks = sort(unique(d$year))) +
      labs(x = if (input$trend_metric %in% c("net_people", "net_rate",
                                             "inflow_people", "outflow_people")) "IRS period end year" else "Year",
           y = if (input$trend_metric == "pop_index")
             "Population change from 2020 (%)" else metric_labels[[input$trend_metric]],
           color = "State") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text", source = "trends") %>%
      layout(margin = list(l = 85, r = 15, b = 55, t = 15),
             paper_bgcolor = "white", plot_bgcolor = "white")
  })
  
  output$download_master <- downloadHandler(
    filename = function() paste0("american_migration_clean_", input$irs_period,
                                 "_ACS", input$acs_year, ".csv"),
    content = function(file) write.csv(view_data(), file, row.names = FALSE)
  )
}

# -------------------- INTEGRATED DESIGN IMPROVEMENTS --------------------
# All six design updates are embedded below; no external update file is needed.
# Original data loading and calculations above are preserved.

stopifnot(exists("ui"), is.function(server), exists("metric_labels"))
old_metric_labels <- metric_labels
metric_labels[c("net_rate", "in_rate", "out_rate", "net_people",
                "inflow_people", "outflow_people")] <- c(
                  "Net migration proxy per 1,000", "Incoming migration proxy per 1,000",
                  "Outgoing migration proxy per 1,000", "Net migration proxy (N2)",
                  "Incoming migration proxy (N2)", "Outgoing migration proxy (N2)")

atlas_definition <- paste(
  "Migration proxy = IRS exemptions (N2), representing people on tax returns,",
  "not all movers. Net = incoming minus outgoing.",
  "All per-1,000 values divide N2 by the state's 2020 population and multiply by 1,000.")
atlas_irs_keys <- c("net_rate", "in_rate", "out_rate", "net_people",
                    "inflow_people", "outflow_people")

# Revise the already-built Shiny tag tree, preserving inputs and output IDs.
atlas_text <- function(x) {
  for (key in names(metric_labels))
    x[x == old_metric_labels[[key]]] <- metric_labels[[key]]
  x[x == "X-axis: explanatory factor"] <- "X-axis: state-level factor"
  x[x == "Detailed route records"] <- "All available route records"
  x[x == "Reported net IRS exemptions per 1,000 people, normalized to 2020 population."] <-
    "Six highest and six lowest reported net migration proxies. * marks partial route data."
  x[x == "Choose a potential explanatory factor and a migration or population outcome. Points represent states—not individual movers."] <-
    "Choose a state-level factor and an outcome. Check the reporting periods below; points represent states, not individual movers."
  x
}
atlas_note_ids <- c(state_map = "atlas_map_period", net_rank_plot = "atlas_rank_period",
                    scatter_plot = "atlas_scatter_period", route_bar = "atlas_route_period",
                    trend_plot = "atlas_trend_period", state_comparison = "atlas_compare_period",
                    route_table = "atlas_table_period", correlation_table = "atlas_corr_period")
atlas_revise_ui <- function(x) {
  if (inherits(x, "shiny.tag")) {
    x$children <- lapply(x$children, atlas_revise_ui)
    id <- x$attribs$id
    if (!is.null(id) && id %in% names(atlas_note_ids)) {
      return(tagList(uiOutput(unname(atlas_note_ids[[id]])), x))
    }
    if (!is.null(id) && id == "go_overview") {
      x$attribs$class <- paste(x$attribs$class, "atlas-active")
      x$attribs[["aria-current"]] <- "page"
    }
    return(x)
  }
  if (is.list(x)) { x[] <- lapply(x, atlas_revise_ui); return(x) }
  if (is.character(x) && !inherits(x, "html")) return(atlas_text(x))
  x
}
ui <- atlas_revise_ui(ui)
ui <- tagList(ui, tags$head(tags$style(HTML("
.atlas-sidebar .action-button.atlas-active {background:#28605f;color:#fff;border-left:4px solid #72d5c5;padding-left:8px;}
.atlas-sidebar .action-button:focus-visible {outline:3px solid #b6e7de;outline-offset:2px;}
.atlas-period-note {font-size:12px;line-height:1.6;color:#405e6c;margin:4px 0 12px;overflow-wrap:anywhere;}
.atlas-period-note .atlas-definition {display:block;font-size:11px;color:#586f7b;margin-top:4px;}
.atlas-period-note .atlas-warning {display:block;color:#8a471e;font-weight:650;margin-top:5px;}
.two-even > *, .two-panels > * {min-width:0;}
.atlas-card {min-width:0;}
.atlas-main .dataTables_wrapper {width:100%;overflow-x:auto;}
@media(max-width:1100px){.two-even,.two-panels{grid-template-columns:minmax(0,1fr);}.kpi-grid{grid-template-columns:repeat(2,minmax(0,1fr));}}
@media(max-width:700px){.atlas-card{padding:15px;}.atlas-control-row{display:block;}.atlas-control-row .form-group{min-width:0;width:100%;}.atlas-sidebar{position:relative;z-index:auto;}.stat-value{font-size:22px;}}
@media(max-width:420px){.kpi-grid{grid-template-columns:1fr;}.atlas-main{padding:12px;}}
")), tags$script(HTML("
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('atlas-active-page', function(page) {
    $('.atlas-sidebar .action-button').removeClass('atlas-active').removeAttr('aria-current');
    $('#go_' + page).addClass('atlas-active').attr('aria-current', 'page');
  });
});
"))))

# Append replacements inside the original server's scope, so its existing
# reactives, click handlers and data downloads remain available.
atlas_server_updates <- quote({
  observe({
    req(input$page)
    session$sendCustomMessage("atlas-active-page", input$page)
  })
  atlas_period <- function(key) {
    if (key %in% atlas_irs_keys)
      return(paste("IRS migration:", unname(period_labels[[input$irs_period]])))
    if (key %in% c("population_growth_pct", "pop_index"))
      return("Population endpoints: 2020 and 2025")
    paste("ACS reference year:", input$acs_year,
          "(confirm survey product and coverage in source metadata)")
  }
  atlas_note <- function(keys, extra = NULL) {
    has_irs <- any(keys %in% atlas_irs_keys)
    div(class = "atlas-period-note",
        tags$strong(paste(unique(vapply(keys, atlas_period, character(1))), collapse = " | ")),
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
  output$atlas_map_period <- renderUI(atlas_note(input$map_metric))
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
    } else if (key == "pop_index") {
      div(class = "atlas-period-note", tags$strong("Population endpoints: 2020 and 2025 only."),
          " The connecting line joins the two observations; it does not show annual estimates.")
    } else {
      div(class = "atlas-period-note", tags$strong("ACS reference years: 2019–2024."),
          " Confirm survey coverage in source metadata; estimates may overlap. Dollar values use their survey-year bases. Sidebar year selections do not filter this chart.")
    }
  })
  # Remove the unconfirmed assertion that the ACS extracts are five-year data.
  output$map_source_note <- renderUI({
    n <- sum(view_data()$incomplete)
    div(class = "atlas-map-note", "Click a state for details.",
        if (input$map_metric %in% atlas_irs_keys && n > 0)
          paste(n, "states have partial IRS totals; hover over states for flags."))
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
                 marker = list(color = ifelse(d$abbr == selected_state(), "#da7b55", "#16887e"),
                               symbol = ifelse(partial, "diamond-open", "circle"), size = 11,
                               line = list(width = 1.5)), source = "scatter", showlegend = FALSE)
    p <- add_lines(p, data = line_d, x = ~x, y = ~y, inherit = FALSE,
                   hoverinfo = "skip", showlegend = FALSE,
                   line = list(color = "#dd8766", dash = "dash", width = 3))
    layout(p, xaxis = list(title = metric_labels[[input$x_metric]], automargin = TRUE),
           yaxis = list(title = metric_labels[[input$y_metric]], automargin = TRUE),
           margin = list(l = 70, r = 15, b = 85, t = 15),
           paper_bgcolor = "white", plot_bgcolor = "white") %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
  output$net_rank_plot <- renderPlot({
    d <- view_data()
    d <- d[order(d$net_rate, decreasing = TRUE), ]
    take <- rbind(head(d, 6), tail(d, 6))
    take$label <- paste0(take$state, ifelse(take$incomplete, " *", ""))
    take$kind <- ifelse(take$net_rate >= 0, "Net gain", "Net loss")
    ggplot(take, aes(reorder(label, net_rate), net_rate, fill = kind)) +
      geom_col(width = .7) + coord_flip() +
      scale_fill_manual(values = c("Net gain" = "#16887e", "Net loss" = "#dd8766")) +
      labs(x = NULL, y = metric_labels[["net_rate"]], caption = "* Partial IRS route data") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", panel.grid.major.y = element_blank())
  })
  
  # The full table and top-ten chart now have deliberately separate datasets.
  atlas_all_routes <- reactive({
    d <- irs_flows[irs_flows$period == input$irs_period, , drop = FALSE]
    f <- pop$fips[match(input$route_state, pop$abbr)]
    req(length(f) == 1L, !is.na(f))
    if (input$route_direction == "in") {
      d <- d[d$destination_fips == f, , drop = FALSE]
      d$other_state <- d$origin_state
    } else {
      d <- d[d$origin_fips == f, , drop = FALSE]
      d$other_state <- d$destination_state
    }
    d[order(d$individuals, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })
  route_data <- reactive({
    d <- atlas_all_routes()
    head(d[is.finite(d$individuals), , drop = FALSE], 10)
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
  output$trend_plot <- renderPlotly({
    d <- trend_data()
    d <- d[is.finite(d$value), , drop = FALSE]
    validate(need(nrow(d) > 0, "No observations available for this indicator."))
    key <- input$trend_metric
    d$partial <- FALSE
    d$tooltip <- paste0(d$state, " · ", d$year, ": ", fmt_num(d$value, 2))
    if (key %in% atlas_irs_keys) {
      m <- merge(irs_totals, pop[, c("fips", "state")], by = "fips")
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
      d$tooltip <- paste0(d$tooltip, "% change from 2020")
    } else {
      d$tooltip <- paste0(d$state, "<br>ACS reference year: ", d$year,
                          "<br>", metric_labels[[key]], ": ", fmt_num(d$value, 2))
    }
    d <- d[order(d$state, d$year), ]
    p <- ggplot(d, aes(year, value, color = state, group = state, text = tooltip)) +
      geom_line(linewidth = 1.1) + geom_point(aes(shape = partial), size = 3.2) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 5),
                         breaks = c(FALSE, TRUE), labels = c("Complete routes", "Partial routes"),
                         name = "IRS coverage") +
      scale_color_manual(values = c("#16887e", "#dd8766")) +
      scale_x_continuous(breaks = sort(unique(d$year))) +
      labs(x = if (key %in% atlas_irs_keys) "IRS period end year" else "Year",
           y = if (key == "pop_index") "Population change from 2020 (%)" else metric_labels[[key]],
           color = "State") + theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    if (!any(d$partial)) p <- p + guides(shape = "none")
    ggplotly(p, tooltip = "text", source = "trends") %>%
      layout(margin = list(l = 75, r = 15, b = 65, t = 15)) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  })
})
body(server) <- as.call(c(as.list(body(server)), list(atlas_server_updates)))


shinyApp(ui = ui, server = server)
