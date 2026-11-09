# =============================================================================
# atlas_ui.R  —  PAGE LAYOUT (what the website looks like)
# American Migration Atlas | BIOL 1185 | Washington and Lee University
# =============================================================================
#
# WHAT THIS FILE DOES
#   Describes the layout of the website: the sidebar, the controls, and the
#   seven pages ("tabs"). It does NOT read data or draw charts. It only places
#   empty "slots" on the page, and atlas_server.R fills each slot.
#
# HOW SLOTS WORK
#   plotlyOutput("state_map")   <- an empty slot named "state_map" in this file
#   output$state_map <- ...     <- the code in atlas_server.R that fills it
#   The names must match exactly. Likewise, selectInput("x_metric", ...) here
#   becomes input$x_metric in atlas_server.R.
#
# EACH TAB HAS ITS OWN FUNCTION (find the tab you want to change):
#   Tab on the website      Function here               Section
#   ---------------------   -------------------------   -------
#   Overview                overview_tab()              4
#   Explore factors         explore_factors_tab()       5
#   Migration routes        migration_routes_tab()      6
#   Compare states          compare_states_tab()        7
#   Trends                  trends_tab()                8
#   About us                about_tab()                 9
#   Data & methods          data_and_methods_tab()      10
#
# TABLE OF CONTENTS
#   1. Reusable layout pieces (cards, summary boxes, ...)
#   2. Shared page parts (sidebar, period controls, page header)
#   3. Text blocks used in more than one place
#   4-10. One function per tab (see table above)
#   11. Styling: CSS (colors, fonts, spacing) and JavaScript
#   12. Putting the page together  <-- the final `ui` object is built here
#
# OBJECTS THIS FILE BORROWS FROM atlas_server.R (loaded first by app.R):
# irs_period_labels, irs_metric_keys, data_source_websites, map_choices, x_axis_choices,
#   y_axis_choices, trend_choices, state_choices, source_groups, csv_download_id
# =============================================================================


# ---------------------------------------------------------------------------
# 1. REUSABLE LAYOUT PIECES
# ---------------------------------------------------------------------------

# A titled section with a thin line above it. Anything after `title` goes inside.
section_card <- function(title, ..., subtitle = NULL) {
  tags$section(class = "atlas-card", h2(title),
               if (!is.null(subtitle)) p(class = "atlas-sub", subtitle), ...)
}

# A big-number summary box (label, value, small detail line).
# NOTE: atlas_server.R also uses this function to draw its summary boxes.
stat_box <- function(label, value, detail = "") {
  div(class = "stat-card", div(class = "stat-label", label),
      div(class = "stat-value", value), div(class = "stat-sub", detail))
}

# Builds a JavaScript test such as  ['a', 'b'].indexOf(input.y_metric) !== -1
# Used by conditionalPanel() to show something only when a dropdown has one of
# the listed values selected.
input_is_one_of <- function(input_id, allowed_values) {
  paste0("[", paste0("'", allowed_values, "'", collapse = ", "), "].indexOf(input.",
         input_id, ") !== -1")
}

# One person on the About page: photo, name, major, short bio, LinkedIn link.
team_card <- function(photo_id, name, discipline, bio, linkedin_url) {
  div(class = "team-card",
      div(class = "team-photo-shell",
          imageOutput(photo_id, width = "100%", height = "100%")),
      h3(class = "team-name", name),
      div(class = "team-discipline", discipline),
      p(class = "team-bio", bio),
      tags$a(class = "team-link", href = linkedin_url,
             target = "_blank", rel = "noopener noreferrer",
             "View LinkedIn profile"))
}

# A bulleted list of download links for the original CSV files.
source_links <- function(filenames) {
  tags$ul(class = "atlas-file-list", lapply(filenames, function(filename) {
    tags$li(downloadLink(csv_download_id(filename), filename))
  }))
}


# ---------------------------------------------------------------------------
# 2. SHARED PAGE PARTS (appear around the tabs)
# ---------------------------------------------------------------------------

# Left sidebar: the project name and one button per page.
# The button ids (go_overview, ...) are handled in section A of atlas_server.R.
sidebar_navigation <- function() {
  div(class = "atlas-sidebar", role = "navigation",
      "aria-label" = "Atlas navigation and data selection",
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
      actionButton("go_methods", "Data & methods"))
}

# The "IRS migration period" and "ACS reference year" dropdowns shown above
# most tabs. They choose which year/period of data every tab uses.
# (The Overview tab adds one more dropdown here: its "Map measure" menu.)
reporting_controls_bar <- function() {
  conditionalPanel(
    "input.page != 'about' && input.page != 'methods'",
    div(class = "atlas-reporting-controls", role = "group",
        "aria-label" = "Reporting periods and map measure",
        selectInput("irs_period", "IRS migration period",
                    choices = stats::setNames(names(irs_period_labels), irs_period_labels),
                    selected = "2223"),
        selectInput("acs_year", "ACS reference year",
                    choices = as.character(2019:2024), selected = "2024"),
        overview_map_measure_menu()))
}


# ---------------------------------------------------------------------------
# 3. TEXT BLOCKS USED IN MORE THAN ONE PLACE
# ---------------------------------------------------------------------------

exemption_example <- paste(
  "Two parents and their two children move from Virginia to South Carolina.",
  "All four are listed on one tax return.",
  "For this move, the IRS migration data count 1 tax return and 4 exemptions, one for each person.")

partial_irs_heading <- "What does a partial IRS total mean?"
partial_irs_text <- paste(
  "At least one route into or out of the state has no reported count. The total adds up the routes we have.",
  "For example, if the Virginia-to-South Carolina count is missing, we don’t know how many people made that move. Missing does not mean zero.")


# =============================================================================
# 4. OVERVIEW TAB
# =============================================================================
# Shows: US map colored by a chosen measure, a summary strip for the clicked
# state, four national summary boxes, and a bar chart of top/bottom states.
#
# Slots filled by atlas_server.R (section C):
#   map_period_note, state_map, map_source_note, selected_details,
#   overview_kpis, ranking_period_note, net_rank_plot

# Page title. Sits ABOVE the shared period controls, so it is built separately.
overview_page_header <- function() {
  conditionalPanel(
    "input.page == 'overview'",
    tags$header(class = "atlas-page-header",
                h1("U.S. migration overview"),
                p(class = "atlas-lede",
                  "Explore state-level migration and population change. Select a measure, then click a state for details.")))
}

# The "Map measure" dropdown. It is placed inside the shared controls bar
# (section 2) but belongs to this tab, so it only shows on the Overview page.
overview_map_measure_menu <- function() {
  conditionalPanel(
    "input.page == 'overview'",
    selectInput("map_metric", "Map measure", choices = map_choices, selected = "net_per_1000"))
}

overview_tab <- function() {
  tabPanel(
    title = "Overview", value = "overview",

    # --- Map section ---
    tags$section(
      class = "atlas-card atlas-map-section",
      uiOutput("map_period_note"),
      plotlyOutput("state_map", height = "470px"),
      uiOutput("map_source_note"),
      # Show the exemption explainer only for IRS-based map measures.
      conditionalPanel(
        input_is_one_of("map_metric", c("net_per_1000", "net_exemptions",
                                        "inflow_per_1000", "outflow_per_1000")),
        tags$details(tags$summary("How IRS exemptions work: example"),
                     p(exemption_example))),
      uiOutput("selected_details")),

    # --- National summary boxes ---
    h2(class = "atlas-summary-heading", "National summary · selected reporting periods"),
    uiOutput("overview_kpis"),

    # --- Top/bottom states bar chart ---
    section_card(
      "States gaining & losing",
      subtitle = "Six highest and six lowest reported net migration proxies. * marks partial route data.",
      uiOutput("ranking_period_note"),
      plotOutput("net_rank_plot", height = "440px")))
}


# =============================================================================
# 5. EXPLORE FACTORS TAB
# =============================================================================
# Shows: a scatterplot (one dot per state) relating a state-level factor
# (x-axis, e.g. rent) to a migration outcome (y-axis), with correlation
# statistics and a table ranking all factors by correlation strength.
#
# Slots filled by atlas_server.R (section D):
#   scatter_period_note, scatter_plot, scatter_statistics, download_scatter,
#   correlation_period_note, correlation_table

explore_factors_tab <- function() {
  # The y-axis choices that come from IRS data (these trigger partial-data notes).
  outcome_is_irs_condition <- input_is_one_of("y_metric", irs_metric_keys)

  tabPanel(
    title = "Explore factors", value = "explore",
    h1("Explore state-level relationships"),
    p(class = "atlas-lede",
      "Choose a state-level factor and an outcome. Check the reporting periods below; points represent states, not individual movers."),

    # --- Scatterplot card ---
    section_card(
      "Compare a factor and an outcome",
      div(class = "atlas-control-row",
          selectInput("x_metric", "X-axis: state-level factor",
                      choices = x_axis_choices, selected = "rent_share_of_income_pct"),
          selectInput("y_metric", "Y-axis: outcome",
                      choices = y_axis_choices, selected = "net_per_1000")),
      uiOutput("scatter_period_note"),
      plotlyOutput("scatter_plot", height = "460px"),

      # Legend explaining the dots and the line.
      div(class = "factor-key", role = "group", "aria-label" = "Scatterplot legend",
          tags$strong("How to read the plot"),
          tags$span(tags$i(class = "key-dot", "aria-hidden" = "true"), "One state"),
          conditionalPanel(
            outcome_is_irs_condition,
            tags$span(tags$i(class = "key-partial", "aria-hidden" = "true"),
                      "Open diamond = partial IRS data")),
          tags$span(tags$i(class = "key-fit", "aria-hidden" = "true"),
                    "Dashed line = linear fit")),
      conditionalPanel(
        outcome_is_irs_condition,
        tags$details(tags$summary(partial_irs_heading), p(partial_irs_text))),

      uiOutput("scatter_statistics"),
      downloadButton("download_scatter", "Download plotted state data (CSV)"),
      div(class = "atlas-alert",
          "A correlation is an association, not proof that an economic condition caused people to move.")),

    # --- Correlation ranking card ---
    section_card(
      "Compare correlations across factors",
      subtitle = "Ranked by absolute Pearson correlation for the currently selected Y-axis outcome.",
      uiOutput("correlation_period_note"),
      DTOutput("correlation_table")))
}


# =============================================================================
# 6. MIGRATION ROUTES TAB
# =============================================================================
# Shows: for one chosen state, who moved in (or out): summary boxes, a bar chart
# of the ten largest routes, and a table of every route.
#
# Slots filled by atlas_server.R (section E):
#   route_kpis, route_chart_period_note, route_bar,
#   route_table_period_note, route_table

migration_routes_tab <- function() {
  tabPanel(
    title = "Migration routes", value = "routes",
    h1("Interstate migration routes"),
    p(class = "atlas-lede",
      "Explore which states sent or received exemptions represented on IRS interstate tax-return records. The partner list also includes Washington, D.C."),

    # --- State/direction picker with summary boxes ---
    section_card(
      "Explore interstate moves",
      div(class = "atlas-control-row",
          selectInput("route_state", "State", choices = state_choices, selected = "SC"),
          selectInput("route_direction", "Direction",
                      choices = c("Incoming: origin states" = "in",
                                  "Outgoing: destination states" = "out"))),
      uiOutput("route_kpis")),

    # --- Chart and table side by side ---
    div(class = "two-even",
        section_card("Top 10 interstate routes",
                     uiOutput("route_chart_period_note"),
                     plotOutput("route_bar", height = "460px")),
        section_card("All available route records",
                     subtitle = "N1 = tax returns; N2 = exemptions (a proxy for represented persons); AGI in millions of dollars.",
                     uiOutput("route_table_period_note"),
                     DTOutput("route_table"))),

    div(class = "atlas-alert",
        "Routes include Washington, D.C. as a partner. Suppressed/omitted pairs are not counted as zero; states with incomplete route data are flagged in the summary."))
}


# =============================================================================
# 7. COMPARE STATES TAB
# =============================================================================
# Shows: a side-by-side table of migration and ACS measures for two states.
#
# Slots filled by atlas_server.R (section F):
#   comparison_period_note, state_comparison

compare_states_tab <- function() {
  tabPanel(
    title = "Compare states", value = "compare",
    h1("Compare two states"),
    p(class = "atlas-lede",
      "Compare reported migration flows, 2020–2025 growth, and ACS socioeconomic measures; housing-cost burden is not included in this data release."),

    section_card(
      "Side-by-side comparison",
      div(class = "atlas-control-row",
          selectInput("state_a", "First state", state_choices, selected = "FL"),
          selectInput("state_b", "Second state", state_choices, selected = "CA")),
      uiOutput("comparison_period_note"),
      DTOutput("state_comparison")))
}


# =============================================================================
# 8. TRENDS TAB
# =============================================================================
# Shows: a line chart of one measure over time for two chosen states.
# (This tab ignores the sidebar year/period; it always shows all years.)
#
# Slots filled by atlas_server.R (section G):
#   trend_period_note, trend_plot

trends_tab <- function() {
  tabPanel(
    title = "Trends", value = "trends",
    h1("Changes over time"),
    p(class = "atlas-lede",
      "Compare ACS population and other measures for reference years 2019–2024, four IRS migration periods, or the separate 2020–2025 population endpoints."),

    section_card(
      "Time-series comparison",
      div(class = "atlas-control-row",
          selectInput("trend_metric", "Measure", choices = trend_choices,
                      selected = "net_exemptions"),
          selectInput("trend_state_a", "First state", state_choices, selected = "FL"),
          selectInput("trend_state_b", "Second state", state_choices, selected = "CA")),
      uiOutput("trend_period_note"),
      plotlyOutput("trend_plot", height = "480px"),
      div(class = "atlas-alert",
          "ACS estimates may represent overlapping multi-year periods and monetary values use their own survey-year inflation bases. ACS population is available for reference years 2019–2024; the separate population-estimates file contains 2020 and 2025 endpoints. IRS per-1,000 values use the fixed 2020 population denominator.")))
}


# =============================================================================
# 9. ABOUT US TAB
# =============================================================================
# Shows: the team (photo, major, bio, LinkedIn) and why the project was built.
#
# Slots filled by atlas_server.R (section H): photo_chacon, photo_kim, photo_salazar

# Edit team details here. `photo_id` must match team_portraits in atlas_server.R.
team_members <- list(
  list(photo_id = "photo_kim", name = "Lucas Kim",
       discipline = "Economics · Class of 2027",
       bio = paste0(
         "Lucas studies economics with a minor in Latin American and Caribbean Studies. ",
         "His interests in environmental economics and public policy offer perspective ",
         "on economic opportunity and regional conditions."),
       linkedin_url = "https://www.linkedin.com/in/lucas-kim-2b321a28a/"),
  list(photo_id = "photo_chacon", name = "Lucas Chacon",
       discipline = "Neuroscience · Class of 2027",
       bio = paste0(
         "Lucas is a Neuroscience major on the pre-dental track and an NCAA varsity swimmer ",
         "at Washington and Lee University. He hopes to bring his interests in science ",
         "and service together as a dentist by expanding access to quality dental care ",
         "in underserved communities."),
       linkedin_url = "https://www.linkedin.com/in/lucas-chacon-0812a3360/"),
  list(photo_id = "photo_salazar", name = "Diana Salazar",
       discipline = "Integrated Engineering & Computer Science · Class of 2027",
       bio = paste0(
         "Diana studies integrated engineering and computer science. She has applied ",
         "data analysis and visual reporting to city procurement records and workforce ",
         "demographics."),
       linkedin_url = "https://www.linkedin.com/in/diana-salazar-a93a8a265/")
)

about_tab <- function() {
  tabPanel(
    title = "About Us", value = "about",
    div(class = "about-eyebrow", "THE PEOPLE BEHIND THE DATA"),
    h1("Meet the team"),
    p(class = "atlas-lede",
      "We are students at Washington and Lee University bringing together interests in economics, neuroscience, engineering and computer science to investigate patterns of migration across the United States."),

    # One card per entry in team_members (above).
    div(class = "team-grid",
        lapply(team_members, function(member) {
          team_card(member$photo_id, member$name, member$discipline,
                    member$bio, member$linkedin_url)
        })),

    section_card(
      "Why we built American Migration Atlas",
      p("As part of our BIOL 1185 Visualizing Big Data project, we investigate where U.S. populations are changing and how interstate migration relates to rent, home values, employment, income and other state-level conditions."),
      p("Our visualizations use publicly available Census, American Community Survey (ACS) and IRS data. We distinguish population growth from interstate migration and treat correlations as exploratory associations, not explanations of individual decisions."),
      tags$span(class = "atlas-badge", "Washington and Lee University · BIOL 1185")))
}


# =============================================================================
# 10. DATA & METHODS TAB
# =============================================================================
# Shows: download links for the original CSVs, definitions of every calculated
# measure, and data-quality notes. Mostly static text.
#
# Slots filled by atlas_server.R (section I): the source_csv_* links, download_master

data_and_methods_tab <- function() {
  tabPanel(
    title = "Data & methods", value = "methods",
    h1("Data, definitions & limitations"),
    p(class = "atlas-lede",
      "All figures are recalculated from the 15 clean CSV files included in this project. The app uses no older housing table or annual population data that are not present in the new release."),

    # --- Where the original data came from (list is defined in atlas_server.R) ---
    section_card(
      "Data sources",
      subtitle = "The original data were obtained from these websites.",
      tags$ul(class = "atlas-file-list",
              lapply(data_source_websites, function(source_website) {
                tags$li(tags$b(source_website$name), tags$br(),
                        tags$a(href = source_website$url, target = "_blank",
                               rel = "noopener noreferrer", source_website$url))
              }))),
    
    div(class = "two-even",
        # --- Left: files you can download ---
        section_card(
          "CSV Files",
          p("Download the original project CSVs by filename. Files are sorted by same data source origin."),
          lapply(names(source_groups), function(group_name) {
            tags$details(tags$summary(group_name), source_links(source_groups[[group_name]]))
          }),
          downloadButton("download_master", "Export selected state dataset (CSV)")),

        # --- Right: how each number is calculated ---
        section_card(
          "How we calculate metrics",
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
            tags$li(tags$b("Rent share (%)"), " = the median share of household income spent on rent, as reported by ACS.")))),

    section_card(
      "Important interpretation and data-quality notes",
      tags$ul(
        tags$li("Both IRS inflow and outflow files describe the same routes. We analyze each pair once rather than add both source files."),
        tags$li("Migration entries use the source field 'exemptions' (N2), not a complete count of all people moving. Some pairs are suppressed or omitted, so reported state totals can be incomplete."),
        tags$li("The route universe includes 50 states and Washington, D.C.; maps and state comparisons cover the 50 states only."),
        tags$li("AGI is measured in thousands of dollars. Some paired IRS files differ by $1,000 of AGI for a few historic routes; the inflow view is used consistently."),
        tags$li("IRS rates retain the fixed 2020 population-estimate denominator for consistency. ACS population is shown separately and is not substituted into these rates."),
        tags$li("ACS Total Population supplies reference-year observations for 2019–2024. Its percentage change uses ACS 2020 as the baseline. The separate 2020–2025 endpoint comparison uses the population-estimates file; these sources are not spliced into a single series."),
        tags$li("Our study only examines correlation between state characteristics and migrations. Correlation is not necessarily causation.")))
  )
}


# =============================================================================
# 11. STYLING: CSS AND JAVASCRIPT
# =============================================================================
# You do not need to read this section to understand the app.
#
# CSS controls how things LOOK (colors, fonts, spacing, mobile layout). Each rule
# targets a `class` used above, e.g. `.atlas-card {...}` styles every
# div(class = "atlas-card"). The colors and spacing sizes near the top
# (--accent, --space-2, ...) are shared by the whole page: change one value and
# it updates everywhere.
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

# JavaScript for two small behaviors:
#   (1) highlight the sidebar button of the page that is open;
#   (2) resize the map and move its color bar below the map on narrow screens.
app_js <- "$(document).on('shiny:connected', function() {
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
})();"


# =============================================================================
# 12. PUTTING THE PAGE TOGETHER
# =============================================================================
# The page has two columns: the sidebar on the left, and the main area on the
# right. Inside the main area, each tab function from sections 4-10 supplies one
# page; only the page picked in the sidebar is shown (type = "hidden" removes
# the usual tab strip because the sidebar buttons do the switching).

ui <- fluidPage(
  # Page title, CSS and JavaScript
  tags$head(tags$title("American Migration Atlas — R Shiny"),
            tags$style(HTML(app_css)),
            tags$script(HTML(app_js))),

  # Keyboard-accessibility link (visible only when tabbing to it)
  tags$a(href = "#atlas_main", class = "atlas-skip-link", "Skip to main content"),

  div(class = "atlas-shell",
      sidebar_navigation(),                                   # left column

      div(class = "atlas-main", id = "atlas_main", role = "main", tabindex = "-1",
          overview_page_header(),                             # title (Overview only)
          reporting_controls_bar(),                           # IRS period / ACS year menus

          tabsetPanel(id = "page", type = "hidden",
                      overview_tab(),            # Section 4
                      explore_factors_tab(),     # Section 5
                      migration_routes_tab(),    # Section 6
                      compare_states_tab(),      # Section 7
                      trends_tab(),              # Section 8
                      about_tab(),               # Section 9
                      data_and_methods_tab())    # Section 10
      )
  )
)
