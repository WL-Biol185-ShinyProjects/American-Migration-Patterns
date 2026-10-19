# American Migration Atlas — RStudio / Shiny

This is the R implementation of the user's original American Migration Atlas HTML dashboard. **All calculations are made in R from the original repository CSVs when the app starts.** The app does **not** need `American_Migration_Master_Data.csv`, any other prepared CSV, JavaScript, or an external API.

## Quick start on an iMac

1. Extract this ZIP into a regular folder (for example, `~/Documents/American-Migration-Atlas-Shiny/`).
2. Double-click `American-Migration-Patterns.Rproj` to open the folder as an RStudio Project. Then open `app.R` in RStudio.
3. Install packages **once**, in the RStudio **Console**:
   ```r
   install.packages(c("shiny", "plotly", "ggplot2", "DT"))
   ```
4. With the app's folder as your working directory, click **Run App** at the top of `app.R`, or type in the R console:
   ```r
   shiny::runApp("/FULL/PATH/TO/American-Migration-Atlas-Shiny")
   ```
   If you downloaded `app.R` separately, put it into your original GitHub repository folder, alongside the original CSVs, and use `shiny::runApp()` from that folder.
5. The app opens in the RStudio Viewer or your web browser. Click **Open in Browser** for the largest interactive view.

## Sections

- **Overview**: Clickable 50-state tile map, population growth, migration inflow/loss rankings, migration KPIs.
- **Explore factors**: Interactive scatterplots with selectable X and Y, regression lines, Pearson `r`, `R²`, `p`-values, factor-correlation rankings, and plotted-point CSV download.
- **Migration routes**: State-to-state IRS flow rankings for 2021–22 and 2022–23, including N1, N2, and AGI.
- **Compare states**: Any two states, with ACS year and IRS period selectors.
- **Trends**: Population change 2020–2025 and available ACS variables 2019–2024.
- **Data & methods**: Formulas, source caveats, and a CSV export of the currently selected merged 50-state dataset (computed on demand).

## CSV files the application actually reads

- `Population Change by State Level.csv`
- `ACS_2019_state_new.csv` through `ACS_2024_state_new.csv` (6 files)
- `Housing Costs as a % of Household Income in the Past 12 Months.csv`
- `stateinflow2122.csv`, `stateoutflow2122.csv`
- `stateinflow2223.csv`, `stateoutflow2223.csv`

These **12 input CSVs are original, unchanged copies** from your uploaded GitHub ZIP. Keeping them in the same folder as `app.R` is required. The full GitHub repository includes other scripts and cleaned CSV files, but none of those are required to run this Shiny dashboard.

## Variable handling

- Official state **FIPS codes** are preserved for merges. Alphabetical state numbers `01`–`50` are separate display identifiers.
- **Population growth**: 2020 to 2025 cumulative percentage change.
- **IRS net domestic migration**: inbound matched-return individuals `N2` minus outbound matched-return individuals `N2`. Divide by Census population in the *starting year* of the IRS period and multiply by 1,000 for a normalized rate.
- **Housing burden**: Over-30% housing-cost-to-income categories divided by households with calculable burden. Over-50% rows are not added separately because they are nested.
- **Unemployment rate**: ACS unemployed civilians / ACS civilian labor force × 100; not the monthly Bureau of Labor Statistics rate.
- **Poverty rate**: three ACS income-to-poverty categories below 1.00, divided by poverty-status universe × 100.
- **IRS AGI**: adjusted gross income recorded in *thousands of dollars*; route table divides by 1,000 to show dollar millions.

## Interpretation limits

- IRS records are tax-return-based, **not complete counts of all people who moved**. Suppressed interstate-route counts are excluded.
- Population change is not the same as interstate migration; births, deaths, and international migration matter.
- 2024 economic conditions are measured after the 2021–22 and 2022–23 IRS periods. A positive or negative correlation cannot establish why individuals moved.
- ACS 2019–2024 extracts can vary in survey period or collection method; yearly median-dollar figures are adjusted to the **respective ACS years**, not a single common price year.
- Total U.S. IRS interstate migration counts include D.C.; displayed origin-destination routes show pairs among the 50 states only.

## Troubleshooting

**`CSV files not found`**: `app.R` must be in the same directory as the twelve original CSV files. Avoid launching it with a different directory as the Shiny app root.

**`There is no package called shiny/plotly/ggplot2/DT`**: Run the installation line above in the Console and retry.

**Plots appear small in Viewer**: Click **Open in Browser** or enlarge the RStudio Viewer pane.

**Want to publish the Shiny version?** A Shiny application needs an R server (for example shinyapps.io or Shiny Server); GitHub Pages can host the original static HTML version, but cannot directly execute R Shiny code.
