# =============================================================================
# app.R  —  LAUNCHER for the American Migration Atlas
# =============================================================================
# Run this file with RStudio's "Run App" button.
# Keep these three R files and the 15 CSV files in the same project folder:
#   app.R           (this file: loads the other two, then starts the app)
#   atlas_server.R  (reads the CSVs, makes the plots, defines `server`)
#   atlas_ui.R      (page layout, one function per tab, defines `ui`)
#
# The order matters: atlas_server.R goes first because atlas_ui.R uses some of
# the lists it creates (dropdown choices, state names, ...).

source("atlas_server.R")
source("atlas_ui.R")

shinyApp(ui = ui, server = server)
