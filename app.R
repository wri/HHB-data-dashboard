# Entry point - this is the file you run (or click "Run App" on in RStudio)
# to launch the dashboard. It just loads the other 3 files:
#   global.R  - loads packages and data, runs once at startup
#   ui.R      - what the app looks like
#   server.R  - what the app does

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
