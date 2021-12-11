# app.R 
# Fiona Jain, Vega Jethani, Aaron Jenkins, Mehr Mehta
# Final Deliverable

# Source UI and Server files
source("combined_app_ui.R")
source("combined_app_server.R")

# Run Shiny App
shinyApp(ui = ui, server = server)