# Load function's project
source("Project_Functions.R")
print("Load function's project...")

# Generate dummy data
source("Generate_Election_Data.R")
print("Generate dummy data...")

# Compile all election data
source("Process_Election_Results.R")
print("Compile all election data...")

# Dashboard display
print("Dashboard display...")
source("ui.R")
source("app.R")
shiny::runApp("app.R")
