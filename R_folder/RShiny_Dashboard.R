# Load required libraries
library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(bslib)
library(DT)

data <- fread("C:/Users/gs8894/OneDrive - Cooperactions/Documents/Election_project/output_file/Compilation_resultats_elections.csv")

Taux_participation <- round(sum(data[,7])/sum(data[,6])*100, 2)

election_data <- data %>%
  group_by(data[,1]) %>%
  summarise(across(9:22, \(x) sum(x, na.rm = TRUE)))

# Define the UI for the application

ui <- page_fluid(
   tags$h1("Election Présidentielle 2010"),
   tags$h2(paste("Bureaux pris en compte :", dim(data)[1],"/500")),
   tags$h3(paste("Taux de participation :", Taux_participation, "%")),
   
   tags$br(),
   tags$h5(paste("Konan BEDIE : ", sum(data$"BEDIE KONAN"))),
   tags$h5(paste("Laurent GBAGBO : ", sum(data$"GBAGBO LAURENT"))),
   tags$h5(paste("Alassane OUATTARA : ", sum(data$"OUATTARA ALASSANE"))),

   #Adds a horizontal line
   tags$hr(),
   tags$br(),

   dataTableOutput("table")
)

server <- function(input, output) {
   output$table <-
     renderDataTable({datatable(election_data, filter = "top")})
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
