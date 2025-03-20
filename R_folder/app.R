library(shiny)
library(data.table)
library(shinydashboard)

data <- fread("C:/Users/gs8894/OneDrive - Cooperactions/Documents/Election_project/output_file/Compilation_resultats_elections.csv")
Taux_participation <- paste(round(sum(data[,7])/sum(data[,6])*100, 2),"%")
Parametre <- c("REGION", "DEPARTEMENT", "SOUS-PREFECTURE", "BUREAU DE VOTE", "INSCRITS", "VOTANTS", "BULLETINS NULS", "SUFFRAGES EXPRIMES")
Selected_parametre <- c("REGION", "DEPARTEMENT")

nb_voix_bedie <- sum(data$"BEDIE KONAN")
nb_voix_gbagbo <- sum(data$"GBAGBO LAURENT")
nb_voix_ado <- sum(data$"OUATTARA ALASSANE")

total_voix <- sum(data$"BEDIE KONAN") + sum(data$"GBAGBO LAURENT") + sum(data$"OUATTARA ALASSANE")

pourcen_bedie <- paste(round((sum(data$"BEDIE KONAN")/total_voix)*100, 2), "%")
pourcen_gbagbo <- paste(round((sum(data$"GBAGBO LAURENT")/total_voix)*100, 2), "%")
pourcen_ado <- paste(round((sum(data$"OUATTARA ALASSANE")/total_voix)*100, 2), "%")

KB <- paste(nb_voix_bedie, "-", pourcen_bedie)
LG <- paste(nb_voix_gbagbo, "-", pourcen_gbagbo)
AO <- paste(nb_voix_ado, "-", pourcen_ado)

ui <- dashboardPage(
  dashboardHeader(title = "CI ELECTION"),
  dashboardSidebar(),
  dashboardBody(
	fluidRow(
	  infoBox("Election Presidentielle 2010", value = paste("Bureaux pris en compte :", dim(data)[1],"/500"), width = 3),
	),
	fluidRow(
	  valueBox(Taux_participation, "Taux de participation", color = "red", icon = icon("people-pulling"), width = 2),
	),
    fluidRow(
      # A static valueBox
      valueBox(KB, "Konan BEDIE", color = "green", icon = icon("check-to-slot"), width = 3),
      valueBox(LG, "Laurent GBAGBO", color = "blue", icon = icon("check-to-slot"), width = 3),
	  valueBox(AO, "Alassane OUATTARA", color = "orange", icon = icon("check-to-slot"), width = 3)
    ),
	fluidPage(
	  sidebarLayout(
		sidebarPanel(
			conditionalPanel(
			'input.dataset === "data"',
			checkboxGroupInput("show_vars", "Critères de sélection :",
                           Parametre, selected = Selected_parametre)
		)
    ),
	mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("data", DT::dataTableOutput("mytable"))
      )
    )
   )
  )
))

# Define server logic
server <- function(input, output) {

  output$mytable <- DT::renderDataTable({
    fixed_columns <- c("LIEU DE VOTE", "BEDIE KONAN", "GBAGBO LAURENT", "OUATTARA ALASSANE")
    selected_columns <- unique(c(input$show_vars, fixed_columns))
    
    if (all(selected_columns %in% names(data))) {
      filtered_data <- data[, ..selected_columns]
    } else {
      filtered_data <- data[, ..fixed_columns]
    }
    
    DT::datatable(filtered_data, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
