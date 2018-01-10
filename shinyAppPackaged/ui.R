library(shiny)

fluidPage(

  # Input: Select a file ----
  fileInput("file1", "Choisir un planning à uploader (format CSV)",
            multiple = FALSE,
            accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),

  # Button
  downloadButton("downloadData", "Télécharger le résultat"),

  actionButton("go", "Lancer le calcul"),

  # App title ----
  titlePanel("Sliders"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      verbatimTextOutput("computation"),

      # Input: Simple integer interval ----
      sliderInput("start.week", "Première semaine du planning:",
                  min = 1, max = 52,
                  value = 1),

      # Input: Decimal interval with step value ----
      sliderInput("end.week", "Dernière semaine du planning:",
                  min = 1, max = 52,
                  value = 10),

      sliderInput('pre.planned.weeks', 'Semaines pré-plannifiées dans le fichier uploadé:',
                  min = 0,
                  max = 52, step = 1,
                  value = c(2, 7)),

      sliderInput('history.weeks', "Semaines d'historique figé:",
                  min = 0,
                  max = 52, step = 1,
                  value = 0),

# weekly sliders
      lapply(1:52, function(i) {
                    uiOutput(paste0('weekUI', i))
                  }),

      sliderInput('max.unfulfilled', "Nombre de contraintes non-respectées maximum",
                  min = 0, max = 40, step = 1,
                  value = 20),

      sliderInput('workload', 'Fourchette de tolerence pour la charge de travail totale individuelle par rapport au standard:',
                  min = 0.5,
                  max = 1.3, step = 0.01,
                  value = c(0.97, 1.03)),

      sliderInput('c1', "Cout d'une annulation de vacation programmée",
                  min = 1, max = 20, step = 0.5,
                  value = 5),

      sliderInput('c2', "Cout d'une programmation de vacation tardive",
                  min = 1, max = 20, step = 0.5,
                  value = 2),

      sliderInput('unbalanced.factor', "Facteur d'équilibrage de la charge de travail",
                  min = 0, max = 2, step = 0.01,
                  value = 0.1)

    ),


    mainPanel(

      tableOutput("pre.planned.table"),


      # Output: Table summarizing the values entered ----
      tableOutput("plannerTable")

#      # workload
#      rowSums(planning) * 2
#      # constrariete
#      rowSums(unfulfilled$const.matrix %*% solution)
#      # workforce
#      workforce.min$const.matrix %*% solution
#

    )
  )
)
