library(shiny)

source('/home/cramdoulfa/Workspace/planning-optim/code/optim.R')
source('/home/cramdoulfa/Workspace/planning-optim/code/planner.R')

# names = c(
# 'week1',
# 'week2',
# 'week3',
# 'week4',
# 'week5',
# 'week6',
# 'week7',
# 'week8',
# 'week9',
# 'week10')
#
# displayNames = c(
# 'Week 1',
# 'Week 2',
# 'Week 3',
# 'Week 4',
# 'Week 5',
# 'Week 6',
# 'Week 7',
# 'Week 8',
# 'Week 9',
# 'Week 10')

# fourchette = t(matrix(rep(c(60, 120), 10), ncol = 10))
# valueFourchette = rep(90, 10)

deltaFourchette = 20

# Define UI for slider demo app ----
ui <- fluidPage(

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

      verbatimTextOutput("workloadValue"),

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
                  value = 1),

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
                  min = 0, max = 5, step = 0.01,
                  value = 0.1),

      sliderInput('max.unfulfilled', "Nombre de contraintes non-respectées maximum",
                  min = 0, max = 40, step = 1,
                  value = 20),

# weekly sliders
      lapply(1:52, function(i) {
                    uiOutput(paste0('weekUI', i))
                  })
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


# Define server logic for slider examples ----
server <- function(input, output) {

  output$workloadValue  <- renderText({
  paste("Computation time:", as.character(planning.solver()$computation.time))
})

# read input CSV file

pre.planned <- reactive({
  req(input$file1)
  df <- read.csv(input$file1$datapath,
                 header = TRUE,
                 sep = ';')
  return(df)
})

output$pre.planned.table <- renderTable({
  req(pre.planned())
  return(pre.planned())
})



  # Compute planning
  variables <- reactive({
    if(!is.null(input$file1))
    {
     define.problem(input$start.week,
                    input$end.week,
                    input$workload[1],
                    unbalanced.factor = input$unbalanced.factor,
                    pre.planned = pre.planned(),
                    pre.planned.start = input$pre.planned.weeks[1],
                    pre.planned.end = input$pre.planned.weeks[2],
                    c1 = input$c1,
                    c2 = input$c2,
                    history.end.week = input$history.weeks,
                    max.unfulfilled = input$max.unfulfilled)#,
    }
    else
    {
      define.problem(input$start.week,
                     input$end.week,
                     input$workload[1])#,
    }
  })


  planning.solver <- eventReactive(input$go, {
    modified.const.val = variables()$const.val
    for (week in (input$history.weeks + 1):input$end.week)
    {
      modified.const.val[week - input$start.week + 1] = input[[paste0('weekSel', week)]][1]
    }
#    modified.const.val[1] = input[['weekSel1']][1]
#    modified.const.val[2] = input[['weekSel2']][1]
#    browser()
    start_time <- Sys.time()
    sol = lp(objective.in = variables()$cost.vector,
             const.mat = variables()$const.mat,
             const.dir = variables()$const.dir,
             const.rhs = modified.const.val,
             direction = "min",
             all.int = TRUE,
             use.rw = TRUE)
    computation.time <- Sys.time() - start_time
    # format the solution
    n.radiologues = nrow(variables()$df)
    solution = sol$solution
    solution = solution[1:(variables()$calendar.length * n.radiologues)]
    planning = format.planning(solution, variables()$df, variables()$calendar.length)
    list('planning' = planning, 'computation.time' = computation.time)
  })


  # DF output
  output$plannerTable <- renderTable({
    planning.solver()$planning
  })


  # Download planning under CSV format
  output$downloadData <- downloadHandler(
    filename = 'suggested-planning.csv',
    content = function(file) {
      write.table(planning.solver()$planning, file, sep = ';')
    })


  lapply(1:52, function(i) {
      output[[paste0('weekUI', i)]] <- renderUI({

        if(!is.na(variables()$workforce.min.limits[i]))
        {
          sliderInput(paste0('weekSel', i), paste0('Fourchette de vacations de la semaine ', i),
                    min = 40,
                    max = variables()$workforce.min.limits[i],
                    value = c(variables()$workforce.min.values[i], variables()$workforce.min.values[i] + deltaFourchette))
        }
      })
    })

}

# Create Shiny app ----
shinyApp(ui, server)
