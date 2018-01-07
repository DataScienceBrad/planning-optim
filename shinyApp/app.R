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
  fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")),

  # Button
  downloadButton("downloadData", "Download Planning"),

  actionButton("go", "Go"),

  # App title ----
  titlePanel("Sliders"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(

      verbatimTextOutput("workloadValue"),

      # Input: Simple integer interval ----
      sliderInput("start.week", "Start week:",
                  min = 1, max = 52,
                  value = 1),

      # Input: Decimal interval with step value ----
      sliderInput("end.week", "End week:",
                  min = 1, max = 52,
                  value = 10),

      sliderInput('pre.planned.weeks', 'Pre-planned weeks:',
                  min = 1,
                  max = 52, step = 1,
                  value = c(1, 7)),

      sliderInput('pre.planned.weeks', 'Pre-planned weeks:',
                  min = 1,
                  max = 52, step = 1,
                  value = c(1, 7)),

      sliderInput('workload', 'Individual workload margin:',
                  min = 0.5,
                  max = 1.3, step = 0.01,
                  value = c(0.97, 1.03)),

      sliderInput('c1', "Cout d'une annulation de vacation programmee",
                  min = 1, max = 20, step = 0.5,
                  value = 5),

      sliderInput('c2', "Cout d'une programmation tardive",
                  min = 1, max = 20, step = 0.5,
                  value = 2)
    ),

    mainPanel(

      tableOutput("pre.planned.table"),

      lapply(1:10, function(i) {
        uiOutput(paste0('weekUI', i))
      }),



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
                    pre.planned = pre.planned(),
                    pre.planned.start = input$pre.planned.weeks[1],
                    pre.planned.end = input$pre.planned.weeks[2],
                    c1 = input$c1,
                    c2 = input$c2)#,
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
#    for (week in input$start.week:input$end.week)
#    {
#      modified.const.val[week - input$start.week + 1] = input[[names[week]]]
#    }
    modified.const.val[1] = input[['weekSel1']]

    print(modified.const.val)
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


  lapply(1:10, function(i) {
      output[[paste0('weekUI', i)]] <- renderUI({
        sliderInput(paste0('weekSel', i), paste0('Workforce week ', i),
                    min = 40,
                    max = variables()$workforce.min.limits[i],
                    value = c(variables()$workforce.min.values[i], variables()$workforce.min.values[i] + deltaFourchette))
                    })
    })

}

# Create Shiny app ----
shinyApp(ui, server)
