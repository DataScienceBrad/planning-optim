library(shiny)

source('/home/cramdoulfa/Workspace/planning-optim/code/optim.R')
source('/home/cramdoulfa/Workspace/planning-optim/code/planner.R')

names = c(
'week1',
'week2',
'week3',
'week4',
'week5',
'week6',
'week7',
'week8',
'week9',
'week10')

displayNames = c(
'Week 1',
'Week 2',
'Week 3',
'Week 4',
'Week 5',
'Week 6',
'Week 7',
'Week 8',
'Week 9',
'Week 10')

fourchette = t(matrix(rep(c(60, 120), 10), ncol = 10))
valueFourchette = rep(90, 10)
deltaFourchette = 20

# Define UI for slider demo app ----
ui <- fluidPage(

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

      sliderInput('workload', 'Individual workload margin:',
                  min = 0.5,
                  max = 1.3, step = 0.01,
                  value = c(0.97, 1.03)),

      uiOutput("testSelector"),

      sliderInput(names[2], displayNames[2],
                  min = fourchette[2, 1],
                  max = fourchette[2, 2],
                  value = c(valueFourchette[2], valueFourchette[2] + deltaFourchette)),

      sliderInput(names[3], displayNames[3],
                  min = fourchette[3, 1],
                  max = fourchette[3, 2],
                  value = c(valueFourchette[3], valueFourchette[3] + deltaFourchette)),

      sliderInput(names[4], displayNames[4],
                  min = fourchette[4, 1],
                  max = fourchette[4, 2],
                  value = c(valueFourchette[4], valueFourchette[4] + deltaFourchette)),

      sliderInput(names[5], displayNames[5],
                  min = fourchette[5, 1],
                  max = fourchette[5, 2],
                  value = c(valueFourchette[5], valueFourchette[5] + deltaFourchette)),

      sliderInput(names[6], displayNames[6],
                  min = fourchette[6, 1],
                  max = fourchette[6, 2],
                  value = c(valueFourchette[6], valueFourchette[6] + deltaFourchette)),

      sliderInput(names[7], displayNames[7],
                  min = fourchette[7, 1],
                  max = fourchette[7, 2],
                  value = c(valueFourchette[7], valueFourchette[7] + deltaFourchette)),

      sliderInput(names[8], displayNames[8],
                  min = fourchette[8, 1],
                  max = fourchette[8, 2],
                  value = c(valueFourchette[8], valueFourchette[8] + deltaFourchette)),

      sliderInput(names[9], displayNames[9],
                  min = fourchette[9, 1],
                  max = fourchette[9, 2],
                  value = c(valueFourchette[9], valueFourchette[9] + deltaFourchette)),

      sliderInput(names[10], displayNames[10],
                  min = fourchette[10, 1],
                  max = fourchette[10, 2],
                  value = c(valueFourchette[10], valueFourchette[10] + deltaFourchette)),

      # Input: Specification of range within an interval ----
      sliderInput("range", "Range:",
                  min = 1, max = 1000,
                  value = c(200,500)),

      # Input: Custom currency format for with basic animation ----
      sliderInput("format", "Custom Format:",
                  min = 0, max = 10000,
                  value = 0, step = 2500,
                  pre = "$", sep = ",",
                  animate = TRUE),

      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("animation", "Looping Animation:",
                  min = 1, max = 2000,
                  value = 1, step = 10,
                  animate =
                    animationOptions(interval = 300, loop = TRUE))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

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


  # Compute planning
  variables <- reactive({
     define.problem(input$start.week,
                    input$end.week,
                    input$workload[1])#,
    })

  planning.solver <- eventReactive(input$go, {
    start_time <- Sys.time()

    sol = lp(objective.in = variables()$cost.vector,
             const.mat = variables()$const.mat,
             const.dir = variables()$const.dir,
             const.rhs = variables()$const.val,
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
      write.table(planning.solver(), file, sep = ';')
    })

  output$testSelector <- renderUI({

    sliderInput(names[1], displayNames[1],
                min = fourchette[1, 1],
                max = variables()$workforce.min.limits[1],
                value = c(valueFourchette[1], valueFourchette[1] + deltaFourchette))
    })
}

# Create Shiny app ----
shinyApp(ui, server)
