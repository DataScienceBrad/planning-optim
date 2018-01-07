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

      sliderInput("end.week", "End week:",
                  min = 1, max = 52,
                  value = 10),

      sliderInput("end.week", "End week:",
                  min = 1, max = 52,
                  value = 10)

#      uiOutput("weekSelector1"),
#      uiOutput("weekSelector2"),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      lapply(1:10, function(i) {
        uiOutput(paste0('weekUI', i))
      }),


#      lapply(1:5, function(i) {
#                                sliderInput(paste0('a', i),
#                                            paste0('Week ', i),
#                                            min = fourchette[i, 1],
#                                            max = fourchette[i, 2],
#                                            value = c(valueFourchette[i], valueFourchette[i] + deltaFourchette))
#                              }),


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
      write.table(planning.solver(), file, sep = ';')
    })


#  output$a_weeks <- renderPrint({
#      res <- lapply(1:5, function(i) {
#         input[[paste0('a', i)]]
#         })
#      str(setNames(res, paste0('myWeek', 1:5)))
#    })

  lapply(1:10, function(i) {
      output[[paste0('weekUI', i)]] <- renderUI({
        sliderInput(paste0('weekSel', i), paste0('Workforce week ', i),
                    min = 40,
                    max = variables()$workforce.min.limits[i],
                    value = c(variables()$workforce.min.values[i], variables()$workforce.min.values[i] + deltaFourchette))
                    })
    })

#  output$weekSelector1 <- renderUI({
#    sliderInput(names[1], displayNames[1],
#                min = 40,
#                max = variables()$workforce.min.limits[1],
#                value = c(variables()$workforce.min.values[1], variables()$workforce.min.values[1] + deltaFourchette))
#    })
#
#  output$weekSelector2 <- renderUI({
#    sliderInput(names[2], displayNames[2],
#                min = 40,
#                max = variables()$workforce.min.limits[2],
#                value = c(variables()$workforce.min.values[2], variables()$workforce.min.values[2] + deltaFourchette))
#    })

}

# Create Shiny app ----
shinyApp(ui, server)
