library('shiny')
library('lpSolve')
library('data.table')
source('optim.R')
source('planner.R')


function(input, output) {

  output$computation  <- renderText({
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
                     input$workload[1],
                     unbalanced.factor = input$unbalanced.factor,
                     history.end.week = input$history.weeks,
                     max.unfulfilled = input$max.unfulfilled)#,
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
                    value = c(variables()$workforce.min.values[i], variables()$workforce.min.values[i] + 20))
        }
      })
    })

}
