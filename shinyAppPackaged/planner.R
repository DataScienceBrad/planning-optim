library('lpSolve')
library('data.table')

start.week = 1
end.week = 10
workload.min.factor = 0.6

define.problem <- function(start.week,
                           end.week,
                           workload.min.factor,
                           workload.max.factor,
                           # balanceing constraint
                           unbalanced.factor = 1 / 10,
                           # pre-planning
                           pre.planned = NA,
                           pre.planned.start = NA,
                           pre.planned.end = NA,
                           c1 = 5,
                           c2 = 2,
                           history.end.week = 0,
                           max.unfulfilled = 20)#,

        #            workload.max.factor)
{
    dataPlanningFile = paste(folder, "data/dataPlanning.csv", sep = '')
    holiday.data = troncate.holiday.data(extract.holiday.data(), start.week, end.week)
    radiologues.data = extract.data(file.location = dataPlanningFile, saturday.penalty = 0)
    df = radiologues.data$df
    n.radiologues = nrow(df)
    # global vars
    history.vector = 1:n.radiologues
    penalty.factor = 1
    workforce.max.factor = 1
    num.bin.solns = 1
    #workload min default values
    full.force.threshold = 75
    workforce.min.factor = 0.6
    max.minimum = 90
    n.week = end.week - start.week + 1
    calendar.length = n.week * 7


    rest.preferences = radiologues.data$rest.preferences
    rownames(holiday.data) = rownames(rest.preferences)

    # CONSTRAINTS

    # one single line
    holiday = holiday.constraint(holiday.data)
    # one line per week
    workforce.min = workforce.min.constraint(df, start.week, end.week, full.force.threshold, workforce.min.factor, max.minimum)
#    workforce.max = workforce.max.constraint(df, n.week, workforce.factor = workforce.max.factor)

    # one line per radiologue
#    workload = workload.constraint(df, n.week = n.week, workload.min.factor, workload.max.factor)
    workload = workload.constraint(df, n.week = n.week, workload.min.factor)

    saturday = saturday.constraint(df, n.week)
    binaries = force.binary.constraint(df, n.week)
    unfulfilled = max.unfulfilled.constraint(rest.preferences, df, n.week, max.unfulfilled = max.unfulfilled)

    # penalty vector for fairness proposition. Not used at the moment
    penalty.vector = ((history.vector + 1) * penalty.factor) ^ 2  / ((max(history.vector + 1) * penalty.factor) ^ 2)
    penalty.vector = round(penalty.vector, 3)
    penalty.vector = rep(1, length(penalty.vector))
    # GENERATE COST VECTOR
    cost.vector = cost.vector.rest.days(rest.preferences, df, penalty.vector, n.week)

    if(!is.null(dim(pre.planned)))
    {
      cost.vector.change = cost.vector.changes(pre.planned, first.week = pre.planned.start, last.week = pre.planned.end, c1 = c1, c2 = c2)
      cost.vector.change = c(cost.vector.change, rep(0, length(cost.vector) - length(cost.vector.change)))
      cost.vector = cost.vector + cost.vector.change
    }

    # Append the constraints
    const.matrix = rbind(workforce.min$const.matrix,
                         workload$const.matrix,
                         holiday$const.matrix,
                         binaries$const.matrix,
                         unfulfilled$const.matrix)#,
  #                       workforce.max$const.matrix)#,
                         #saturday$const.matrix)
    const.dir = c(workforce.min$const.dir,
                  workload$const.dir,
                  holiday$const.dir,
                  binaries$const.dir,
                  unfulfilled$const.dir)#,
#                  workforce.max$const.dir)#,
                  #saturday$const.dir)
    const.val = c(workforce.min$const.value,
                  workload$const.value,
                  holiday$const.value,
                  binaries$const.value,
                  unfulfilled$const.value)#,
#                  workforce.max$const.value)#,
                  #saturday$const.value)

    if(history.end.week > 0)
    {
      history = force.history.constraint(pre.planned, history.end.week)
      ncol.to.add = ncol(const.matrix) - ncol(history$const.matrix)
      extended.history.const.matrix = cbind(history$const.matrix, matrix(rep(0, nrow(history$const.matrix) * ncol.to.add), nrow(history$const.matrix)))
      const.matrix = rbind(const.matrix, extended.history.const.matrix)
      const.val = c(const.val, history$const.value)
      const.dir = c(const.dir, history$const.dir)
    }



    # Artificial variables are used to add the absolute value in the balance constraint
    # for the weeks with no work at all (full weeks of holiday) it does not work.
    # We therefor retreive these weeks to exculde them from the artifical variables.
    vacances = read.csv(paste(folder, "data/semaines-conges.csv", sep = ''), header = TRUE, sep = ';')
    vacances = vacances[3:31, 4:11]
    df.vacances = sapply(as.data.frame(vacances), function(x) as.numeric(substring(x,2)))
    # artifical size is the number of artifical variables, after removal of the full holiday weeks
    # compute artifical size:
    artificial.size = 0
    for (radiologue in 1:n.radiologues)
    {
      weekly.standard = as.numeric(df[,4])[radiologue] / 2
      for (week in 1:n.week)
      {
        if(!((week + start.week - 1) %in% df.vacances[radiologue,]))
        {
          artificial.size = artificial.size + 2
        }
      }
    }


    # because the balance constraint requires to add artificial variables, it is dealt with after
    balancing = TRUE
    if (balancing)
    {
      balance.matrix = matrix(0, nrow = artificial.size, ncol = calendar.length * n.radiologues + artificial.size)
      balance.matrix.row = 1
      for (radiologue in 1:n.radiologues)
      {
        weekly.standard = as.numeric(df[,4])[radiologue] / 2
        for (week in 1:n.week)
        {
          if(!((week + start.week - 1)  %in% df.vacances[radiologue,]))
          {
            for (day in 1:7)
            {
              balance.matrix[balance.matrix.row, ((week - 1) * 7 + (day - 1)) * n.radiologues + radiologue] = 1
              balance.matrix[balance.matrix.row + 1, ((week - 1) * 7 + (day - 1)) * n.radiologues + radiologue] = -1
            }
            # append artifical variables region
            balance.matrix[balance.matrix.row, calendar.length * n.radiologues + balance.matrix.row] = -1
            balance.matrix[balance.matrix.row + 1, calendar.length * n.radiologues + balance.matrix.row] = -1

      #      balance.matrix = rbind(balance.matrix, neg.indiv.weekly.interpolation)
            balance.matrix.row = balance.matrix.row + 2

            const.val = c(const.val, weekly.standard)
            const.val = c(const.val, -weekly.standard)
            const.dir = c(const.dir, rep('<=', 2))
          }
        }
      }
      # ralonger les matrices avec des 0 en fonction de la taille des bogus variables
      const.matrix = cbind(const.matrix, matrix(rep(0, dim(const.matrix)[1] * artificial.size), dim(const.matrix)[1]))
      const.matrix = rbind(const.matrix, balance.matrix)
      # update cost vector due to artificial variable addition
      cost.vector = c(cost.vector, rep(unbalanced.factor, artificial.size))
    }

    # computation time landmark
    middle_time <- Sys.time()

    return(list('cost.vector' = cost.vector,
           'const.mat' = const.matrix,
           'const.dir' = const.dir,
           'const.val' = const.val,
           'workforce.min.values' = workforce.min$minimums,
           'workforce.min.limits' = workforce.min$limits,
           'df' = df,
           'calendar.length' = calendar.length))
}




#    sol = lp(objective.in = cost.vector,
#             direction = "min",
#             const.mat = const.matrix,
#             const.dir = const.dir,
#             const.rhs = const.val,
#             all.int = TRUE,
#    #         compute.sens = 1,
#             use.rw = TRUE)
#
#    middle_time2 <- Sys.time()
#
#    # format the solution
#    solution = sol$solution
#    solution = solution[1:(calendar.length*n.radiologues)]
#    planning = format.planning(solution, df, calendar.length)
#    return(planning, workforce.min.values = workforce.min['minimums'], workforce.min.limits = workforce.min['limits'])
#}
