source('/home/cramdoulfa/Workspace/planning-optim/code/optim.R')
source('/home/cramdoulfa/Workspace/planning-optim/code/planningFeedback.R')

library('lpSolve')
library('data.table')

# computation time
start_time <- Sys.time()

# global vars
n.radiologues = 29
dataPlanningFile = paste(folder, "data/dataPlanning.csv", sep = '')
history.vector = 1:n.radiologues
penalty.factor = 1
workforce.min.factor = 0.6 # proportion de travail minimal total hebdomadaire / standard
workforce.max.factor = 1
workload.min.factor = 1
workload.max.factor = 1.05
unbalanced.factor = 1 / 10
num.bin.solns = 1
max.unfulfilled = 52
full.force.threshold = 75
workforce.min.factor = 0.6
max.minimum = 90

# generation variable
start.week = 1
end.week = 22
n.week = end.week - start.week + 1
calendar.length = n.week * 7
holiday.data = troncate.holiday.data(extract.holiday.data(), start.week, end.week)
radiologues.data = extract.data(file.location = dataPlanningFile, saturday.penalty = 0)
df = radiologues.data$df
rest.preferences = radiologues.data$rest.preferences
rownames(holiday.data) = rownames(rest.preferences)

# CONSTRAINTS

# one single line
holiday = holiday.constraint(holiday.data)
# one line per week
workforce.min = workforce.min.constraint(df, start.week, end.week, full.force.threshold = 75, workforce.min.factor = 0.6, max.minimum = 90)
workforce.max = workforce.max.constraint(df, n.week, workforce.factor = workforce.max.factor)
# one line per radiologue
workload = workload.constraint(df, n.week = n.week, workload.min.factor = workload.min.factor)
#workload = workload.constraint(df, n.week = n.week, workload.min.factor = workload.min.factor, workload.max.factor = workload.max.factor)
saturday = saturday.constraint(df, n.week)
binaries = force.binary.constraint(df, n.week)
unfulfilled = max.unfulfilled.constraint(rest.preferences, df, n.week, max.unfulfilled = max.unfulfilled)

# penalty vector for fairness proposition. Not used at the moment
penalty.vector = ((history.vector + 1) * penalty.factor) ^ 2  / ((max(history.vector + 1) * penalty.factor) ^ 2)
penalty.vector = round(penalty.vector, 3)
penalty.vector = rep(1, length(penalty.vector))
# GENERATE COST VECTOR
cost.vector = cost.vector.rest.days(rest.preferences, df, penalty.vector, n.week)

# Append the constraints
const.matrix = rbind(workload$const.matrix,
                     holiday$const.matrix,
                     binaries$const.matrix,
                     unfulfilled$const.matrix,
                     workforce.min$const.matrix,
                     workforce.max$const.matrix)#,
                     #saturday$const.matrix)
const.dir = c(workload$const.dir,
              holiday$const.dir,
              binaries$const.dir,
              unfulfilled$const.dir,
              workforce.min$const.dir,
              workforce.max$const.dir)#,
              #saturday$const.dir)
const.val = c(workload$const.value,
              holiday$const.value,
              binaries$const.val,
              unfulfilled$const.value,
              workforce.min$const.value,
              workforce.max$const.value)#,
              #saturday$const.value)

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

const.val[1:29] = const.val[1:29] - 1

sol = lp(objective.in = cost.vector,
         direction = "min",
         const.mat = const.matrix,
         const.dir = const.dir,
         const.rhs = const.val,
         all.int = TRUE,
#         compute.sens = 1,
         use.rw = TRUE)

middle_time2 <- Sys.time()

# format the solution
solution = sol$solution
solution = solution[1:(calendar.length*n.radiologues)]
planning = format.planning(solution, df, calendar.length)



end_time <- Sys.time()
end_time - start_time
middle_time2 - middle_time

# workload
rowSums(planning) * 2
# constrariete
rowSums(unfulfilled$const.matrix %*% solution)
# workforce
workforce.min$const.matrix %*% solution
# planning
planning

#write.table(planning, 'suggested-planning.csv', sep = ';')




# to do:
# samedis
# connect input to planner
# workload per radiologue, connect with DF
# feedback on computation temination
# connect with real past schedule 

# done
# solution download


#a = 29*7 - compute.weekly.days.off(1,52)
#source('optim.R')
#minis = get.weekly.minimums(1, 52)
#workforce.min.constraint(df, 1, 52)$const.value
