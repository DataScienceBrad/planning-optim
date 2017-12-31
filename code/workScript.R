source('optim.R')
source('planningFeedback.R')

library('lpSolve')
library('data.table')

# global vars
n.radiologues = 29
dataPlanningFile = paste(folder, "data/dataPlanning.csv", sep = '')
history.vector = 1:n.radiologues
penalty.factor = 2
workforce.factor = 0.6 # proportion de travail minimal total hebdomadaire / standard
overwork.factor = 0.4 # proportion minimal totale par worker pour la workload constraint
unbalanced.factor = 1 / 10
num.bin.solns = 1
max.unfulfilled = 10
# generation variable

start.week = 1
end.week = 51
n.week = end.week - start.week + 1
holiday.data = extract.holiday.data()
holiday.data = troncate.holiday.data(holiday.data, start.week, end.week)
radiologues.data = extract.data(file.location = dataPlanningFile, saturday.penalty = 2)
df = radiologues.data$df
rest.preferences = radiologues.data$rest.preferences

rownames(holiday.data) = rownames(rest.preferences)
class(holiday.data)

penalty.vector = ((history.vector + 1) * penalty.factor) ^ 2  / ((max(history.vector + 1) * penalty.factor) ^ 2)
penalty.vector = round(penalty.vector, 3)
penalty.vector = rep(1, length(penalty.vector))

# one single line
holiday = holiday.constraint(holiday.data)
# one line per week
workforce = workforce.constraint(df, n.week, workforce.factor = workforce.factor)
# one line per radio
workload = workload.constraint(df, n.week = n.week, overwork.factor = overwork.factor)
saturday = saturday.constraint(df, n.week)
binaries = force.binary.constraint(df, n.week)
unfulfilled = max.unfulfilled.constraint(rest.preferences, df, n.week, max.unfulfilled = max.unfulfilled)

rowSums(matrix(holiday$const.matrix, n.radiologues))
sum(holiday$const.matrix)
# la contrainte du samedi doit etre un nombre de samedi par an et par personne
# (different pour temps plein / demi)

cost.vector = cost.vector.rest.days(rest.preferences, df, penalty.vector, n.week)

const.matrix = rbind(workload$const.matrix,
                     holiday$const.matrix,
                     binaries$const.matrix,
#                     unfulfilled$const.matrix)#,
                     workforce$const.matrix)#,
                     #saturday$const.matrix)

dim(binaries$const.matrix)

const.dir = c(workload$const.dir,
              holiday$const.dir,
              binaries$const.dir,
          #    unfulfilled$const.dir)#,
              workforce$const.dir)#,
              #saturday$const.dir)

const.val = c(workload$const.value,
              holiday$const.value,
              binaries$const.val,
          #    unfulfilled$const.dir)#,
              workforce$const.value)#,
              #saturday$const.value)

length(cost.vector)
length(const.val)
length(const.dir)
dim(const.matrix)

(const.val)

# get all the full holiday weeks to exculde these from the artifical variables
vacances = read.csv(paste(folder, "data/semaines-conges.csv", sep = ''), header = TRUE, sep = ';')
vacances = vacances[3:31, 4:11]
df.vacances = sapply(as.data.frame(vacances), function(x) as.numeric(substring(x,2)))
#nbr.semaines.vacances = 0
#for (i in start.week:end.week)
#{
#  nbr.semaines.vacances = nbr.semaines.vacances + as.numeric(table(df.vacances)[i])
#}



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

# unbalance constraint creation
calendar.length = n.week * 7
#artificial.size =  2 * (n.radiologues * n.week - nbr.semaines.vacances)



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

length(cost.vector)
length(const.val)
length(const.dir)
dim(const.matrix)
dim(balance.matrix)

# ralonger les matrices avec des 0 en fonction de la taille des bogus variables
int.vect = c()
binary.vect = 1:ncol(const.matrix)
balancing = TRUE
if (balancing)
{
  cost.vector = c(cost.vector, rep(unbalanced.factor, artificial.size))

  const.matrix = cbind(const.matrix, matrix(rep(0, dim(const.matrix)[1] * artificial.size), dim(const.matrix)[1]))
  const.matrix = rbind(const.matrix, balance.matrix)
#  int.vect = (max(binary.vect)+1):(ncol(const.matrix))
}

length(cost.vector)
length(const.val)
length(const.dir)
dim(const.matrix)
dim(balance.matrix)

start_time <- Sys.time()

# Time difference of 1.000327 mins
sol = lp(objective.in = cost.vector,
         direction = "min",
         const.mat = const.matrix,
         const.dir = const.dir,
         const.rhs = const.val,
         all.int = TRUE,
#         compute.sens = 1,
         use.rw = TRUE)

solution = sol$solution
solution = solution[1:(calendar.length*n.radiologues)]

middle_time <- Sys.time()

rowSums(unfulfilled$const.matrix * solution)


unfulfilled$const.matrix[1,1:29]


planning = format.planning(solution, df, calendar.length)

end_time <- Sys.time()

end_time - start_time

planning
#rowSums(planning)

write.table(planning, 'suggested-planning.csv', sep = ';')
# ajouter le dimanche dans le vecteur, vacances pour tout le monde






max.unfulfilled.constraint <- function(rest.preferences,
                                       df,
                                       n.week,
                                       max.unfulfilled = 5)
{
  n.radiologues = nrow(df)
  calendar.length = n.week * 7
  rest.preferences.cost.vector = rep(c(rest.preferences), n.week)
  individual.preferences.matrix = matrix(nrow = 0, ncol = calendar.length * n.radiologues)
  for(i in 1:n.radiologues)
  {
    individual.interpolation = rep(0, n.radiologues)
    individual.interpolation[i] = 1
    individual.interpolation = rep(individual.interpolation, calendar.length)
    individual.rest.preferences = rest.preferences.cost.vector * individual.interpolation
    individual.preferences.matrix = rbind(individual.preferences.matrix, individual.rest.preferences)
  }
  return(list("const.matrix" = individual.preferences.matrix,
              "const.dir" = rep("<=", n.radiologues),
              "const.value" = rep(max.unfulfilled, n.radiologues)))
}
