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
overwork.factor = 1 # proportion minimal totale par worker pour la workload constraint
unbalanced.factor = 1 / 10
num.bin.solns = 100
# generation variable

start.week = 1
end.week = 52
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

holiday = holiday.constraint(holiday.data)
workforce = workforce.constraint(df, n.week, workforce.factor = workforce.factor)
workload = workload.constraint(df, n.week = n.week, overwork.factor = overwork.factor)
saturday = saturday.constraint(df, n.week)
rowSums(matrix(holiday$const.matrix, n.radiologues))
sum(holiday$const.matrix)
# la contrainte du samedi doit etre un nombre de samedi par an et par personne
# (different pour temps plein / demi)

cost.vector = cost.vector.rest.days(rest.preferences, df, penalty.vector, n.week)
length(cost.vector)
cost.vector = c(cost.vector, rep(unbalanced.factor, n.radiologues * n.week))

# unbalance constraint creation
n.radiologues = 2
n.week = 3
calendar.length = n.week * 7
artificial.size =  2 * n.radiologues * n.week
balance.matrix = matrix(nrow = 0, ncol = calendar.length * n.radiologues + artificial.size)
artificial = 1
for (radiologue in 1:n.radiologues)
{
  for (week in 1:n.week)
  {
    pos.indiv.weekly.interpolation = rep(0, calendar.length * n.radiologues)
    neg.indiv.weekly.interpolation = rep(0, calendar.length * n.radiologues)
    for (day in 1:7)
    {
      pos.indiv.weekly.interpolation[((week - 1) * 7 + (day - 1)) * n.radiologues + radiologue] = 1
      neg.indiv.weekly.interpolation[((week - 1) * 7 + (day - 1)) * n.radiologues + radiologue] = -1
    }
    # append artifical variables region
    artificial.region = rep(0, artificial.size)
    artificial.region[artificial] = -1
    pos.indiv.weekly.interpolation = c(pos.indiv.weekly.interpolation, artificial.region)
    balance.matrix = rbind(balance.matrix, pos.indiv.weekly.interpolation)
    artificial.region = rep(0, artificial.size)
    artificial.region[artificial + 1] = -1
    neg.indiv.weekly.interpolation = c(neg.indiv.weekly.interpolation, artificial.region)
    balance.matrix = rbind(balance.matrix, neg.indiv.weekly.interpolation)
    artificial = artificial + 2
  }
}


const.matrix = rbind(workload$const.matrix,
                     holiday$const.matrix,
                     workforce$const.matrix)#,
                     #saturday$const.matrix)
# ralonger les matrices avec des 0 en fonction de la taille des bogus variables



const.dir = c(workload$const.dir,
              holiday$const.dir,
              workforce$const.dir)#,
              #saturday$const.dir)

const.val = c(workload$const.value,
              holiday$const.value,
              workforce$const.value)#,
              #saturday$const.value)

sol <- planning.solver(cost.vector,
                       const.matrix,
                       const.dir,
                       const.val,
                       num.bin.solns)

sol

planning = format.planning(sol, df)

rowSums(planning)

write.table(planning, 'suggested-planning.csv', sep = ';')
# ajouter le dimanche dans le vecteur, vacances pour tout le monde
