source('optim.R')
source('planningFeedback.R')

library('lpSolve')
library('data.table')

# global vars
dataPlanningFile = paste(folder, "data/dataPlanning.csv", sep = '')
history.vector = 1:29
penalty.factor = 2
balance.factor = 0.6 # proportion de travail minimal total hebdomadaire / standard
overwork.factor = 1 # proportion minimal totale par worker pour la workload constraint
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
balance = week.balance.constraint(df, n.week, balance.factor = balance.factor)
workload = workload.constraint(df, n.week = n.week, overwork.factor = overwork.factor)
saturday = saturday.constraint(df, n.week)
rowSums(matrix(holiday$const.matrix, 29))
sum(holiday$const.matrix)
# la contrainte du samedi doit etre un nombre de samedi par an et par personne
# (different pour temps plein / demi)

cost.vector = cost.vector.rest.days(rest.preferences, df, penalty.vector, n.week)
length(cost.vector)

const.matrix = rbind(workload$const.matrix,
                     holiday$const.matrix,
                     balance$const.matrix)#,
                     #saturday$const.matrix)


const.dir = c(workload$const.dir,
              holiday$const.dir,
              balance$const.dir)#,
              #saturday$const.dir)

const.val = c(workload$const.value,
              holiday$const.value,
              balance$const.value)#,
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
