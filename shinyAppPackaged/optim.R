library('lpSolve')
library('data.table')
# parameters

folder = ""
#folder = getwd()
# La variable d'optimisation du problème est organisé par jour:
# Les 29 premiers indices correspondent à la présence de chaque radiologue
# le premier jour du calendrier considéré, puis les 29 suivant le deuxieme, etc
extract.data <- function(file.location = paste(folder, "data/dataPlanning.csv", sep = ''), saturday.penalty = 0)
{
  # Import and format data for rest day preferences
  df <- read.table(file.location, header = TRUE, sep = ";")
  # use strings instead of factor (the dataset is small)
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  # rest.preferences: 5 colonnes, 29 lignes. Un 1 quand un indice coerrespond a un jour de congé souhaité
  rest.preferences = matrix(ncol = 7, nrow = 0)
  # scrape rest days in dataframe and convert into desired format
  for(i in 1:nrow(df))
  {
    rest.vector = c(rep(0, 5), saturday.penalty, 0)
    rest.days = paste(df[i, 5], df[i, 6], df[i, 7], sep = '.')
    rest.days = strsplit(rest.days, '\\.')
    if('lu' %in% rest.days[[1]]) rest.vector[1] = 1
    if('ma' %in% rest.days[[1]]) rest.vector[2] = 1
    if('me' %in% rest.days[[1]]) rest.vector[3] = 1
    if('je' %in% rest.days[[1]]) rest.vector[4] = 1
    if('ve' %in% rest.days[[1]]) rest.vector[5] = 1
    rest.preferences = rbind(rest.preferences, rest.vector)
  }
  radiologues = df[, 1]
  weekdays = c("lu", "ma", "me", "je", "ve", "sa", "di")
  rownames(rest.preferences) <- radiologues
  colnames(rest.preferences) <- weekdays
  return(list("df" = df, "rest.preferences" = rest.preferences))
}


extract.holiday.data <- function(file.location = "data/conges-calendrier.csv",
                                 column.range = 4:367,
                                 row.range = 8:36)
{
  # Import and format data for rest day preferences
  df <- read.table(file.location, sep = ";")
  # use strings instead of factor (the dataset is small)
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  # rest.preferences: 5 colonnes, 29 lignes. Un 1 quand un indice coerrespond a un jour de congé souhaité
  df <- df[row.range, column.range]
  # invert 0 and 1 as the convention is opposite is the csv and in the program
  df = as.data.frame(sapply(df, function(x) abs(as.numeric(x) - 1)))
  return(df)
}

# depreciated, since we now have a different format for the csv file
extract.holiday.data2 <- function(file.location = "../data/holidays.csv")
{
  # Import and format data for rest day preferences
  df <- read.table(file.location, header = TRUE, sep = ";")
  # use strings instead of factor (the dataset is small)
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  # rest.preferences: 5 colonnes, 29 lignes. Un 1 quand un indice coerrespond a un jour de congé souhaité
  names <- df[, 1]
  df <- df[, -1]
  rownames(df) <- names
  return(df)
}

troncate.holiday.data <- function(holiday.df, start.week, end.week)
{
  return(holiday.df[, ((start.week - 1) * 7 + 1):(end.week * 7)])
}

# WORKLOAD #
# nature des contraintes
# pour l'intant égalité mais doit devenir inégalité +/- 5%
# Constraint matrix
# Chaque radiologue doit réaliser un certain nombre de jour de travail
# Il correspond a vacPerYear / averageVacPerday
# Chaque radiologue peut avoir une marge de +/- 5% sur l'année
# Ce differentiel est gardé en crédit positif ou négatif sur l'année suivante
# Calcul du nombre moyen de vacation par jour: (à préciser)
# nombre de jour ouvré par an = 250z
# 5 semaines de vacances = -25 jours
# Un jour de congé par semaine pour un temps plein
# fullTimeNbrWorkDay = floor((250 - 25)*4/5) = 180
# Il semble qu'un radiologue fasse deux vacation par jour de travail
# On estime le nbr de jour de travail par radiologue et par an en divisant son nbr de vacation par 2
workload.constraint2 <- function(df, n.week = 8, workload.min.factor = 0.98, workload.max.factor = 1.02)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  const.dir = rep(">=", n.radiologue)
  const.dir = c(const.dir, rep("<=", n.radiologue))
  # valeur des containtes (terme de droite)
  # egal au nombre de jour de boulot a realiser dans le calendrier pour chaque radiologue
  # La division par deux vient du fait qu'on considère 2 vac / jour de boulot
  workload.standard.vector = as.numeric(df[, 3])
  min.value = floor(workload.min.factor * workload.standard.vector * n.week / (2 * 52))
  max.value = floor(workload.max.factor * workload.standard.vector * n.week / (2 * 52)) + 1
  const.value = c(min.value, max.value)
  const.matrix = matrix(rep(diag(1, n.radiologue), calendar.length), n.radiologue, calendar.length * n.radiologue)
  const.matrix = rbind(const.matrix, const.matrix)
  return(list("const.matrix" = const.matrix , "const.dir" = const.dir, "const.value" = const.value))
}

workload.constraint <- function(df, n.week = 8, workload.min.factor = 1)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  const.dir = rep(">=", n.radiologue)
  # valeur des containtes (terme de droite)
  # egal au nombre de jour de boulot a realiser dans le calendrier pour chaque radiologue
  # La division par deux vient du fait qu'on considère 2 vac / jour de boulot
  const.value = floor(workload.min.factor * as.numeric(df[, 3]) * n.week / (2 * 52)) + 1
  const.matrix = matrix(rep(diag(1, n.radiologue), calendar.length), n.radiologue, calendar.length * n.radiologue)
  return(list("const.matrix" = const.matrix , "const.dir" = const.dir, "const.value" = const.value))
}

saturday.constraint <- function(df, n.week, saturday.frequency = 1 / 5)
{
  n.radiologue = nrow(df)
  const.value = floor( as.numeric(df[, 4]) * n.week / 8 * saturday.frequency)
  const.dir = rep(">=", n.radiologue)
  const.matrix = matrix(rep(c(rep(diag(0, n.radiologue), 5), diag(1, n.radiologue), diag(0, n.radiologue)), n.week), n.radiologue)
  return(list("const.matrix" = const.matrix , "const.dir" = const.dir, "const.value" = const.value))
}

# WORKLOAD #
# New version of workload constraint
# A chaque instant de l'année, chaque radiologue doit avoir effectué la proportion correspondante
# de sa charge de traivail annuelle, en prennant en compte les vacances passée et à venir pour assurer
# une charge de travail équirépartie sur les periodes de présence.
# On peut rajouter un underwork factor pour garder des cartouches pour les periodes de rush
# La fonction travail à l'echelle du calendrier de vacances, pas de l'année
# first week: début de la periode a générer par rapport a holiday.data
workload.constraint2 <- function(df, n.week = 8, week.start, work.meter, holiday.data, first.week)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  const.dir = rep(">=", n.radiologue)
  # valeur des containtes (terme de droite)
  total.workloard = floor(as.numeric(df[, 3]) * dim(holiday.data)[2] / (7 * 52))
  total.target.workload = (year.workload * n.week / (2 * 52) ) + 1
  const.matrix = matrix(rep(diag(1, n.radiologue), calendar.length), n.radiologue, calendar.length * n.radiologue)
  return(list("const.matrix" = const.matrix , "const.dir" = const.dir, "const.value" = const.value))
}


# chaque semaine, le nombre total de vacation atribuées doit etre superieur a un seuil
# sur cette version, ce seuil depend du nombre de radiologues disponibles (donc du nbr de radiologues en vacances)
workforce.min.constraint <- function(df, start.week, end.week, full.force.threshold = 75, workforce.min.factor = 0.6, max.minimum = 90)
{
  n.radiologues = nrow(df)
  n.week = end.week - start.week + 1
  calendar.length = n.week * 7
  const.matrix = c()
  get.weekly.min = get.weekly.minimums(start.week, end.week, n.radiologues, full.force.threshold, workforce.min.factor, max.minimum)[]
  weekly.min = get.weekly.min['minimums']
  for(i in 1:n.week)
  {
    ith.week.constraint = rep(0, n.radiologues * calendar.length)
    ith.week.constraint[((i - 1) * 7 * n.radiologues + 1):(i * 7 * n.radiologues)] = rep(1, 7)
    const.matrix = rbind(const.matrix, ith.week.constraint)
  }
  effective.week.workforce = sum(as.numeric(gsub(",",".", df[,"ETP"]))) * 4
  const.value = get.weekly.min$minimums
  const.dir = rep(">=", n.week)
  return(list("const.matrix" = const.matrix,
              "const.dir" = const.dir,
              "const.value" = const.value,
              'minimums' = get.weekly.min$minimums,
              'limits' = get.weekly.min$limits))
}

get.weekly.minimums <- function(start.week,
                                end.week,
                                n.radiologues,
                                full.force.threshold,
                                workforce.min.factor,
                                max.minimum)
{
  # number of available vacations
  weekly.ressources = n.radiologues * 7 - compute.weekly.days.off(start.week = start.week, end.week = end.week)
  minimums = c()
  for (i in 1:length(weekly.ressources))
  {
    max = weekly.ressources[i]
    if (max <= full.force.threshold)
    {
      minimums[i] = max
    }
    else
    {
      minimums[i] = workforce.min.factor * max
    }
    if (minimums[i] > max.minimum)
    {
      minimums[i] = max.minimum
    }
  }
  return(list("minimums" = floor(minimums), "limits" = weekly.ressources))
}

# compute the amount of people on holiday every week to output the maximum presence potential
compute.weekly.days.off <- function(start.week, end.week)
{
  holidays <- extract.holiday.data()
  days.off = c()
  for (week in start.week:end.week)
  {
    days.off = c(days.off, sum(holidays[, (7 * (week - 1) + 1):(7 * week)]))
  }
  return(days.off)
}



# chaque semaine, le nombre total de vacation atribuées doit etre superieur a un seuil
workforce.min.constraint2 <- function(df, n.week, workforce.factor)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7

  const.matrix = c()
  for(i in 1:n.week)
  {
    ith.week.constraint = rep(0, n.radiologue * calendar.length)
    ith.week.constraint[((i - 1) * 7 * n.radiologue + 1):(i * 7 * n.radiologue)] = rep(1, 7)
    const.matrix = rbind(const.matrix, ith.week.constraint)
  }
  effective.week.workforce = sum(as.numeric(gsub(",",".", df[,"ETP"]))) * 4
  const.value = rep(floor(effective.week.workforce * workforce.factor), n.week)
  const.dir = rep(">=", n.week)
  return(list("const.matrix" = const.matrix, "const.dir" = const.dir, "const.value" = const.value))
}

workforce.max.constraint <- function(df, n.week, workforce.max.vector)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7

  const.matrix = c()
  for(i in 1:n.week)
  {
    ith.week.constraint = rep(0, n.radiologue * calendar.length)
    ith.week.constraint[((i - 1) * 7 * n.radiologue + 1):(i * 7 * n.radiologue)] = rep(1, 7)
    const.matrix = rbind(const.matrix, ith.week.constraint)
  }
  effective.week.workforce = sum(as.numeric(gsub(",",".", df[,"ETP"]))) * 4
  const.value = rep(floor(effective.week.workforce * workforce.factor), n.week)
  const.dir = rep("<=", n.week)
  return(list("const.matrix" = const.matrix, "const.dir" = const.dir, "const.value" = const.value))
}

# balance constraint a integrer en flexible dans la fonction de cout


# SAMEDIS #
# Recalé jusqu'à nouvel ordre. Assigner les samedi dans un second temps
# remplacé par la contrainte no.saturday.constraint: la somme du travail le samedi doit être nulle
no.saturday.constraint <- function(df, n.week)
{
  n.radiologue = nrow(df)
  # Chaque samedi, un certain nombre de personnes doivent être en poste. saturday.total.workload)
  saturday.total.workload = 0
  saturday.total.const.value = rep(saturday.total.workload, n.week)
  saturday.total.const.dir = rep("==", n.week)
  saturday.total.const.matrix = c()
  # générer n.week vecteurs avec que des 0, et un 1 le jour du samedi concerné
  saturday.const.matrix = c()
  for(i in 1:n.week)
  {
    ith.saturday.vector = rep(0, n.radiologue * 7 * n.week)
    ith.saturday.vector[(n.radiologue * 7 * (i-1) + n.radiologue * 5 + 1):(n.radiologue * 7 * i)] = rep(1, n.radiologue)
    saturday.const.matrix = c(saturday.const.matrix, ith.saturday.vector)
  }
  saturday.const.matrix = matrix(saturday.const.matrix, nrow = n.week, byrow = TRUE)
  return(list("const.matrix" = saturday.const.matrix, "const.dir" = saturday.total.const.dir, "const.value" = saturday.total.const.value))
  # De plus, chacun doit travailler au minimum un nombre de samedi donné, proportionnel à sa charge hebdomadaire
  # Il est préférable de gérer les samedis dans un second temps, après l'optimisation
  # En effet, effectuer un samedi est une assignation sans préférence et sans influence
  # sur le reste de l'optim. On aura ainsi un meilleur controle sur l'historique,
  # et on pourra utiliser ce levier pour alléger une semaine où un radiologue aurait été très lésé.
}

generate.random.holiday.data <- function(df,
                                         n.week,
                                         holiday.proportion = 7 / 52)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7

  holiday = matrix(rep(0, n.radiologue * calendar.length), nrow = n.radiologue)
  for(r in 1:n.radiologue)
  {
    r.holidays = sample.int(calendar.length,
                            size = calendar.length * holiday.proportion,
                            replace = FALSE)
    for(h in r.holidays)
    {
      holiday[r, h] = 1
    }
  }
  radiologues = df[, 1]
  weekdays = c("lu", "ma", "me", "je", "ve", "sa")
  rownames(holiday) <- radiologues
  colnames(holiday) <- rep(weekdays, dim(holiday)[2] / 7)
  return(holiday)
}

holiday.constraint <- function(holiday.data, n.week)
{
  const.matrix = matrix(as.numeric(unlist(c(holiday.data))), 1)
  return(list("const.matrix" = const.matrix,
              "const.dir" = c("=="),
              "const.value" = c(0)))
}

# BINARY CONSTRAINT
force.binary.constraint <- function(df, n.week)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  n.constraints = n.radiologue * calendar.length
  const.matrix = diag(n.constraints)
  rsplit <- as.list(as.data.frame(t(const.matrix)))
  const.matrix = do.call(rbind,rsplit)
  const.value = rep(1, n.constraints)
  const.dir = rep("<=", n.constraints)
  return(list("const.matrix" = const.matrix, "const.dir" = const.dir, "const.value" = const.value))
}

# MAX UNFULFILLED PREFERENCE CONSTRAINT
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

# HISTORY CONSTRAINT
# history solution is formatted a planning solution
# if the length of it is 29*14, it will be assumed
# that these are the first two weeks of the year, to be set
force.history.constraint <- function(pre.planned, history.end.week)
{
  history.solution = c(pre.planned[1:(history.end.week * 7)])
  history.solution = Reduce(c, history.solution)
  n.constraints = length(history.solution)
  const.matrix = diag(n.constraints)
  rsplit <- as.list(as.data.frame(t(const.matrix)))
  const.matrix = do.call(rbind,rsplit)
  const.value = Reduce(c, history.solution)
  const.dir = rep("==", n.constraints)
  return(list("const.matrix" = const.matrix, "const.dir" = const.dir, "const.value" = const.value))
}

# Cost function:
# Combinaison lineraire des couts individuels sur les contraintes flexibles,
# pondérées par un coefficient dépendant de l'historique de pénalité.
# Generate the coeff vector for the cost function
# correspond to the rest-days flexible constraint
# A vector on the same pattern than the optim variable: 29 first indice correspond
# to the 29 radiologues on the first day of the calendar
cost.vector.rest.days <- function(rest.preferences,
                                  df,
                                  penalty.vector,
                                  n.week)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  rest.preferences.cost.vector = rep(c(rest.preferences), n.week)
  individual.preferences.matrix = matrix(nrow = 0, ncol = calendar.length * n.radiologue)
  for(i in 1:n.radiologue)
  {
    individual.interpolation = rep(0, n.radiologue)
    individual.interpolation[i] = 1
    individual.interpolation = rep(individual.interpolation, calendar.length)
    individual.rest.preferences = rest.preferences.cost.vector * individual.interpolation
    individual.preferences.matrix = rbind(individual.preferences.matrix, individual.rest.preferences)
  }
  # Lets generate a random penalty coefficient vector:
  penalty.coefficients = penalty.vector
  cost.vector.rest.days = colSums(diag(penalty.coefficients) %*% individual.preferences.matrix)
  return(cost.vector.rest.days)
}

cost.vector.week.balance <- function(df,
                                     n.week)
{
  n.radiologue = nrow(df)
  calendar.length = n.week * 7
  weekly.load.matrix = matrix(nrow = 0, ncol = calendar.length * n.radiologue)
  for(i in 1:n.radiologue)
  {
    individual.interpolation = rep(0, n.radiologue)
    individual.interpolation[i] = 1
    individual.interpolation = rep(individual.interpolation, calendar.length)
    individual.rest.preferences = rest.preferences.cost.vector * individual.interpolation
    individual.preferences.matrix = rbind(individual.preferences.matrix, individual.rest.preferences)
  }
  # Lets generate a random penalty coefficient vector:
  penalty.coefficients = penalty.vector
  cost.vector.rest.days = colSums(diag(penalty.coefficients) %*% individual.preferences.matrix)
  return(cost.vector.rest.days)
}

# Cost function due to change in the pre-planed solution
# One can show that with S the pre planed solution, and X the optimisation variable
# cost = c2 . 1 * X - (c1 + c2) S * X
# cost.vector = (c2 . 1 - (c1 + c2). S)
# induces a cost of c1 for a false positive (canceling a scheduled working day)
# as an oppotunity cost (ie 0 if we cancel -c1 if we don't cancel)
# and a direct cost of c2 if we plan someone who was not suppose to work
# pre.planned.length: number of days to take into account. Usually N - 7.
# ex: we wanna plan the next 8 week and we have the 7 first weeks pre-planned
# The ratio c1 over c2 allows to control for the importance of false negative as compared to false positive
cost.vector.changes <- function(pre.planned.sol,
                                first.week = 1,
                                last.week = 7,
                                c1 = 5,
                                c2 = 2)
{
      pre.planned.vector = c(pre.planned.sol[, ((first.week - 1) * 7 + 1):(last.week * 7)])
      pre.planned.length = 7 * (last.week - first.week + 1)
      pre.planned.vector = Reduce(c, pre.planned.vector)
      cost.vector.changes = rep(c2, pre.planned.length * dim(pre.planned.sol)[1]) - (c1 + c2) * pre.planned.vector
      cost.vector.changes = c(cost.vector.changes, rep(0, pre.planned.length * dim(pre.planned.sol)[1] - length(pre.planned.vector)))
      return(cost.vector.changes)
}

planning.solver <- function(cost.vector,
                            const.matrix,
                            const.dir,
                            const.val,
                            int.vect,
                            binary.vect,
                            all.bin = TRUE)
{
  sol = lp(objective.in = cost.vector,
           direction = "min",
           const.mat = const.matrix,
           const.dir = const.dir,
           const.rhs = const.val,
           all.bin = all.bin,
           compute.sens = 1,
           use.rw = TRUE)
  return(sol)
}


format.planning <- function(solution, df, calendar.length)
{
  radiologues = df[, 1]
  weekdays = c("lu", "ma", "me", "je", "ve", "sa", "di")
  sol.planning = matrix(solution, nrow = length(radiologues), byrow = FALSE)
  rownames(sol.planning) <- radiologues
  colnames(sol.planning) <- rep(weekdays, ncol(sol.planning) / 7)
  return(sol.planning)
}

format.planning2 <- function(sol, df, sol.number = 1)
{
  number.of.solution =  sol$num.bin.solns
  solution.length = (length(sol$solution)) / number.of.solution ## il y avait un -1 bizare qui a ete supprime
  radiologues = df[, 1]
  weekdays = c("lu", "ma", "me", "je", "ve", "sa", "di")
  solution.vector = sol$solution[((sol.number - 1) * solution.length + 1):(sol.number * solution.length)]
  sol.planning = matrix(solution.vector, nrow = length(radiologues), byrow = FALSE)
  rownames(sol.planning) <- radiologues
  colnames(sol.planning) <- rep(weekdays, dim(sol.planning)[2] / 7)
  return(sol.planning)
}

most.spread.conflicts.sol <- function(sol, df, rest.preferences)
{
  number.of.solution = sol$num.bin.solns
  best = Inf
  best.sol.indice = 0
  for(i in 1:number.of.solution)
  {
    vect = c()
    sol.mat = format.planning(sol, df, i)
    sol.mat = conflict(sol.mat, rest.preferences)
    vect = colSums(sol.mat)
    concentration = sd(vect)
    if(concentration < best)
    {
      best = concentration
      best.sol.indice = i
    }
  }
  return(best.sol.indice)
}

most.weekly.spread.conflicts <- function(sol, df, rest.preferences)
{
  number.of.solution = sol$num.bin.solns
  best = Inf
  n.week = dim(format.planning(sol, df, 1))[2] / 7
  best.sol.indice = 0
  for(i in 1:number.of.solution)
  {
    vect = c()
    sol.mat = format.planning(sol, df, i)
    sol.mat = conflict(sol.mat, rest.preferences)
    for(w in 1:n.week)
    {
      weekly.conflicts = sum(colSums(sol.mat[, (1 + (w-1) * 7):(w * 7)]))
      vect = c(vect, weekly.conflicts)
    }
    concentration = sd(vect)
    if(concentration < best)
    {
      best = concentration
      best.sol.indice = i
    }
  }
  return(best.sol.indice)
}

generate.planning <- function(history.vector = 1:29,
                              n.week = 8,
                              num.bin.solns = 100,
                              sol.number = 0, #when = 0 uses most_weekly_spread_conflicts
                              return.full.sol = FALSE,
                              dataPlanningFile = paste(folder, "data/dataPlanning.csv", sep = ''),
                              penalty.factor = 2, # penalty factor for preferences between first and last group
                              pre.planned = NA,
                              pre.planned.length = dim(pre.planned)[2] - 7, # number of days to take into account
                              c1 = -500, # cost for false positive
                              c2 = -200, # cost for false negative
                              number.of.groups = 4, # number of groups in penalty
                              overwork.factor = 1.1, # effective work as compared to required
                              saturday.penalty = 2,
                              holiday.file = NA,
                              first.week = 1,
                              holiday.proportion = 0.12,
                              workforce.factor = 0.8)
{
    df = extract.data(file.location = dataPlanningFile, saturday.penalty = 2)$df
    rest.preferences = extract.data(file.location = dataPlanningFile, saturday.penalty = 2)$rest.preferences
    # penalty.vector = generate.penalty.vector(history.vector, penalty.factor, number.of.groups)
    # alternate caluculation for penalty vector:
    penalty.vector = ((history.vector + 1) * penalty.factor) ^ 2  / ((max(history.vector + 1) * penalty.factor) ^ 2)
    penalty.vector = round(penalty.vector, 3)
    print('penalty vector:')
    print(penalty.vector)
    workload.constraint = workload.constraint(df,
                                              n.week = n.week,
                                              overwork.factor = overwork.factor)

    # if holiday data is not given, generate random
    if(is.null(dim(holiday.data)))
    {
      holiday.data = generate.random.holiday.data(df,
                                                  n.week,
                                                  holiday.proportion)
    }
    else
    {
      holiday.data = troncate.holiday.data(holiday.data, first.week, first.week + n.week - 1)
    }

    holiday.constraint = holiday.constraint(holiday.data)
    workforce.constraint = week.workforce.constraint(df, n.week, workforce.factor = workforce.factor)
    saturday.constraint = no.saturday.constraint(df, n.week)
    cost.vector = cost.vector.rest.days(rest.preferences,
                                        df,
                                        penalty.vector,
                                        n.week)
    if(!is.null(dim(pre.planned)))
    {
      cost.vector.change = cost.vector.changes(pre.planned, pre.planned.length, n.week = n.week, first.week = first.week, c1, c2)
      cost.vector = cost.vector + cost.vector.change
    }

    const.matrix = rbind(workload.constraint$const.matrix,
                         holiday.constraint$const.matrix,
                         workforce.constraint$const.matrix,
                         saturday.constraint$const.matrix)

    const.dir = c(workload.constraint$const.dir,
                  holiday.constraint$const.dir,
                  workforce.constraint$const.dir,
                  saturday.constraint$const.dir)

    const.val = c(workload.constraint$const.value,
                  holiday.constraint$const.value,
                  workforce.constraint$const.value,
                  saturday.constraint$const.value)

    sol <- planning.solver(cost.vector,
                           const.matrix,
                           const.dir,
                           const.val,
                           num.bin.solns = num.bin.solns)

    if(return.full.sol) return(sol)
    if(sol.number == 0) return(format.planning(sol, df, sol.number))
    else
    {
      best.sol.indice = most.weekly.spread.conflicts(sol, df, rest.preferences)
      sol.planning = format.planning(sol, df, best.sol.indice)
      return(sol.planning)
    }
}


recursive.generation <- function(holiday.data = NA,
                                 first.week = 1,
                                 last.week = 52,
                                 number.of.rounds = 51,
                                 num.bin.sol = 1,
                                 pre.planned.length = 7 * 7)
{
  planning = generate.planning(n.week = last.week,
                               first.week = first.week,
                               sol.number = 1)
  result.stack = planning
  penalty.vector.stack = rep(0, dim(planning)[1])
  NA.week = data.frame(matrix(rep(NA, dim(planning)[1] * 7), nrow = dim(planning)[1]))
  colnames(NA.week) = c("lu", "ma", "me", "je", "ve", "sa")
  rownames(NA.week) = rownames(planning)
  NA.weeks = NA.week
  for(week in first.week:(first.week + number.of.rounds))
  {
      planning = generate.planning(pre.planned = planning,
                                   n.week = last.week - week,
                                   first.week = week,
                                   sol.number = 1,
                                   pre.planned.length = min(pre.planned.length, (last.week - week) * 7))

      # stocker l'etat des 7 semaines a venir a chaque round
      penalty.vector.stack = rbind(penalty.vector.stack, current.penalties(planning, rest.preferences, week))

      planning = cbind(NA.weeks, planning) # coller NA dataframe pour la periode deja ecoulee
      result.stack = rbind(result.stack, planning)
      NA.weeks = cbind(NA.weeks, NA.week)
      browser()
      # stocker la version reactualisee a chaaue round
  }
  return(result.stack)
}


continue.planning <- function(planning,
                              df = extract.data(file.location = paste(folder, "data/dataPlanning.csv", sep = ''),
                                                saturday.penalty = 2)$df,
                              rest.preferences = extract.data(file.location = paste(folder, "data/dataPlanning.csv", sep = ''),
                                                              saturday.penalty = 2)$rest.preferences,
                              n.week = 8,
                              penalty.factor = 2, # penalty factor between first and last group
                              number.of.groups = 4, # number of groups in penalty
                              overwork.factor = 1,
                              saturday.penalty = 2,
                              holiday.proportion = 0.12,
                              holiday.data = NA,
                              workforce.factor = 0.8)
{
  first.week = (dim(planning)[2] / 7) + 1
  history.vector = current.penalties(planning, rest.preferences)
  following.planning = generate.planning(history.vector = history.vector,
                                         n.week = n.week,
                                         df = df,
                                         rest.preferences = rest.preferences,
                                         penalty.factor = penalty.factor, # penalty factor for preferences between first and last group
                                         number.of.groups = number.of.groups, # number of groups in penalty
                                         overwork.factor = overwork.factor, # effective work as compared to required
                                         saturday.penalty = saturday.penalty,
                                         holiday.proportion = holiday.proportion,
                                         holiday.data = 0,
                                         workforce.factor = workforce.factor)
  new.planning = cbind(planning, following.planning)
  print(current.penalties(new.planning, rest.preferences))
  return(new.planning)
}

# On a 4 sacs, chacuns remplis de tous les numéros des radiologues temps plein
# Idem 3 sacs avec les 3/4 pleins
# Idem 2 sacs avec les 1/2 pleins
# On vide de manière équirépartie les sacs des 3/4 et des 1/2 pleins dans
# les 4 sacs des full time sans qu'il y ait de doublons dans un sac!
# On tire au sort dans un sac jusqu'à ce qu'il soit vide, puis on recommence.
# OU
# On a un grand sac avec tous autant de fois le numéro de chaque radiologue qu'il ne doit faire de samedi dans l'année
# On tire au sort, avec un garde fou pour pas bosser deux samedis de suite

generate.new.waitlist <- function(df)
{
  full.time = which(df[, 3] == 370)
  three.quarter = which(df[, 3] == 278)
  half.time = which(df[, 3] == 185)

  waitlist = c(sample(full.time),
               sample(tree.quarter),
               sample(full.time),
               sample(half.time),
               sample(tree.quarter),
               sample(full.time),
               sample(tree.quarter),
               sample(full.time),
               sample(half.time))
  return(waitlist)
}

attribute.saturday <- function(planning, df, waitlist)
{

}


# FROM PLANNING FEEDBACK

# Inputs a planning and a preference table
# Generates the list of conflict for each radiologue
conflict <- function(sol.planning, rest.preferences)
{
  n.week = dim(sol.planning)[2] / 6
  n.radiologue = dim(sol.planning)[1]
  sol.planning * matrix(rep(rest.preferences, n.week), nrow = n.radiologue)
}

current.penalties <- function(sol.planning, rest.preferences, first.n.weeks = 0)
{
  if(first.n.weeks > 0)
  {
    sol.planning = sol.planning[, 1:(6 * first.n.weeks)]
    rest.preferences = rest.preferences
  }
  conf = conflict(sol.planning, rest.preferences)
  rowSums(conf)
}

generate.penalty.vector <- function(history.vector, penalty.factor, number.of.groups = 4)
{
  penalty.vector = rank(history.vector, ties.method="min") %/% number.of.groups
  penalty.vector = rep(1, length(history.vector)) + penalty.vector * penalty.factor / number.of.groups
  return(penalty.vector)
}


# Inclus:
# - Les jours de travail preferentiel
# - Workload mini par practicien et par calendrier
# - Samedi, nbr mini de practicien présent (pas équitable) ???
# - equité dans le non respect des containtes
# - vacances

# A ajouter:
# - Ajouter un max de postes par jour: fouchette
# - virer le samedi de la liste des preferences
# - intégrer que la workload en periode normale est inferieur a une simple moyenne car suactivité en Juillet Aout
# - pour que toutes les pénalités dues au overwork ne soient pas attribuées la meme semaine pour tous les radiologues,
# ajouter une contrainte de total work sum per week proportionnel au nbr de radiologues present et au overwork factor
# - formations

# - 370 / an, 2 par jour dans 90%, entre 3 et 5 par semaines
# - formation: absence qui se decompte dans les vacation payés (limite de 10)
# - les deux peter qui veulent bosser ensemble
# - differnecier les pénalité par radio et par jour
# - On peut introduire des demis-journées
# Tous les samedis, on a besoin de 3 radiologues.

# qu'est ce qui rend le planning pas respectable?
# - Un nbr mini de radiologues par jour
# - jours feriés
# - Des maladies, ou jours feriés vacances. Pour l'instant chaque radiologue peut ne pas bosser
# tous les jours qu'il désire sans contrarier de contrainte. (nbr de vacation parfaitement calibré)

# question:
# - jours feriés et connecter avec calendrier
# - Autant de samedi pour tout le monde ou plus pour les temps pleins? 5 par seamedi
# Un samedi travaillé = 1 ou 2 vacation?

# changer à la journée ou demi-journée

#rapport: connecter les deux phases ()#

#A présenter:
#somme des jours travaillés par radiologue
#conflicts#

#A faire:
#considérer le nombre de vac mini proportionnel à la somme des vacances par semaine.
#peridode roulante / vision à 7 semaine.
#Profils radiologues.
#Les deux peter syncro
# forcer un travail hebdomadaire minimal quand on est pas en vacances

# DEBUG:
# lorsquon run un render avec 4 semaines ou moins, le tableau est vide...
# probablement pas de solution factible

# DEAL WITH THE GOD DAMN SAMEDIS
