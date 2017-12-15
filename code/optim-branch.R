library('lpSolve')
library('data.table')
source('planningFeedback.R')
# parameters

# La variable d'optimisation du problème est organisé par jour:
# Les 29 premiers indices correspondent à la présence de chaque radiologue
# le premier jour du calendrier considéré, puis les 29 suivant le deuxieme, etc

extract.data <- function(file.location = "~/Desktop/planningRadio/data/dataPlanning.csv",
                         saturday.penalty = 5)
{
  # Import and format data for rest day preferences
  df <- read.table(file.location, header = TRUE, sep = ";")
  # use strings instead of factor (the dataset is small)
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  # rest.preferences: 5 colonnes, 29 lignes. Un 1 quand un indice coerrespond a un jour de congé souhaité
  rest.preferences = matrix(ncol = 6, nrow = 0)
  # scrape rest days in dataframe and convert into desired format
  for(i in 1:nrow(df))
  {
    rest.vector = c(rep(0, 5), saturday.penalty)
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
  weekdays = c("lu", "ma", "me", "je", "ve", "sa")
  rownames(rest.preferences) <- radiologues
  colnames(rest.preferences) <- weekdays
  return(list("df" = df, "rest.preferences" = rest.preferences))
}

extract.holiday.data <- function(file.location = "~/Desktop/planningRadio/data/holidays.csv")
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
  return(holiday.df[, ((start.week - 1) * 6 + 1):(end.week * 6)])
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
workload.constraint <- function(df, year.fraction = 1/4, overwork.factor = 1)
{
  n.radiologue = nrow(df)
  n.week = floor(52 * year.fraction)
  calendar.length = n.week * 6
  const.dir = rep("==", n.radiologue)
  # valeur des containtes (terme de droite)
  # egal au nombre de jour de boulot a realiser dans le calendrier pour chaque radiologue
  # La division par deux vient du fait qu'on considère 2 vac / jour de boulot
  const.value = floor(overwork.factor * as.numeric(df[, 3]) * year.fraction / 2) + 1
  const.matrix = matrix(rep(diag(1, n.radiologue), calendar.length), n.radiologue, calendar.length * n.radiologue)
  return(list("const.matrix" = const.matrix , "const.dir" = const.dir, "const.value" = const.value))
}

week.balance.constraint <- function(df, year.fraction, balance.factor)
{
  n.radiologue = nrow(df)
  n.week = floor(52 * year.fraction)
  calendar.length = n.week * 6

  const.matrix = c()
  for(i in 1:n.week)
  {
    ith.week.constraint = rep(0, n.radiologue * calendar.length)
    ith.week.constraint[((i - 1) * 6 * n.radiologue + 1):(i * 6 * n.radiologue)] = rep(1, 6)
    const.matrix = rbind(const.matrix, ith.week.constraint)
  }

  effective.week.workforce = sum(as.numeric(gsub(",",".", df[,"ETP"]))) * 4
  const.value = rep(floor(effective.week.workforce * balance.factor), n.week)
  const.dir = rep(">=", n.week)
  return(list("const.matrix" = const.matrix, "const.dir" = const.dir, "const.value" = const.value))
}

# SAMEDIS #
# Recalé jusqu'à nouvel ordre. Assigner les samedi dans un second temps
# remplacé par la contrainte no.saturday.constraint: la somme du travail le samedi doit être nulle
saturday.constraint <- function(df, year.fraction)
{
  n.radiologue = nrow(df)
  n.week = floor(52 * year.fraction)
  # Chaque samedi, un certain nombre de personnes doivent être en poste. saturday.total.workload)
  saturday.total.workload = 0
  saturday.total.const.value = rep(saturday.total.workload, n.week)
  saturday.total.const.dir = rep("==", n.week)
  saturday.total.const.matrix = c()
  # générer n.week vecteurs avec que des 0, et un 1 le jour du samedi concerné
  saturday.const.matrix = c()
  for(i in 1:n.week)
  {
    ith.saturday.vector = rep(0, n.radiologue * 6 * n.week)
    ith.saturday.vector[(n.radiologue * 6 * (i-1) + n.radiologue * 5 + 1):(n.radiologue * 6 * i)] = rep(1, n.radiologue)
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
                                         year.fraction,
                                         holiday.proportion = 1 / 12)
{
  n.radiologue = nrow(df)
  n.week = floor(52 * year.fraction)
  calendar.length = n.week * 6

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
  colnames(holiday) <- rep(weekdays, dim(holiday)[2] / 6)
  return(holiday)
}

holiday.constraint <- function(holiday.data)
{
  const.matrix = c(holiday.data)
  return(list("const.matrix" = const.matrix,
              "const.dir" = c("=="),
              "const.value" = c(0)))
}


# Cost function:
# Combinaison lineraire des couts individuels sur les contraintes flexibles,
# pondérées par un coefficient dépendant de l'historique de pénalité.
# Generate the coeff vector for the cost function
# correspond to the rest-days flexible constraint
# A vector on the same pattern than the optim variable: 29 first indice correspond
# to the 29 radiologues on the first day of the calendar
# Penalty vector is to favor people who have had unmet constraints in the past
cost.function <- function(rest.preferences,
                          df,
                          penalty.vector,
                          year.fraction,
                          pre.planned = c())
{
  browser()
  n.radiologue = nrow(df)
  n.week = floor(52 * year.fraction)
  calendar.length = n.week * 6
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
  cost.function.vector = colSums(diag(penalty.vector) %*% individual.preferences.matrix)
  return(cost.function.vector)
}


planning.solver <- function(cost.function,
                            const.matrix,
                            const.dir,
                            const.value,
                            num.bin.solns)
{
  sol = lp(objective.in = cost.function, const.mat = const.matrix, const.dir = const.dir, const.rhs = const.value,
           all.bin = TRUE,
           compute.sens = 1,
           num.bin.solns = num.bin.solns)
  return(sol)
}


format.planning <- function(sol, df, sol.number = 1)
{
  number.of.solution =  sol$num.bin.solns
  solution.length = (length(sol$solution) - 1) / number.of.solution
  radiologues = df[, 1]
  weekdays = c("lu", "ma", "me", "je", "ve", "sa")
  solution.vector = sol$solution[((sol.number - 1) * solution.length + 1):(sol.number * solution.length)]
  sol.planning = matrix(solution.vector, nrow = length(radiologues), byrow = FALSE)
  rownames(sol.planning) <- radiologues
  colnames(sol.planning) <- rep(weekdays, dim(sol.planning)[2] / 6)
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
    print(i)
    print(vect)
    print(sol.mat)
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
  n.week = dim(format.planning(sol, df, 1))[2] / 6
  best.sol.indice = 0
  for(i in 1:number.of.solution)
  {
    vect = c()
    sol.mat = format.planning(sol, df, i)
    sol.mat = conflict(sol.mat, rest.preferences)
    for(w in 1:n.week)
    {
      weekly.conflicts = sum(colSums(sol.mat[, (1 + (w-1) * 6):(w * 6)]))
      vect = c(vect, weekly.conflicts)
    }
    concentration = sd(vect)
    if(concentration < best)
    {
      best = concentration
      best.sol.indice = i
    }
  }
  sol = format.planning(sol, df, best.sol.indice)
  return(sol)
}


generate.planning <- function(history.vector = sample(29),
                              year.fraction = 1 / 7,
                              num.bin.solns = 100,
                              sol.number = 0,
                              return.full.sol = FALSE,
                              df = extract.data(file.location = "~/Desktop/planningRadio/data/dataPlanning.csv",
                                                saturday.penalty = 2)$df,
                              rest.preferences = extract.data(file.location = "~/Desktop/planningRadio/data/dataPlanning.csv",
                                                              saturday.penalty = 2)$rest.preferences,
                              penalty.factor = 2, # penalty factor for preferences between first and last group
                              number.of.groups = 4, # number of groups in penalty
                              overwork.factor = 1.1, # effective work as compared to required
                              saturday.penalty = 2,
                              holiday.data = 0,
                              holiday.proportion = 0.12,
                              balance.factor = 0.8)
{
    # penalty.vector = generate.penalty.vector(history.vector, penalty.factor, number.of.groups)
    # alternate caluculation for penalty vector:
    penalty.vector = ((history.vector + 1) * penalty.factor) ^ 2  / ((max(history.vector + 1) * penalty.factor) ^ 2)
    penalty.vector = round(penalty.vector, 3)
    print(penalty.vector)
    workload.constraint = workload.constraint(df,
                                              year.fraction = year.fraction,
                                              overwork.factor = overwork.factor)
    # if holiday data is not given, generate random
    if(holiday.data == 0)
    {
      holiday.data = generate.random.holiday.data(df,
                                                  year.fraction,
                                                  holiday.proportion)
    }
    holiday.constraint = holiday.constraint(holiday.data)
    balance.constraint = week.balance.constraint(df, year.fraction, balance.factor = balance.factor)
    saturday.constraint = saturday.constraint(df, year.fraction)
    cost.function = cost.function(rest.preferences,
                                  df,
                                  penalty.vector,
                                  year.fraction)

    const.matrix = rbind(workload.constraint$const.matrix,
                         holiday.constraint$const.matrix,
                         balance.constraint$const.matrix,
                         saturday.constraint$const.matrix)
    const.dir = c(workload.constraint$const.dir,
                  holiday.constraint$const.dir,
                  balance.constraint$const.dir,
                  saturday.constraint$const.dir)

    const.val = c(workload.constraint$const.value,
                  holiday.constraint$const.value,
                  balance.constraint$const.value,
                  saturday.constraint$const.value)

    sol <- planning.solver(cost.function,
                           const.matrix,
                           const.dir,
                           const.val,
                           num.bin.solns = num.bin.solns)

    if(return.full.sol) return(sol)
    if(sol.number != 0) return(format.planning(sol, df, sol.number))
    else
    {
      sol.planning = most.weekly.spread.conflicts(sol, df, rest.preferences)
      return(sol.planning)
    }
}


continue.planning <- function(planning,
                              df = extract.data(file.location = "~/Desktop/planningRadio/data/dataPlanning.csv",
                                                saturday.penalty = 2)$df,
                              rest.preferences = extract.data(file.location = "~/Desktop/planningRadio/data/dataPlanning.csv",
                                                              saturday.penalty = 2)$rest.preferences,
                              year.fraction = 1 / 7,
                              penalty.factor = 2, # penalty factor between first and last group
                              number.of.groups = 4, # number of groups in penalty
                              overwork.factor = 1,
                              saturday.penalty = 2,
                              holiday.proportion = 0.12,
                              balance.factor = 0.8)
{
  history.vector = current.penalties(planning, rest.preferences)
  following.planning = generate.planning(history.vector = history.vector,
                                         year.fraction = year.fraction,
                                         df = df,
                                         rest.preferences = rest.preferences,
                                         penalty.factor = penalty.factor, # penalty factor for preferences between first and last group
                                         number.of.groups = number.of.groups, # number of groups in penalty
                                         overwork.factor = overwork.factor, # effective work as compared to required
                                         saturday.penalty = saturday.penalty,
                                         holiday.proportion = holiday.proportion,
                                         balance.factor = balance.factor)
  new.planning = cbind(planning, following.planning)
  print(current.penalties(new.planning, rest.preferences))
  return(new.planning)
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
