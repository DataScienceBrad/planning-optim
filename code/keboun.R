




planning.optim <- function(penalty.vector,
                           year.fraction = 1/6,
                           week.per.year = 45,
                           preference.file = "~/Desktop/planningRadio/data/dataPlanning.csv")
{
  weekdays = c("lu", "ma", "me", "je", "ve", "sa")
  n.week = floor(week.per.year * year.fraction)


  # Import and format data for rest day preferences
  df <- read.table(preference.file, header = TRUE, sep = ";")
  # use strings instead of factor (the dataset is small)
  df <- data.frame(lapply(df, as.numeric), stringsAsFactors=FALSE)
  n.radiologue = nrow(df)
  radiologues = df[, 1]

  # rest.preferences: 5 colonnes, 29 lignes. Un 1 quand un indice coerrespond a un jour de congé souhaité
  rest.preferences = matrix(ncol = 6, nrow = 0)
  # scrape rest days in dataframe and convert into desired format
  for(i in 1:nrow(df))
  {
    rest.vector = c(rep(0, 5), 1)
    rest.days = paste(df[i, 5], df[i, 6], df[i, 7], sep = '.')
    rest.days = strsplit(rest.days, '\\.')
    if('lu' %in% rest.days[[1]]) rest.vector[1] = 1
    if('ma' %in% rest.days[[1]]) rest.vector[2] = 1
    if('me' %in% rest.days[[1]]) rest.vector[3] = 1
    if('je' %in% rest.days[[1]]) rest.vector[4] = 1
    if('ve' %in% rest.days[[1]]) rest.vector[5] = 1
    rest.preferences = rbind(rest.preferences, rest.vector)
  }
  rownames(rest.preferences) <- radiologues
  colnames(rest.preferences) <- weekdays


  # Cost function:
  # Combinaison lineraire des couts individuels sur les contraintes flexibles,
  # pondérées par un coefficient dépendant de l'historique de pénalité.
  # Generate the coeff vector for the cost function
  # correspond to the rest-days flexible constraint
  # A vector on the same pattern than the optim variable: 29 first indice correspond
  # to the 29 radiologues on the first day of the calendar
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
  # code syntax probably not optimal speedwise
  penalty.coefficients = penalty.vector
  cost.function.vector = colSums(diag(penalty.coefficients) %*% individual.preferences.matrix)

  ### CONSTRAINTS ###

  # WORKLOAD #
  # nature des contraintes
  # pour l'intant égalité mais doit devenir inégalité +/- 5%
  workload.const.dir = rep("==", n.radiologue)
  # valeur des containtes (terme de droite)
  # egal au nombre de jour de boulot a realiser dans le calendrier pour chaque radiologue
  # La division par deux vient du fait qu'on considère 2 vac / jour de boulot
  workload.const.value = floor(as.numeric(df[, 3]) * year.fraction / 2) + 1
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
  workload.const.matrix = matrix(rep(diag(1, n.radiologue), calendar.length), n.radiologue, calendar.length * n.radiologue)

  # SAMEDIS #
  # Chaque samedi, un certain nombre de personnes doivent être en poste. saturday.total.workload)
  saturday.total.workload = 1
  saturday.total.const.value = rep(saturday.total.workload, n.week)
  saturday.total.const.dir = rep(">=", n.week)
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
  # De plus, chacun doit travailler au minimum un nombre de samedi donné, proportionnel à sa charge hebdomadaire
  # Il est préférable de gérer les samedis dans un second temps, après l'optimisation
  # En effet, effectuer un samedi est une assignation sans préférence et sans influence
  # sur le reste de l'optim. On aura ainsi un meilleur controle sur l'historique,
  # et on pourra utiliser ce levier pour alléger une semaine où un radiologue aurait été très lésé.


  # Sum the various constraints
  const.matrix = rbind(workload.const.matrix, saturday.const.matrix)
  const.dir = c(workload.const.dir, saturday.const.dir)
  const.value = c(workload.const.value, saturday.const.value)


  # Solve the god damn thing:
  sol = lp(objective.in = cost.function.vector, const.mat = const.matrix, const.dir = const.dir, const.rhs = const.value, all.bin = TRUE)
  sol.planning = matrix(sol$solution, nrow = n.radiologue, byrow = FALSE)
  rownames(sol.planning) <- radiologues
  colnames(sol.planning) <- rep(weekdays, n.week)
  return(sol.planning)
}
