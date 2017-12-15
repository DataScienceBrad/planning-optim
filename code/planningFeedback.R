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

## Faire une fonction qui analyse les penalites dans le temps
