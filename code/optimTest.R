library('lpSolve')

# 5 radiologues
# 20 jours
# 2 radiologues en poste par jour, 8 jour de boulot par radio

n.radiologues = 5
n.week = 4

rest.preferences = apply(diag(1, n.radiologues), 2, rep, n.week)

# Vecteur coefficient fonction de cout:
# equivaut à la contrainte des jours de congé préférés
# Pour l'instant, un jour de congé par personne par semaine
# => Le vecteur de coeff coerrespond a une matrice identité
# mise en stack par colonnes.

objective.in = c(t(rest.preferences))

# Matrice de contraintes
# Equité du travail, ie tout le monde doit travailler entre 7 et 8 fois dawns le mois

individual.work.matrix = apply(diag(1, n.radiologues), 2, rep, n.week * n.radiologues)
const.mat = t(individual.work.matrix)

# nature des contraintes

const.dir = rep("==", 5)

# valeur des containtes (terme de droite)

const.rhs = rep(8, 5)

# Solve the god damn thing:

sol = lp(objective.in = objective.in, const.mat = const.mat, const.dir = const.dir, const.rhs = const.rhs, all.bin = TRUE)
matrix(sol$solution, 20, 5)
