import numpy as np
from scipy.optimize import linprog
from scipy.optimize import fmin
from numpy.linalg import solve
from scipy.optimize import minimize


# Days in column, radiologues in lines
# Fist guess: everybody's working the first 8 days and rests the last 12
initial_guess = np.column_stack((np.ones((4, 8)), np.zeros((4, 12))))
# Stack of the columns from the matrix form,
# ie day schedule easy to see, radiologues with modulo[]
vectorised_guess = np.transpose(initial_guess).reshape(80, 1)

# On suppose ici que le nbr de jour par semaine est égal au nbr de radiologues
def holiday_constraint_vector(nbr_radiologues, nbr_semaines):
	holiday_vector = np.tile(np.eye(nbr_radiologues), (1, nbr_semaines))
	holiday_vector = np.transpose(holiday_vector).reshape(1, nbr_semaines * nbr_radiologues ** 2)
	return holiday_vector


def holiday_penalty(x):
	holiday_vector = np.tile(np.eye(4), (1, 5))
	holiday_vector = np.transpose(holiday_vector).reshape(1, 80)
	penalty = np.dot(holiday_vector, x)
	return penalty

def work_amount_penaly(x, nbr_radiologues = 4, work_amount_required = 8):
	penalty = 0
	for i in range(0, nbr_radiologues):
		days_radiologue_i = np.tile(np.eye(nbr_radiologues)[:, i], (1, 20))
		penalty = penalty + abs(np.dot(days_radiologue_i, x) - work_amount_required)
	return penalty

def cost(x):
	return holiday_penalty(x) + work_amount_penaly(x)

fmin(ridiculous, vectorised_guess)
minimize(cost, vectorised_guess, bounds = ((0, 1), (0, 1)) * 40)
# exec(open("optimTest.py").read())
