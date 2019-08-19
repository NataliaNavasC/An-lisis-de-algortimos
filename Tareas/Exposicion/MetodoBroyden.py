
# -------------------------------------------------------------------------------------
# Código realizado por:
# * Natalia Navas
# * Jorge Salgado
# * Sebastián Santamaría
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 1903
# -------------------------------------------------------------------------------------

from __future__ import division
import numpy as np
import scipy.linalg as sla

def broyden_good(x, y, f_equations, J_equations, tol=10e-10, maxIters=50):
    steps_taken = 0

    f = f_equations(x,y)
    J = J_equations(x,y)

    while np.linalg.norm(f,2) > tol and steps_taken < maxIters:

        s = sla.solve(J,-1*f)

        x = x + s[0]
        y = y + s[1]
        newf = f_equations(x,y)
        z = newf - f

        J = J + (np.outer ((z - np.dot(J,s)),s)) / (np.dot(s,s))

        f = newf
        steps_taken += 1

    return steps_taken, x, y

tol = 10.0** -15
maxIters = 50
x0 = 1
y0 = 2

def fs(x,y):
    return np.array([x + 2*y - 2, x**2 + 4*y**2 - 4])

def Js(x,y):
    return np.array([[1,2],
             [2, 16]])

n, x, y = broyden_good(x0, y0, fs, Js, tol, maxIters=50 )
print "iterations: ", n
print "x and y: ", x, y
