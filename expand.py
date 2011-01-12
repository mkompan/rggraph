from sys import stdin
from sympy import *

q_1 = Symbol('q_1')
q_2 = Symbol('q_2')
q_3 = Symbol('q_3')
q_4 = Symbol('q_4')
theta_1 = Symbol('theta_1')
theta_2 = Symbol('theta_2')
theta_3 = Symbol('theta_3')
theta_4 = Symbol('theta_4')
theta_5 = Symbol('theta_5')
theta_6 = Symbol('theta_6')
theta_7 = Symbol('theta_7')
theta_8 = Symbol('theta_8')
a_1 = Symbol('a_1')
a_2 = Symbol('a_2')
a_3 = Symbol('a_3')
mu2 = Symbol('mu2')

for line in stdin:
	exec("print(ccode((%s).subs({mu2:1})))" % line)

