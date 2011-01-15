from sys import stdin
from sympy import *

q_1 = Symbol('q_1')
q_2 = Symbol('q_2')
q_3 = Symbol('q_3')
q_4 = Symbol('q_4')
x_1 = Symbol('x_1')
x_2 = Symbol('x_2')
x_3 = Symbol('x_3')
x_4 = Symbol('x_4')
x_5 = Symbol('x_5')
x_6 = Symbol('x_6')
x_7 = Symbol('x_7')
x_8 = Symbol('x_8')
y_1 = Symbol('y_1')
y_2 = Symbol('y_2')
y_3 = Symbol('y_3')
y_4 = Symbol('y_4')
y_5 = Symbol('y_5')
y_6 = Symbol('y_6')
y_7 = Symbol('y_7')
y_8 = Symbol('y_8')
a_1 = Symbol('a_1')
a_2 = Symbol('a_2')
a_3 = Symbol('a_3')
mu2 = Symbol('mu2')

for line in stdin:
	exec("print(ccode((%s).subs({mu2:1})))" % line)

