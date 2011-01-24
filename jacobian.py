from sys import stdin,argv
from sympy import *

def jacobian(j,n):
	if (n == 1):
		return j*q_1**(d-1)
	if (n == 2):
		return j*q_1**(d-1)*q_2**(d-1)
	if (n == 3):
		return j*q_1**(d-1)*q_2**(d-1)*q_3**(d-1)
	if (n == 4):
		return j*q_1**(d-1)*q_2**(d-1)*q_3**(d-1)*q_4**(d-1)
	return j

def taylN(e,v,n):
	return (diff(e,v,n)).subs({v:0})/factorial(n)

eps = Symbol('eps')
d = S(6) - eps
q_1 = Symbol('q_1')
q_2 = Symbol('q_2')
q_3 = Symbol('q_3')
q_4 = Symbol('q_4')
y_1 = Symbol('y_1')
y_2 = Symbol('y_2')
y_3 = Symbol('y_3')
y_4 = Symbol('y_4')
y_5 = Symbol('y_5')
y_6 = Symbol('y_6')
y_7 = Symbol('y_7')
y_8 = Symbol('y_8')
r = Symbol('r')

n = int(argv[1])

for line in stdin:
	exec("j=%s" % line)
	for i in range(0,5-n):
		print ccode(taylN(jacobian(j,n),eps,i))

