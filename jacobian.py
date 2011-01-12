from sys import stdin,argv
from sympy import *

def jacobian(j,n):
	if (n == 1):
		return j.subs({r:q_1})
	if (n == 2):
		return (j.subs({r:q_1}))*(j.subs({r:q_2}))
	if (n == 3):
		return (j.subs({r:q_1}))*(j.subs({r:q_2}))*(j.subs({r:q_3}))
	if (n == 4):
		return (j.subs({r:q_1}))*(j.subs({r:q_2}))*(j.subs({r:q_3}))*(j.subs({r:q_4}))
	return j

def taylN(e,v,n):
	return (diff(e,v,n)).subs({v:0})/factorial(n)

eps = Symbol('eps')
d = S(6) - eps
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
r = Symbol('r')

n = int(argv[1])

for line in stdin:
	exec("j=%s" % line)
	for i in range(0,5-n):
		print ccode(taylN(jacobian(j,n),eps,i))

