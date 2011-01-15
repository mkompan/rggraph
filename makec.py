#!/usr/bin/python

from sys import argv
from string import find,join

a_vs = ['a_1','a_2','a_3','a_4']
y_vs = ['y_1','y_2','y_3','y_4','y_5','y_6','y_7','y_8']

head = """
#include <math.h>
#include <stdio.h>
#include <vegas.h>
#include <stdlib.h>

#define DIMENSION (N_M+N_Y+N_A)
#define FUNCTIONS 1
#define ITERATIONS 10000
#define NTHREADS 2
#define NITER 2
#define NEPS 0
#ifndef PI
#define PI	3.14159265358979323846
#endif
"""

tail = """
int t_gfsr_k;
unsigned int t_gfsr_m[SR_P];
double gfsr_norm;


int main(int argc, char **argv)
{
  int i;
  long long npoints;
  int nthreads;
  int niter;
  double region_delta;
  double reg[2*DIMENSION];
  int idx;
  if(argc >= 2)
    {
      npoints = atoll(argv[1]);

    }
  else 
    {
      npoints = ITERATIONS;
    }

  if(argc >= 3)
    {
      nthreads = atoi(argv[2]);

    }
  else 
    {
      nthreads = NTHREADS;
    }
   
   if(argc >= 5)
    {
      region_delta = atof(argv[4]);

    }
  else 
    {
      region_delta = 0.;
    } 

  if(argc >= 4)
    {
      niter = atoi(argv[3]);

    }
  else 
    {
      niter = NITER;
    }    
    
    for(idx=0; idx<2*DIMENSION; idx++)
      {
         if(idx<DIMENSION)
           {
             reg[idx] = reg_initial[idx]+region_delta;
           }
         else
           {
             reg[idx] = reg_initial[idx]-region_delta;
           }
      }
      
  double estim[FUNCTIONS];   /* estimators for integrals                     */
  double std_dev[FUNCTIONS]; /* standard deviations                          */
  double chi2a[FUNCTIONS];   /* chi^2/n                                      */

  vegas(reg, DIMENSION, func,
        0, npoints/10, 5, NPRN_INPUT | NPRN_RESULT,
        FUNCTIONS, 0, nthreads,
        estim, std_dev, chi2a);
  vegas(reg, DIMENSION, func,
        2, npoints , niter, NPRN_INPUT | NPRN_RESULT,
        FUNCTIONS, 0, nthreads,
        estim, std_dev, chi2a);
double delta= std_dev[0]/estim[0];
printf ("result = %20.18g\\nstd_dev = %20.18g\\ndelta = %20.18g\\n", estim[0], std_dev[0], delta);
//  printf ("Result %d: %g +/- %g delta=%g\\n",NEPS, estim[0], std_dev[0], delta);
//  for (i=1; i<FUNCTIONS; ++i)
//    printf("Result %i:\\t%g +/- %g  \\tdelta=%g\\n", i, estim[i], std_dev[i],std_dev[i]/estim[i]);
  return(0);
}
"""

def check_vars(vs,expr):
	return map(lambda x : find(expr,x) != -1,vs)

def create_limits(val,jac):
	s = "double reg_initial[2*DIMENSION] = {"
	l = []
	r = []
	for i in range(0,n):
		l.append(0)
		r.append(1)
	for (y,f) in zip(y_vs,check_vars(y_vs,jac)):
		if f:
			l.append(-1)
			r.append(1)
	for (a,f) in zip(a_vs,check_vars(a_vs,val)):
		if f:
			l.append(0)
			r.append(1)
	s += join(map(str,l+r),',')
	s += "};\n\n"
	return s

def moment_vars(val):
	s = """	double p_%d = k[%d];
	double q_%d = p_%d/(1-p_%d);"""
	res = join(map(lambda i : s % (i+1,i,i+1,i+1,i+1), range(0,n)),'\n')
	res += "\n\tdouble jac_moments = " + join(map(lambda i : "1/pow(1-p_%d,2)" % (i+1), range(0,n)),'*') + ";\n"
	return res

def angle_vars(jac):
	s = """	double x_%d = k[%d];
	double y_%d = sqrt(1-pow(x_%d,2));"""
	r = []
	js = []
	i = 0;
	for (y,f) in zip(y_vs,check_vars(y_vs,jac)):
		if f:
			k = int(y[2:])
			r.append(s % (k,n+i,k,k))
			js.append('(-' + y + ')')
			i += 1
	if len(js) <> 0:
		j_a = '1/(' + join(js,'*') + ')'
	else:
		j_a = '1'
	res = join(r,'\n') + '\n\tdouble jac_angles = ' + j_a + ';\n'
	return res

def param_vars(val,pos):
	s = """	double a_%d = k[%d];"""
	r = []
	for (a,f) in zip(a_vs,check_vars(a_vs,val)):
		if f:
			k = int(a[2:])
			r.append(s % (k,pos))
			pos += 1
	return join(r,'\n')

intf = open(argv[1])
jacf = open(argv[2])

out_dir = argv[3]
n = int(argv[4])

i = 0

for val in intf:
	i = i+1
	j = -1
	for jac in jacf:
		j = j+1
		filename = "%s/eps%d_part%d.c" % (out_dir,j,i)
		print filename
		out = open(filename,"w")
		out.write(head)
		out.write("#define N_M %d\n" % n)
		out.write("#define N_Y %d\n" % sum(check_vars(y_vs,jac)))
		out.write("#define N_A %d\n\n\n" % sum(check_vars(a_vs,val)))
		out.write(create_limits(val,jac))
		out.write("void func(double k[DIMENSION], double f[FUNCTIONS])\n{\n")
		out.write(moment_vars(val))
		out.write(angle_vars(jac))
		pos = n + sum(check_vars(y_vs,jac))
		out.write(param_vars(val,pos))
		out.write("""
	double val = %s;
	double jac = %s;

	f[0] = val*jac*jac_moments*jac_angles;
}""" % (val,jac))
		out.write(tail)
		out.close()
	jacf.seek(0)


