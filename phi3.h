/* dumb file to handle vert(), prop() and friends with C preprocessor */
#define vert(x)	1
#define prop(x)	(1/((x)+mu2))
#define dMu2(x) diff(x,mu2)
#define dA_1(x) diff(x,a_1)
#define dA_2(x) diff(x,a_2)
#define dA_3(x) diff(x,a_3)
#define dA_4(x) diff(x,a_4)
#define dA_5(x) diff(x,a_5)
#define dA_6(x) diff(x,a_6)
#define dA_7(x) diff(x,a_7)

