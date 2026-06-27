#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

typedef double real;
 
     int rand_int(int n1, int n2)
{
  int res;

  if (n2 < n1) exit(2);

  res = n1 + rand() % (n2 - n1 + 1);
  printf("rand: %d\n", res);

  return res;
}

int rand_real(double n1, double n2)
{
  double res;

  if (n2 < n1) exit(2);

  res = (double)rand()/RAND_MAX * (n2 - n1) + n1;
  printf("rand: %f\n", res);

  return res;
}

int main(int argc, char *argv[])
{

  srand(time(NULL));

#define rand(x, y) rand_itv(x, y)
#line 1 "exp.tiny"
/* Computing e as the solution of xdot = x */
real x, delta;
int cpt, res;

x=1.;
cpt=0;
res=100;
delta=0.01; /* For the moment no cast btw types.
	       We cannot define delta = 1/res */
while (cpt < res) {
 x = x + delta*x;
 cpt++;
}

#line 54 "stdout"
  printf("At end of execution:\n");
  printf("cpt = %d\n", cpt);
  printf("delta = %f\n", delta);
  printf("res = %d\n", res);
  printf("x = %f\n", x);

  return 0;
}
