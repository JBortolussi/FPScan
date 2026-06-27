#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

typedef float real;
int rand_int(int n1, int n2)
{
  int res;

  if (n2 < n1) exit(2);

  res = n1 + rand() % (n2 - n1 + 1);
  printf("rand: %d\n", res);

  return res;
}

int rand_real(float n1, float n2)
{
  float res;

  if (n2 < n1) exit(2);

  res = (float)rand()/RAND_MAX * (n2 - n1) + n1;
  printf("rand: %f\n", res);

  return res;
}

int main(int argc, char *argv[])
{

  srand(time(NULL));

#define rand(x, y) rand_itv(x, y)
#line 1 "patriot.tiny"
real x;
int i;

i = 0;
x = 0.;
while (i <= 10000) {
  i++;
  x += 0.1;
}

#line 49 "stdout"
  printf("At end of execution:\n");
  printf("i = %d\n", i);
  printf("x = %f\n", x);

  return 0;
}
