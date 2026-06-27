#include <fenv.h>
#include <stdio.h>
#include <stdlib.h>

/* Set floating point rounding mode to 'towards +oo'
 * (required for interval arithmetic). */
static void setround()
{
  double d;

  if (fesetround(FE_UPWARD)) goto err;

  d = 1 + 0x1p-54;

  if (d <= 1) goto err;

  return;
 err:
  fprintf(stderr, "Error: Unable to set rounding mode to 'toward +oo'.\n");
  exit(2);
}
