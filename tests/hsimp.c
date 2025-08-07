#include <stdio.h>
#include "Callback_stub.h"
#include "hsimp.h"

int
hsimp(int n)
{
  printf("enter hsimp n=%d\n", n);
  fflush(stdout);
  int r = hsexp(n);
  printf("exit hsimp hsexp=%d\n", r);
  return r;
}
