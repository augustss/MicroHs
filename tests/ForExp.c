#include <stdio.h>
#include "ForExp_stub.h"

int
main(int argc, char **argv)
{
  mhs_init();
  printf("%d\n", (int)funcName(5));
  printf("%d\n", (int)other(6, 7));
}
