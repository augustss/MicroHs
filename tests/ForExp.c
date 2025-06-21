#include <stdint.h>
#include <stdio.h>

typedef intptr_t value_t;
void mhs_init(void);
value_t funcName(value_t x1);
value_t other(value_t x1, value_t x2);


int
main(int argc, char **argv)
{
  mhs_init();
  printf("%d\n", (int)funcName(5));
  printf("%d\n", (int)other(6, 7));
}
