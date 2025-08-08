#include <stdint.h>
#include <stdlib.h>
#include "mhsffi.h"
#include "hsasp.h"

void
hsasp(uintptr_t sp)
{
  char *ret = apply_sp(sp, "arg");
  printf("hsasp: %s\n", ret);
  free(ret);
}
