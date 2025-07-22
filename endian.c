#include <stdio.h>
#include <inttypes.h>

int
main(int argc, char **argv)
{
  uint32_t x = 0x12345678;
  uint8_t *p = (void*)&x;
  printf("%02x %02x %02x %02x\n", p[0], p[1], p[2], p[3]);
  return (0);
}
