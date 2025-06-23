#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
  int s;
  if (INTPTR_MAX == 0x7fffffffffffffff)
    s = 64;
  else if (INTPTR_MAX == 0x7fffffff)
    s = 32;
  else if (INTPTR_MAX == 0x7fff)
    s = 16;
  else {
    fprintf(stderr, "unknown word size\n");
    exit(1);
  }
  printf("#define WORD_SIZE_IN_BITS %d\n", s);
  uint32_t word = 0x12345678;
  int big;
  switch (((char *)&word)[0]) {
  case 0x78: big = 0; break;
  case 0x12: big = 1; break;
  default:
    fprintf(stderr, "unknown endianness\n");
    exit(1);
  }
  printf("#%s WORDS_BIGENDIAN\n", big ? "define" : "undef");

  exit(0);
}
