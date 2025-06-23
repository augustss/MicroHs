#include <inttypes.h>
#include <stdio.h>

int
main(int argc, char **argv)
{
  short s;
  printf("#define WORD_SIZE_IN_BITS %d\n",
         INTPTR_MAX == 0x7fffffffffffffff ? 64 : 32);
  s = 0x1234;
  printf("#%s WORDS_BIGENDIAN\n", ((char *)&s)[0] == 0x34 ? "undef" : "define");

  return 0;
}
