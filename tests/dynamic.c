#include "dynamic.h"

/* Each result needs the exact C type: with a wider integer type the
   upper bits of a negative or narrow result would be wrong. */
int sub3(int a, int b, int c) { return a - b - c; }
int8_t neg8(int8_t x) { return -x; }
uint16_t inc16(uint16_t x) { return x + 1; }
float halve(float x) { return x / 2; }
double scale(double x, long n) { return x * n; }
long long big(void) { return -1234567890123LL; }
void store(int *p, int x) { *p = x; }
void (*pick(int i))(void) { return i ? (void (*)(void))sub3 : (void (*)(void))neg8; }
