#include <stdint.h>

double unisonWord64ToDouble(uint64_t lin) {
  union {
    double d;
    uint64_t l;
  } u;
  u.l = lin;
  return u.d;
}

float unisonWord32ToFloat(uint32_t lin) {
  union {
    float f;
    uint32_t l;
  } u;
  u.l = lin;
  return u.f;
}
