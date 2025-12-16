#include <stdint.h>

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
int64_t testi64(int64_t m, int64_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
int32_t testi32(int32_t m, int32_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
int16_t testi16(int16_t m, int16_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint64_t testu64(uint64_t m, uint64_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint32_t testu32(uint32_t m, uint32_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint16_t testu16(uint16_t m, uint16_t n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
float testf(float m, float n) {
  return 1 + m + n;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
double testd(double m, double n) {
  return 1 + m + n;
}
