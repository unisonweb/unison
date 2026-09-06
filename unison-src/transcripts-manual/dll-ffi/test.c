#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

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
int8_t testi8(int8_t m, int8_t n) {
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
uint8_t testu8(uint8_t m, uint8_t n) {
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

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
void testptr(uint64_t sz, uint8_t *ptr) {
  uint64_t i = 0;
  uint8_t j = 1;
  while (i < sz) {
    ptr[i++] = j++;
  }
}

uint32_t arr[10] = {0,1,2,3,4,5,6,7,8,9};

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint32_t *getptr() {
  return arr;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint32_t accessarr(uint64_t i) {
  return arr[i];
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
void testptr2(uint64_t sz, uint32_t *ptr) {
  uint64_t i = 0;
  uint32_t j = 10;
  while (i < sz) {
    ptr[i++] = j++;
  }
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint32_t *allocptr() {
  uint32_t *ptr;
  ptr = malloc(sizeof(uint32_t));
  return ptr;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
void freeptr(uint32_t *ptr) {
  free(ptr);
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
int64_t testvarsum(int32_t count, ...) {
  va_list args;
  va_start(args, count);
  int64_t sum = 0;
  for (int32_t i = 0; i < count; ++i) {
    sum += va_arg(args, int64_t);
  }
  va_end(args);
  return sum;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
double testvarmixed(float bias, int32_t count, ...) {
  va_list args;
  va_start(args, count);
  double sum = bias;
  for (int32_t i = 0; i < count; ++i) {
    sum += va_arg(args, int);
    sum += va_arg(args, double);
  }
  va_end(args);
  return sum;
}

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
uint32_t testvarpointer(int32_t unused, ...) {
  va_list args;
  va_start(args, unused);
  uint32_t *p = va_arg(args, uint32_t *);
  va_end(args);
  return *p;
}
