#include <stdint.h>

#ifdef WINDOWS_BUILD
__declspec(dllexport)
#endif
int64_t test(int64_t m, int64_t n) {
  return 1 + m + n;
}
