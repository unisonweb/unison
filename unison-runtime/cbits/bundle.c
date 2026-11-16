/* The section of a bundle that holds its program, on macOS
 * (src/Unison/Runtime/Bundle.hs). Elsewhere a bundle's program follows the
 * executable, and there is nothing to find here. */
#include <stddef.h>
#include <stdint.h>

#if defined(__APPLE__)
#include <mach-o/getsect.h>
#include <mach-o/ldsyms.h>

const uint8_t *unisonrt_bundle_section(size_t *size) {
  unsigned long n = 0;
  const uint8_t *p = getsectiondata(&_mh_execute_header, "__UNISON", "__uc", &n);
  *size = p ? n : 0;
  return p;
}
#endif
