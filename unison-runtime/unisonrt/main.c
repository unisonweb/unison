/* unisonrt's entry point. It is GHC's own (the main a Haskell program gets
 * without -no-hs-main), plus one thing: the RTS reads its +RTS flags only
 * before the .uc file, so that the arguments after it reach the program
 * as they are, +RTS or not. It finds the file the way Main.hs does, the
 * first argument that is not a flag, and puts --RTS in front of it.
 * The flags that take their value as the next argument are listed here
 * and in Main.hs. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(_WIN32)
#include <windows.h>
#else
#include <unistd.h>
#endif
#include "Rts.h"

extern StgClosure ZCMain_main_closure;

/* Whether this executable is a bundle (`unisonrt bundle`): unisonrt and a
 * compiled program, whose arguments are all the program's. On macOS the
 * program is in the section __UNISON,__uc, which starts with the magic
 * below; elsewhere it follows the executable, which ends with a trailer:
 * the program's offset (8 bytes, little-endian), then the magic. Bundle.hs
 * reads and writes both. */
#define BUNDLE_MAGIC "UNISONRT"

#if defined(__APPLE__)
/* cbits/bundle.c */
const uint8_t *unisonrt_bundle_section(size_t *size);

static int is_bundle(void) {
  size_t n;
  const uint8_t *p = unisonrt_bundle_section(&n);
  return p && n >= 16 && memcmp(p, BUNDLE_MAGIC, 8) == 0;
}
#else
static int is_bundle(void) {
  char path[4096];
#if defined(_WIN32)
  DWORD n = GetModuleFileNameA(NULL, path, sizeof(path));
  if (n == 0 || n == sizeof(path)) return 0;
#else
  ssize_t n = readlink("/proc/self/exe", path, sizeof(path) - 1);
  if (n < 0) return 0;
  path[n] = 0;
#endif
  FILE *f = fopen(path, "rb");
  if (!f) return 0;
  char trailer[16];
  int ok = fseek(f, -16, SEEK_END) == 0 && fread(trailer, 1, 16, f) == 16 &&
           memcmp(trailer + 8, BUNDLE_MAGIC, 8) == 0;
  fclose(f);
  return ok;
}
#endif

static int takes_value(const char *arg) {
  return strcmp(arg, "--jit") == 0 || strcmp(arg, "--jit-dump-ir") == 0;
}

int main(int argc, char *argv[]) {
  char **args = malloc(sizeof(char *) * (argc + 2));
  int n = 0, i = 0, rts = 0, placed = 0;
  args[n++] = argv[i++];
  int bundle = is_bundle();
  if (bundle) placed = 1;
  for (; i < argc; i++) {
    const char *a = argv[i];
    if (!placed) {
      if (rts) {
        if (strcmp(a, "-RTS") == 0) rts = 0;
      } else if (strcmp(a, "+RTS") == 0) {
        rts = 1;
      } else if (strcmp(a, "--RTS") == 0) {
        placed = 1;
      } else if (a[0] == '-') {
        if (takes_value(a) && i + 1 < argc) args[n++] = argv[i++];
      } else {
        args[n++] = "--RTS";
        placed = 1;
      }
    }
    args[n++] = argv[i];
  }
  args[n] = NULL;

  RtsConfig conf = defaultRtsConfig;
  /* A bundle's arguments are all the program's: the RTS reads none of them,
   * though it still reads GHCRTS. (Inserting --RTS, as below for a .uc
   * file, would not do: on Windows the RTS reads the process's command line
   * from the system, not the argv given here.) */
  conf.rts_opts_enabled = bundle ? RtsOptsIgnore : RtsOptsAll;
  /* ucm's defaults (unison-cli-main/package.yaml), less its statistics */
  conf.rts_opts = "-I0 -N -qn4 -qg1";
  conf.rts_hs_main = true;
  return hs_main(n, args, &ZCMain_main_closure, conf);
}
