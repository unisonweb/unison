/* unisonrt's entry point. It is GHC's own (the main a Haskell program gets
 * without -no-hs-main), plus one thing: the RTS reads its +RTS flags only
 * before the .uc file, so that the arguments after it reach the program
 * as they are, +RTS or not. It finds the file the way Main.hs does, the
 * first argument that is not a flag, and puts --RTS in front of it.
 * The flags that take their value as the next argument are listed here
 * and in Main.hs. */
#include <stdlib.h>
#include <string.h>
#include "Rts.h"

extern StgClosure ZCMain_main_closure;

static int takes_value(const char *arg) {
  return strcmp(arg, "--jit") == 0 || strcmp(arg, "--jit-dump-ir") == 0;
}

int main(int argc, char *argv[]) {
  char **args = malloc(sizeof(char *) * (argc + 2));
  int n = 0, i = 0, rts = 0, placed = 0;
  args[n++] = argv[i++];
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
  conf.rts_opts_enabled = RtsOptsAll;
  /* ucm's defaults (unison-cli-main/package.yaml), less its statistics */
  conf.rts_opts = "-I0 -N -qn4 -qg1";
  conf.rts_hs_main = true;
  return hs_main(n, args, &ZCMain_main_closure, conf);
}
