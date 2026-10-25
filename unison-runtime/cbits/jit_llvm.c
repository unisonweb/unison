// The JIT's only use of LLVM: a small shim over the C API. Haskell calls these
// functions and nothing else from LLVM (why a shim of our own, and why IR text:
// src/Unison/Runtime/JIT/LLVM.hs).
//
// LLVM is not linked in. It is loaded with dlopen when the JIT is turned on,
// and the thirty-odd functions used are looked up with dlsym, so that building
// and running ucm need no LLVM at all unless the JIT is asked for. For the
// same reason there are no LLVM headers here: the declarations below are our
// own copies of the C API's, which has kept these types and values stable
// since LLVM 12. The recent ones are LLVMRunPasses (17) and LLVMGetVersion
// (16); LLVMOrcCreateNewThreadSafeContextFromLLVMContext (21) is used when
// the library has it and the older pair (LLVMOrcCreateNewThreadSafeContext
// and LLVMOrcThreadSafeContextGetContext, deprecated in 21) when not. 20 is
// the oldest version accepted because it is the oldest tested (Homebrew's
// llvm@20, apt.llvm.org's and Ubuntu 24.04's llvm-20).

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dirent.h>
#include <dlfcn.h>
#include <unistd.h>
#endif

// The dynamic loader, in the two spellings. The library is never closed
// once it is the one in use (generated code lives in it).
#if defined(_WIN32)
static void *lib_open(const char *path) { return (void *)LoadLibraryA(path); }
static void *lib_sym(void *lib, const char *name) { return (void *)GetProcAddress((HMODULE)lib, name); }
static void lib_close(void *lib) { FreeLibrary((HMODULE)lib); }
static const char *lib_error(void) {
  static char buf[256];
  snprintf(buf, sizeof buf, "LoadLibrary failed, error %lu", (unsigned long)GetLastError());
  return buf;
}
#else
static void *lib_open(const char *path) { return dlopen(path, RTLD_NOW | RTLD_LOCAL); }
static void *lib_sym(void *lib, const char *name) { return dlsym(lib, name); }
static void lib_close(void *lib) { dlclose(lib); }
static const char *lib_error(void) { return dlerror(); }
#endif


// The settings that change code generation or weaken the runtime are read
// only when UNISON_JIT_DEBUG is set, like JIT.Config's, so that a stray
// variable in the environment of a program's user does nothing.
static int jit_debug(void) {
  static int v = -1;
  if (v < 0) v = getenv("UNISON_JIT_DEBUG") != NULL;
  return v;
}
static const char *debug_getenv(const char *name) { return jit_debug() ? getenv(name) : NULL; }

// ---- Declarations copied from llvm-c (Types.h, Error.h, Orc.h, TargetMachine.h)

typedef int LLVMBool;
typedef struct LLVMOpaqueContext *LLVMContextRef;
typedef struct LLVMOpaqueModule *LLVMModuleRef;
typedef struct LLVMOpaqueMemoryBuffer *LLVMMemoryBufferRef;
typedef struct LLVMOpaqueError *LLVMErrorRef;
typedef struct LLVMTarget *LLVMTargetRef;
typedef struct LLVMOpaqueTargetMachine *LLVMTargetMachineRef;
typedef struct LLVMOpaquePassBuilderOptions *LLVMPassBuilderOptionsRef;
typedef struct LLVMOrcOpaqueLLJIT *LLVMOrcLLJITRef;
typedef struct LLVMOrcOpaqueLLJITBuilder *LLVMOrcLLJITBuilderRef;
typedef struct LLVMOrcOpaqueJITTargetMachineBuilder *LLVMOrcJITTargetMachineBuilderRef;
typedef struct LLVMOrcOpaqueJITDylib *LLVMOrcJITDylibRef;
typedef struct LLVMOrcOpaqueThreadSafeContext *LLVMOrcThreadSafeContextRef;
typedef struct LLVMOrcOpaqueThreadSafeModule *LLVMOrcThreadSafeModuleRef;
typedef struct LLVMOrcOpaqueSymbolStringPoolEntry *LLVMOrcSymbolStringPoolEntryRef;
typedef struct LLVMOrcOpaqueMaterializationUnit *LLVMOrcMaterializationUnitRef;
typedef uint64_t LLVMOrcExecutorAddress;

typedef struct {
  uint8_t GenericFlags;
  uint8_t TargetFlags;
} LLVMJITSymbolFlags;

typedef struct {
  LLVMOrcExecutorAddress Address;
  LLVMJITSymbolFlags Flags;
} LLVMJITEvaluatedSymbol;

typedef struct {
  LLVMOrcSymbolStringPoolEntryRef Name;
  LLVMJITEvaluatedSymbol Sym;
} LLVMOrcCSymbolMapPair;

enum { LLVMJITSymbolGenericFlagsExported = 1, LLVMJITSymbolGenericFlagsCallable = 4 };
enum { LLVMCodeGenLevelDefault = 2 };
enum { LLVMRelocDefault = 0 };
enum { LLVMCodeModelJITDefault = 1 };

// ---- The functions used, as pointers filled in by dlsym.
// X(name, return type, argument types)

#if defined(__aarch64__) || defined(__arm64__)
#define ARCH AArch64
#define ARCH_SUPPORTED 1
#elif defined(__x86_64__)
#define ARCH X86
#define ARCH_SUPPORTED 1
#else
#define ARCH Unsupported
#define ARCH_SUPPORTED 0
#endif
#define CAT3(a, b, c) a##b##c
#define CAT3_(a, b, c) CAT3(a, b, c)
#define TARGET_FN(suffix) CAT3_(LLVMInitialize, ARCH, suffix)

#define LLVM_FUNCTIONS(X)                                                                      \
  X(LLVMGetVersion, void, (unsigned *, unsigned *, unsigned *))                                \
  X(LLVMGetErrorMessage, char *, (LLVMErrorRef))                                               \
  X(LLVMDisposeErrorMessage, void, (char *))                                                   \
  X(LLVMDisposeMessage, void, (char *))                                                        \
  X(LLVMContextCreate, LLVMContextRef, (void))                                                 \
  X(LLVMDisposeModule, void, (LLVMModuleRef))                                                  \
  X(LLVMPrintModuleToString, char *, (LLVMModuleRef))                                          \
  X(LLVMCreateMemoryBufferWithMemoryRangeCopy, LLVMMemoryBufferRef,                            \
    (const char *, size_t, const char *))                                                      \
  X(LLVMParseIRInContext, LLVMBool, (LLVMContextRef, LLVMMemoryBufferRef, LLVMModuleRef *, char **)) \
  X(LLVMGetDefaultTargetTriple, char *, (void))                                                \
  X(LLVMGetHostCPUName, char *, (void))                                                        \
  X(LLVMGetHostCPUFeatures, char *, (void))                                                    \
  X(LLVMGetTargetFromTriple, LLVMBool, (const char *, LLVMTargetRef *, char **))               \
  X(LLVMCreateTargetMachine, LLVMTargetMachineRef,                                             \
    (LLVMTargetRef, const char *, const char *, const char *, int, int, int))                  \
  X(LLVMCreatePassBuilderOptions, LLVMPassBuilderOptionsRef, (void))                           \
  X(LLVMDisposePassBuilderOptions, void, (LLVMPassBuilderOptionsRef))                          \
  X(LLVMRunPasses, LLVMErrorRef,                                                               \
    (LLVMModuleRef, const char *, LLVMTargetMachineRef, LLVMPassBuilderOptionsRef))            \
  X(LLVMParseCommandLineOptions, void, (int, const char *const *, const char *))              \
  X(LLVMOrcCreateLLJIT, LLVMErrorRef, (LLVMOrcLLJITRef *, LLVMOrcLLJITBuilderRef))             \
  X(LLVMOrcCreateLLJITBuilder, LLVMOrcLLJITBuilderRef, (void))                                 \
  X(LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine, LLVMOrcJITTargetMachineBuilderRef,  \
    (LLVMTargetMachineRef))                                                                    \
  X(LLVMOrcLLJITBuilderSetJITTargetMachineBuilder, void,                                       \
    (LLVMOrcLLJITBuilderRef, LLVMOrcJITTargetMachineBuilderRef))                               \
  X(LLVMOrcLLJITGetTripleString, const char *, (LLVMOrcLLJITRef))                              \
  X(LLVMOrcLLJITGetMainJITDylib, LLVMOrcJITDylibRef, (LLVMOrcLLJITRef))                        \
  X(LLVMOrcLLJITAddLLVMIRModule, LLVMErrorRef,                                                 \
    (LLVMOrcLLJITRef, LLVMOrcJITDylibRef, LLVMOrcThreadSafeModuleRef))                         \
  X(LLVMOrcLLJITLookup, LLVMErrorRef, (LLVMOrcLLJITRef, LLVMOrcExecutorAddress *, const char *)) \
  X(LLVMOrcLLJITMangleAndIntern, LLVMOrcSymbolStringPoolEntryRef, (LLVMOrcLLJITRef, const char *)) \
  X(LLVMOrcJITDylibDefine, LLVMErrorRef, (LLVMOrcJITDylibRef, LLVMOrcMaterializationUnitRef))  \
  X(LLVMOrcAbsoluteSymbols, LLVMOrcMaterializationUnitRef, (LLVMOrcCSymbolMapPair *, size_t))  \
  X(LLVMOrcDisposeThreadSafeContext, void, (LLVMOrcThreadSafeContextRef))                      \
  X(LLVMOrcCreateNewThreadSafeModule, LLVMOrcThreadSafeModuleRef,                              \
    (LLVMModuleRef, LLVMOrcThreadSafeContextRef))                                              \
  X(LLVMOrcDisposeThreadSafeModule, void, (LLVMOrcThreadSafeModuleRef))                        \
  X(TARGET_FN(TargetInfo), void, (void))                                            \
  X(TARGET_FN(Target), void, (void))                                                \
  X(TARGET_FN(TargetMC), void, (void))                                              \
  X(TARGET_FN(AsmPrinter), void, (void))                                            \
  X(TARGET_FN(AsmParser), void, (void))

// The indirection through DECLARE_ and ENTRY_ expands TARGET_FN(...) before
// the name is stringified.
#define DECLARE(name, ret, args) static ret (*name) args;
#define DECLARE_(name, ret, args) DECLARE(name, ret, args)
LLVM_FUNCTIONS(DECLARE_)

// Looked up on their own, since no one version has all three: with
// LLVMOrcCreateNewThreadSafeContextFromLLVMContext (21 and later) a
// thread-safe context is made from a context of our own; before it, the
// pair LLVMOrcCreateNewThreadSafeContext and LLVMOrcThreadSafeContextGetContext
// (deprecated in 21, gone by 23) makes the context for us. A library needs
// one or the other.
static LLVMOrcThreadSafeContextRef (*CreateNewThreadSafeContextFromLLVMContext)(LLVMContextRef) = NULL;
static LLVMOrcThreadSafeContextRef (*CreateNewThreadSafeContext)(void) = NULL;
static LLVMContextRef (*ThreadSafeContextGetContext)(LLVMOrcThreadSafeContextRef) = NULL;

#define ENTRY(name, ret, args) {#name, (void **)&name},
#define ENTRY_(name, ret, args) ENTRY(name, ret, args)
static struct {
  const char *name;
  void **slot;
} symbols[] = {LLVM_FUNCTIONS(ENTRY_)};

// ---- Loading

static void *llvm_lib = NULL;
static char llvm_path[1024] = "";
static char llvm_version[64] = "";
static char last_error[4096] = "";

// What loading tried, one line per location: "path: outcome".
static char attempts[8192] = "";

static void note(const char *path, const char *outcome) {
  size_t n = strlen(attempts);
  snprintf(attempts + n, sizeof attempts - n, "%s: %s\n", path, outcome);
}

const char *unison_jit_last_error(void) { return last_error; }
const char *unison_jit_load_attempts(void) { return attempts; }
const char *unison_jit_llvm_path(void) { return llvm_path; }
const char *unison_jit_llvm_version(void) { return llvm_version; }

#define MIN_MAJOR 20

// Tries one library. Returns 1 if it is now loaded and usable.
static int try_lib(const char *path) {
  if (!ARCH_SUPPORTED) {
    note(path, "unsupported CPU architecture (the JIT runs on arm64 and x86_64)");
    return 0;
  }
  void *lib = lib_open(path);
  if (!lib) {
    // the loader's text for a missing file is long; say less for that case
    struct stat st;
    int absolute = path[0] == '/' || (isalpha((unsigned char)path[0]) && path[1] == ':');
    if (absolute && stat(path, &st) != 0) note(path, "not found");
    else if (!absolute) note(path, "not found on the dynamic loader's search path");
    else note(path, lib_error());
    return 0;
  }
  void (*get_version)(unsigned *, unsigned *, unsigned *) = (void (*)(unsigned *, unsigned *, unsigned *))lib_sym(lib, "LLVMGetVersion");
  unsigned major = 0, minor = 0, patch = 0;
  if (get_version) get_version(&major, &minor, &patch);
  char v[64];
  snprintf(v, sizeof v, "%u.%u.%u", major, minor, patch);
  if (!get_version || major < MIN_MAJOR) {
    char why[128];
    if (get_version) snprintf(why, sizeof why, "LLVM %s, too old", v);
    else snprintf(why, sizeof why, "not an LLVM library (or one older than 16)");
    note(path, why);
    lib_close(lib);
    return 0;
  }
  for (size_t i = 0; i < sizeof symbols / sizeof symbols[0]; i++) {
    void *p = lib_sym(lib, symbols[i].name);
    if (!p) {
      char why[256];
      snprintf(why, sizeof why, "LLVM %s, but it has no %s", v, symbols[i].name);
      note(path, why);
      lib_close(lib);
      return 0;
    }
    *symbols[i].slot = p;
  }
  CreateNewThreadSafeContextFromLLVMContext =
      (LLVMOrcThreadSafeContextRef (*)(LLVMContextRef))lib_sym(lib, "LLVMOrcCreateNewThreadSafeContextFromLLVMContext");
  CreateNewThreadSafeContext = (LLVMOrcThreadSafeContextRef (*)(void))lib_sym(lib, "LLVMOrcCreateNewThreadSafeContext");
  ThreadSafeContextGetContext =
      (LLVMContextRef (*)(LLVMOrcThreadSafeContextRef))lib_sym(lib, "LLVMOrcThreadSafeContextGetContext");
  if (!CreateNewThreadSafeContextFromLLVMContext && !(CreateNewThreadSafeContext && ThreadSafeContextGetContext)) {
    char why[256];
    snprintf(why, sizeof why, "LLVM %s, but it has neither LLVMOrcCreateNewThreadSafeContextFromLLVMContext nor the older pair", v);
    note(path, why);
    lib_close(lib);
    return 0;
  }
  llvm_lib = lib;  // never dlclosed: generated code lives in it
  snprintf(llvm_path, sizeof llvm_path, "%s", path);
  snprintf(llvm_version, sizeof llvm_version, "%s", v);
  note(path, "loaded");
  return 1;
}

#if defined(__APPLE__)
#define LIB "libLLVM.dylib"
#elif defined(_WIN32)
// the official Windows release ships the C API as LLVM-C.dll, not as a
// monolithic libLLVM
#define LIB "LLVM-C.dll"
#else
#define LIB "libLLVM.so"
#endif

#if !defined(_WIN32)
// Tries <dir>/<entry>/lib/libLLVM.* for every entry of dir whose name starts
// with prefix, newest version first (names sort as text: llvm@9 would come
// after llvm@23, but there is no such version to worry about).
static int try_versioned_dir(const char *dir, const char *prefix) {
  DIR *d = opendir(dir);
  if (!d) return 0;
  char names[64][64];
  int n = 0;
  struct dirent *e;
  while ((e = readdir(d)) && n < 64)
    if (strncmp(e->d_name, prefix, strlen(prefix)) == 0 && strlen(e->d_name) < 64)
      snprintf(names[n++], 64, "%s", e->d_name);
  closedir(d);
  // insertion sort, descending by (name length, then text): llvm@23 before llvm@9
  for (int i = 1; i < n; i++)
    for (int j = i; j > 0; j--) {
      size_t la = strlen(names[j - 1]), lb = strlen(names[j]);
      if (la < lb || (la == lb && strcmp(names[j - 1], names[j]) < 0)) {
        char t[64];
        memcpy(t, names[j - 1], 64);
        memcpy(names[j - 1], names[j], 64);
        memcpy(names[j], t, 64);
      } else break;
    }
  for (int i = 0; i < n; i++) {
    char path[1024];
    snprintf(path, sizeof path, "%.512s/%.64s/lib/" LIB, dir, names[i]);  // bounded, for gcc's truncation check
    if (try_lib(path)) return 1;
  }
  return 0;
}
#endif

// Finds and loads LLVM. Returns 0 on success. On failure, last_error says
// why and unison_jit_load_attempts lists every location tried.
int unison_jit_load(void) {
  if (llvm_lib) return 0;
  attempts[0] = 0;
  const char *override = getenv("UNISON_LLVM_LIB");
  if (override && override[0]) {
    if (try_lib(override)) return 0;
    snprintf(last_error, sizeof last_error, "UNISON_LLVM_LIB=%s is not a usable LLVM library", override);
    return -1;
  }
  if (try_lib(LIB)) return 0;
#if defined(_WIN32)
  // the LLVM installer's default location (choco's llvm package uses it too)
  if (try_lib("C:\\Program Files\\LLVM\\bin\\" LIB)) return 0;
  if (try_lib("C:\\Program Files (x86)\\LLVM\\bin\\" LIB)) return 0;
#elif defined(__APPLE__)
  // Homebrew: /opt/homebrew (Apple silicon) or /usr/local (Intel); `llvm` is
  // the current version, `llvm@N` the others. Then MacPorts.
  const char *brews[] = {"/opt/homebrew/opt", "/usr/local/opt", NULL};
  for (int i = 0; brews[i]; i++) {
    char path[1024];
    snprintf(path, sizeof path, "%s/llvm/lib/" LIB, brews[i]);
    if (try_lib(path)) return 0;
    if (try_versioned_dir(brews[i], "llvm@")) return 0;
  }
  if (try_versioned_dir("/opt/local/libexec", "llvm-")) return 0;
#else
  // apt.llvm.org and Debian/Ubuntu: /usr/lib/llvm-N/lib. Then the usual
  // system directories, where distributions name the file after the version.
  if (try_versioned_dir("/usr/lib", "llvm-")) return 0;
  const char *dirs[] = {"/usr/lib64", "/usr/lib", "/usr/local/lib", "/usr/lib/x86_64-linux-gnu", "/usr/lib/aarch64-linux-gnu", NULL};
  for (int i = 0; dirs[i]; i++) {
    DIR *d = opendir(dirs[i]);
    if (!d) continue;
    struct dirent *e;
    int found = 0;
    while (!found && (e = readdir(d)))
      if (strncmp(e->d_name, "libLLVM", 7) == 0 && strstr(e->d_name, ".so")) {
        char path[1024];
        snprintf(path, sizeof path, "%s/%s", dirs[i], e->d_name);
        found = try_lib(path);
      }
    closedir(d);
    if (found) return 0;
  }
#endif
  snprintf(last_error, sizeof last_error, "no LLVM %d or newer found", MIN_MAJOR);
  return -1;
}

// ---- The JIT

static LLVMOrcLLJITRef jit = NULL;
static LLVMTargetMachineRef machine = NULL;

static int fail(const char *where, LLVMErrorRef e) {
  char *msg = LLVMGetErrorMessage(e);
  snprintf(last_error, sizeof last_error, "%s: %s", where, msg);
  LLVMDisposeErrorMessage(msg);
  return -1;
}

const char *unison_jit_triple(void) { return jit ? LLVMOrcLLJITGetTripleString(jit) : "none"; }

// Process exit while the compile thread is inside LLVM. The RTS's hs_exit
// doesn't wait for a thread in a safe foreign call, and exit() then runs
// LLVM's static destructors under the compile in progress, which crashed in
// the code generator (2026-10-04, seen at the end of transcript runs: the
// re-entry batches flush after a quiet period, which is just when a program
// finishes). So: an atexit handler, registered after LLVM is loaded so that
// it runs before LLVM's own destructors, waits for a compile in flight to
// finish, and a compile that finishes (or starts) once we are exiting parks
// its thread instead of returning into a runtime that is gone. The process's
// exit then takes the parked thread with it.
static int compiling = 0;  // compiles in flight
static int exiting = 0;

static void park_forever(void) {
#if defined(_WIN32)
  for (;;) Sleep(INFINITE);
#else
  for (;;) pause();
#endif
}

static void at_exit(void) {
  __atomic_store_n(&exiting, 1, __ATOMIC_SEQ_CST);
  struct timespec ms = {0, 1000 * 1000};
  for (int i = 0; i < 10000 && __atomic_load_n(&compiling, __ATOMIC_SEQ_CST) > 0; i++) nanosleep(&ms, NULL);
}

// Loads LLVM if needed and starts the JIT. Returns 0 on success.
int unison_jit_init(void) {
  if (jit) return 0;
  if (unison_jit_load() != 0) return -1;
  atexit(at_exit);
  TARGET_FN(TargetInfo)();
  TARGET_FN(Target)();
  TARGET_FN(TargetMC)();
  TARGET_FN(AsmPrinter)();
  TARGET_FN(AsmParser)();

  // The backend's two instruction schedulers (before and after register
  // allocation) were 73% of code generation time on the suite's largest
  // module (2026-10-09): our functions are long straight-line blocks of
  // stack slot traffic, and the schedulers' dependency graphs grow
  // superlinearly with block length. On an out-of-order core the hardware
  // reorders at run time anyway, so they are off; UNISON_JIT_SCHED=1 keeps
  // them, for comparison. Must come before any target machine is created.
  // UNISON_JIT_INLINE_THRESHOLD=N sets the inliner's threshold (LLVM's
  // default is 225), for measurement.
  {
    const char *argv[4] = {"unison"};
    int argc = 1;
    if (!debug_getenv("UNISON_JIT_SCHED")) {
      argv[argc++] = "-enable-misched=false";
      argv[argc++] = "-enable-post-misched=false";
    }
    static char inl[64];
    const char *t = debug_getenv("UNISON_JIT_INLINE_THRESHOLD");
    if (t && *t) {
      snprintf(inl, sizeof inl, "-inline-threshold=%d", atoi(t));
      argv[argc++] = inl;
    }
    if (argc > 1) LLVMParseCommandLineOptions(argc, argv, NULL);
    // UNISON_JIT_LLVM_OPTS: further LLVM command line options, space
    // separated (-time-passes, -debug-pass-manager, ...), for measurement.
    const char *extra = debug_getenv("UNISON_JIT_LLVM_OPTS");
    if (extra && *extra) {
      static char buf[1024];
      snprintf(buf, sizeof buf, "%s", extra);
      const char *xargv[64] = {"unison"};
      int xargc = 1;
      for (char *w = strtok(buf, " "); w && xargc < 64; w = strtok(NULL, " ")) xargv[xargc++] = w;
      LLVMParseCommandLineOptions(xargc, xargv, NULL);
    }
  }

  // A target machine of our own, used only to run the optimization passes,
  // and with UNISON_JIT_CODEGEN_LEVEL=0..3 (for measurement) a second one
  // at that level for the JIT itself, which otherwise builds its own at
  // the default level.
  char *triple = LLVMGetDefaultTargetTriple();
  char *cpu = LLVMGetHostCPUName();
  char *features = LLVMGetHostCPUFeatures();
  LLVMTargetRef target;
  char *msg = NULL;
  if (LLVMGetTargetFromTriple(triple, &target, &msg)) {
    snprintf(last_error, sizeof last_error, "target: %s", msg);
    LLVMDisposeMessage(msg);
    return -1;
  }
  machine = LLVMCreateTargetMachine(target, triple, cpu, features,
                                    LLVMCodeGenLevelDefault, LLVMRelocDefault,
                                    LLVMCodeModelJITDefault);
  LLVMOrcLLJITBuilderRef builder = NULL;
  const char *level = debug_getenv("UNISON_JIT_CODEGEN_LEVEL");
  if (level && level[0] >= '0' && level[0] <= '3') {
    LLVMTargetMachineRef jtm = LLVMCreateTargetMachine(target, triple, cpu, features,
                                                       level[0] - '0', LLVMRelocDefault,
                                                       LLVMCodeModelJITDefault);
    builder = LLVMOrcCreateLLJITBuilder();
    LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(
        builder, LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine(jtm));
  }
  LLVMDisposeMessage(triple);
  LLVMDisposeMessage(cpu);
  LLVMDisposeMessage(features);

  LLVMErrorRef e = LLVMOrcCreateLLJIT(&jit, builder);
  if (e) return fail("create LLJIT", e);
  return 0;
}

// Parses a module from IR text, optimizes it, and hands it to the JIT.
// `passes` is a pass pipeline such as "default<O2>", or "" for none.
// If out_opt is non-null, it receives the module's text after the passes
// ran (malloc'd with LLVM's allocator; free it with unison_jit_free_string).
static int add_module(const char *ir, size_t len, const char *passes, char **out_opt);

int unison_jit_add_module(const char *ir, size_t len, const char *passes, char **out_opt) {
  if (__atomic_load_n(&exiting, __ATOMIC_SEQ_CST)) park_forever();
  __atomic_add_fetch(&compiling, 1, __ATOMIC_SEQ_CST);
  int r = add_module(ir, len, passes, out_opt);
  __atomic_sub_fetch(&compiling, 1, __ATOMIC_SEQ_CST);
  if (__atomic_load_n(&exiting, __ATOMIC_SEQ_CST)) park_forever();
  return r;
}

static int add_module(const char *ir, size_t len, const char *passes, char **out_opt) {
  LLVMContextRef ctx;
  LLVMOrcThreadSafeContextRef tsctx;
  if (CreateNewThreadSafeContextFromLLVMContext) {
    ctx = LLVMContextCreate();
    tsctx = CreateNewThreadSafeContextFromLLVMContext(ctx);  // takes ownership of ctx
  } else {
    tsctx = CreateNewThreadSafeContext();
    ctx = ThreadSafeContextGetContext(tsctx);
  }

  LLVMMemoryBufferRef buf =
      LLVMCreateMemoryBufferWithMemoryRangeCopy(ir, len, "unison-jit");
  LLVMModuleRef mod;
  char *msg = NULL;
  if (LLVMParseIRInContext(ctx, buf, &mod, &msg)) {
    snprintf(last_error, sizeof last_error, "parse: %s", msg);
    LLVMDisposeMessage(msg);
    LLVMOrcDisposeThreadSafeContext(tsctx);
    return -1;
  }

  if (passes && passes[0]) {
    LLVMPassBuilderOptionsRef opts = LLVMCreatePassBuilderOptions();
    LLVMErrorRef e = LLVMRunPasses(mod, passes, machine, opts);
    LLVMDisposePassBuilderOptions(opts);
    if (e) {
      LLVMDisposeModule(mod);
      LLVMOrcDisposeThreadSafeContext(tsctx);
      return fail("passes", e);
    }
  }
  if (out_opt) *out_opt = LLVMPrintModuleToString(mod);

  LLVMOrcThreadSafeModuleRef tsm = LLVMOrcCreateNewThreadSafeModule(mod, tsctx);
  LLVMOrcDisposeThreadSafeContext(tsctx);
  LLVMErrorRef e = LLVMOrcLLJITAddLLVMIRModule(
      jit, LLVMOrcLLJITGetMainJITDylib(jit), tsm);
  if (e) {
    LLVMOrcDisposeThreadSafeModule(tsm);
    return fail("add module", e);
  }
  return 0;
}

void unison_jit_free_string(char *s) { LLVMDisposeMessage(s); }

// Returns the address of a compiled symbol, or 0 on failure.
// Code is generated the first time a symbol from its module is looked up.
uint64_t unison_jit_lookup(const char *name) {
  LLVMOrcExecutorAddress addr = 0;
  LLVMErrorRef e = LLVMOrcLLJITLookup(jit, &addr, name);
  if (e) {
    fail("lookup", e);
    return 0;
  }
  return addr;
}

// Makes `name` in generated code refer to `addr`. Used for runtime helpers.
int unison_jit_define_symbol(const char *name, uint64_t addr) {
  LLVMJITSymbolFlags flags = {
      LLVMJITSymbolGenericFlagsExported | LLVMJITSymbolGenericFlagsCallable, 0};
  LLVMOrcCSymbolMapPair pair = {LLVMOrcLLJITMangleAndIntern(jit, name),
                                {addr, flags}};
  LLVMErrorRef e = LLVMOrcJITDylibDefine(LLVMOrcLLJITGetMainJITDylib(jit),
                                         LLVMOrcAbsoluteSymbols(&pair, 1));
  if (e) return fail("define symbol", e);
  return 0;
}
