# 0.1.3.1

- Fix linking issue `symbol not found in flat namespace '_kSecRandomDefault'`
  when using splitmix in TH on macOS.

# 0.1.3

- Use system specific entropy/randomess sources to initialise the default generator.
  Specifically `SecRandomCopyBytes` on Apple platforms and
  `RtlGenRandom` on Windows.

# 0.1.2

- Use `getentropy` for initialisation on unix-like systems (i.e. not Windows).

# 0.1.1

- Drop support for GHCs prior 8.6.5
- Support GHC-9.12

# 0.1.0.4

- Add TestU01 test-suite

# 0.1.0.3

- Fix oops bugs in 0.1.0.2

  - It's lowercase `windows.h`.
    I blame Microsoft docs for using capital case `Windows.h` in the docs.
    https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getprocessid

  - accidental `shiftL` vs `shiftR` mixup for 32-bit generator initialization.
    Doesn't affect Linux.

# 0.1.0.2

- Drop `time` dependency in favour of handcoded initialization
  - On Unix platforms we use `/dev/urandom` if it exists,
    otherwise use `gettimeofday`, `clock` and `getpid`.
  - On Windows we use `GetCurrentProcessID`, `GetCurrentThreadId()`,
    `GetTickCount`, `GetSystemTime` and `QueryPerformanceCounter`.
  - On GHCJS use `Math.random()`
  - Using `time` is a fallback option (e.g. for Hugs).

# 0.1.0.1

- Add `INLINEABLE` pragmas to `bitmaskWithRejection*` functions
- Support GHC-9.0

# 0.1

- Drop `random` dependency unconditionally.
  https://github.com/phadej/splitmix/issues/34

# 0.0.5

- Add `nextInteger`
- Use smaller range in `bitmaskWithRejection32` and `64`,
  when upper bound is 2^n - 1.
  This changes generated values when they were on the boundary.

# 0.0.4

- Add `bitmaskWithRejection32'` and `bitmaskWithRejection64'`
  which generate numbers in closed range `[0, n]`.
  Unticked variants generate in closed-open range `[0, n)`.

# 0.0.3

- Add `System.Random.SplitMix32` module
- Add `bitmaskWithRejection32` and `bitmaskWithRejection64` functions
- Add `nextWord32`, `nextTwoWord32` and `nextFloat`
- Add `random` flag, dropping dependency on `random`
  (breaks things, e.g. `QuickCheck`, when disabled).

# 0.0.2

- Support back to GHC-7.0
- Add `Read SMGen` instance

# 0.0.1

- Add `NFData SMGen` instance
- Fix a bug. http://www.pcg-random.org/posts/bugs-in-splitmix.html
  The generated numbers will be different for the same seeds!
