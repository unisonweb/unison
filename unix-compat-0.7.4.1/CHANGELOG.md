## Version 0.7.4.1 (2025-08-26)

- Fix build with `-Werror=unused-packages`.
- Tested with GHC 8.0 - 9.14 alpha1.

## Version 0.7.4 (2025-03-27)

- Add `wasm32-wasi` support
  ([PR #16](https://github.com/haskell-pkg-janitors/unix-compat/pull/16)).
- Tested with GHC 8.0 - 9.12.

## Version 0.7.3 (2024-10-11)

- Fix `sysmacros.h` include for GNU/Hurd
  ([PR #12](https://github.com/haskell-pkg-janitors/unix-compat/pull/12)).
- Tested with GHC 8.0 - 9.10.

## Version 0.7.2 (2024-06-25)

- Remove flag `old-time` and drop support for `old-time`.
- Remove support for GHC 7.
- Tested with GHC 8.0 - 9.10.

## Version 0.7.1 (2023-12-06) Santa Clause edition

- Add `System.PosixCompat.Process` module, exporting `getProcessID`.

## Version 0.7 (2023-03-15)

- Remove `System.PosixCompat.User` module.

## Version 0.6 (2022-05-22)

- Better support for symbolic links.
