## Overview

- [x] What does this change accomplish and why?
  - Adds Argon2id password hashing support as builtins, enabling secure password storage in Unison applications
  - Argon2id is the recommended password hashing algorithm [per OWASP guidelines](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html) and won the Password Hashing Competition
  - Migrates from `cryptonite` to `crypton` (its actively-maintained fork) across the codebase
  - Accompanying [@unison/base contribution](https://share.unison-lang.org/@unison/base/contributions/154) - not sure if the diff is stalling due to failures in depending on unreleased unison code or for other reasons

- [x] How does it change the user experience?
  - Users can now hash passwords securely and verify them against stored hashes
  - Two new builtins in the `crypto.argon2` namespace

  ```unison
  -- Hash with user-provided options and salt (raw bytes API)
  crypto.argon2.hashRaw : Nat -> Nat -> Nat -> Nat -> Bytes -> Bytes -> Either Failure Bytes
  -- Args: memory (KiB), iterations, parallelism, outputLen, password, salt
  -- Returns: raw hash bytes

  -- Verify password against raw hash
  crypto.argon2.verifyRaw : Nat -> Nat -> Nat -> Bytes -> Bytes -> Bytes -> Either Failure Boolean
  -- Args: memory (KiB), iterations, parallelism, password, salt, expectedHash
  -- Returns: Right True if match, Right False if mismatch, Left Failure on error
  ```

- [x] Before and after examples:

  **Hashing a password (with OWASP-recommended settings):**
  ```unison
  password = Text.toUtf8 "my-secret-password"
  salt = Text.toUtf8 "16bytesalt12345!"  -- 16 bytes minimum

  -- Hash with memory=47104 KiB, iterations=1, parallelism=1, output=32 bytes
  hash = crypto.argon2.hashRaw 47104 1 1 32 password salt
  -- Result: Right 0xscf12689adbeb0f1a921e87a22393f1d6acf8cdf3d9ad6bbb99561f1918ef55c9
  ```

  **Verifying a password:**
  ```unison
  expectedHash = 0xscf12689adbeb0f1a921e87a22393f1d6acf8cdf3d9ad6bbb99561f1918ef55c9

  isValid = crypto.argon2.verifyRaw 47104 1 1 password salt expectedHash
  -- Result: Right true

  isWrong = crypto.argon2.verifyRaw 47104 1 1 (Text.toUtf8 "wrong") salt expectedHash
  -- Result: Right false
  ```

## Implementation approach and notes

1. **Migrated from `cryptonite` to `crypton`** across the codebase
   - `crypton` is the actively-maintained fork of `cryptonite` (which is no longer maintained)
   - `crypton` provides Argon2 support via `Crypto.KDF.Argon2`
   - Updated all `package.yaml` files: `unison-core`, `unison-syntax`, `unison-runtime`, `unison-hashing-v2`, `unison-cli`

2. **Defined ForeignFunc enum variants** in `Type.hs`:
   - `Crypto_Argon2_HashRaw`
   - `Crypto_Argon2_VerifyRaw`

3. **Implemented foreign functions** in `Function.hs`:
   - `argon2HashRawWrapper` - Calls `Argon2.hash` with user options/salt, returns raw hash bytes
   - `argon2VerifyRawWrapper` - Re-hashes password with same parameters and does constant-time comparison
   - `argon2ErrMsg` - Maps all `CryptoError` codes to human-readable messages

4. **Added 6-tuple ForeignConvention support** - The hash function takes 6 arguments (4 options + password + salt), which required adding:
   - `pattern Tup5V`
   - `pattern Tup6C`
   - `decodeTup6` / `encodeTup6`
   - `ForeignConvention (a,b,c,d,e,f)` instance

## Interesting/controversial decisions

1. **Argon2id only** - Only exposed Argon2id variant (not Argon2i or Argon2d). Argon2id is the recommended hybrid approach that provides both side-channel resistance and GPU/ASIC resistance.

2. **Individual Nat arguments vs options struct** - Chose individual `Nat` arguments for the builtins rather than a struct. This keeps the builtins simple; wrapper functions in `@unison/base` can provide a nicer `Argon2Options` struct API.

3. **Raw bytes API** - The builtins work with raw bytes rather than PHC-encoded strings. This is the lower-level API; PHC encoding can be added in `@unison/base` wrapper functions if needed.

4. **Either Failure Boolean for verify** - `verifyRaw` returns `Either Failure Boolean` rather than just `Boolean`. This allows callers to distinguish between:
   - `Right True` - password matches
   - `Right False` - password doesn't match (not an error)
   - `Left Failure` - crypto error (e.g., invalid parameters)

   This is important because a crypto error (like invalid salt size) is a data integrity issue worth logging, whereas a wrong password is expected behavior.

5. **6-tuple support** - Added general 6-tuple `ForeignConvention` rather than using nested tuples, as it's cleaner and may benefit other future builtins.

6. **cryptonite to crypton migration** - Took the opportunity to migrate the entire codebase from the unmaintained `cryptonite` to its actively-maintained fork `crypton`, which has an identical API.

## Test coverage

- [x] Transcript test added: `unison-src/transcripts/idempotent/argon2.md`
  - Tests basic hashing with OWASP-recommended settings
  - Tests verification with correct password (returns `Right true`)
  - Tests verification with wrong password (returns `Right false`)
  - Tests error handling (salt too short returns `Left Failure` with descriptive message)

- [ ] Additional test coverage could include:
  - Haskell unit tests for edge cases (empty password, maximum sizes, etc.)
  - Property tests for hash/verify round-trip

## Loose ends

- **@unison/base wrappers** - Convenience functions with default options, an `Argon2Options` type, and PHC encoding can be added to `@unison/base` in a follow-up PR to that repo
- **Documentation** - Crypto documentation should be updated to include argon2

## Final checklist

- [x] **PR title:** Add Argon2id builtins; migrate cryptonite to crypton
- [x] **Transcripts included:** `unison-src/transcripts/idempotent/argon2.md`
- [x] **package.yaml files updated** (not .cabal files directly)
