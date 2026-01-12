# Argon2id Password Hashing

This transcript tests the Argon2id password hashing builtins.

``` ucm :hide
> builtins.merge
```

## Test crypto.argon2.hashRaw

``` unison
-- Test password and salt
password = Text.toUtf8 "my-secret-password"
salt = Text.toUtf8 "16bytesalt12345!"  -- 16 bytes

-- Hash with OWASP recommended settings (memory=47104 KiB, iterations=1, parallelism=1, output=32 bytes)
hash1 = crypto.argon2.hashRaw 47104 1 1 32 password salt

> hash1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + hash1    : Either Failure Bytes
  + password : Bytes
  + salt     : Bytes

  Run `update` to apply these changes to your codebase.

    8 | > hash1
          ⧩
          Right
            0xscf12689adbeb0f1a921e87a22393f1d6acf8cdf3d9ad6bbb99561f1918ef55c9
```

## Test crypto.argon2.verifyRaw

``` unison
-- Test verification with correct password
-- Parameters must match what was used to create the hash
password = Text.toUtf8 "my-secret-password"
salt = Text.toUtf8 "16bytesalt12345!"
expectedHash = 0xscf12689adbeb0f1a921e87a22393f1d6acf8cdf3d9ad6bbb99561f1918ef55c9

verifyCorrect = crypto.argon2.verifyRaw 47104 1 1 password salt expectedHash

-- Test verification with wrong password
verifyWrong = crypto.argon2.verifyRaw 47104 1 1 (Text.toUtf8 "wrong-password") salt expectedHash

> verifyCorrect
> verifyWrong
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + expectedHash  : Bytes
  + password      : Bytes
  + salt          : Bytes
  + verifyCorrect : Boolean
  + verifyWrong   : Boolean

  Run `update` to apply these changes to your codebase.

    12 | > verifyCorrect
           ⧩
           true

    13 | > verifyWrong
           ⧩
           false
```

## Test error handling

``` unison
-- Salt too short (must be at least 8 bytes)
shortSaltHash = crypto.argon2.hashRaw 47104 1 1 32 (Text.toUtf8 "password") (Text.toUtf8 "short")

> shortSaltHash
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + shortSaltHash : Either Failure Bytes

  Run `update` to apply these changes to your codebase.

    4 | > shortSaltHash
          ⧩
          Left
            (Failure
              (typeLink CryptoFailure)
              "argon2: salt too short (minimum 8 bytes)"
              (Any ()))
```
