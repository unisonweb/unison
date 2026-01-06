# Argon2id Password Hashing

This transcript tests the Argon2id password hashing builtins.

``` ucm :hide
> builtins.merge
```

## Test argon2HashWith (with user-provided salt)

``` unison
-- Test password and salt
password = Text.toUtf8 "my-secret-password"
salt = Text.toUtf8 "16bytesalt12345!"  -- 16 bytes

-- Hash with OWASP recommended settings (memory=47104 KiB, iterations=1, parallelism=1, output=32 bytes)
hash1 = crypto.argon2HashWith 47104 1 1 32 password salt

> hash1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + hash1    : Either Failure Text
  + password : Bytes
  + salt     : Bytes

  Run `update` to apply these changes to your codebase.

    8 | > hash1
          ⧩
          Right
            "$argon2id$v=19$m=47104,t=1,p=1$MTZieXRlc2FsdDEyMzQ1IQ$zxJomtvrDxqSHoeiI5Px1qz4zfPZrWu7mVYfGRjvVck"
```

## Test argon2Verify (verification)

``` unison
-- Test verification with correct password (use the actual hash from above)
verifyCorrect = crypto.argon2Verify "$argon2id$v=19$m=47104,t=1,p=1$MTZieXRlc2FsdDEyMzQ1IQ$zxJomtvrDxqSHoeiI5Px1qz4zfPZrWu7mVYfGRjvVck" (Text.toUtf8 "my-secret-password")

-- Test verification with wrong password
verifyWrong = crypto.argon2Verify "$argon2id$v=19$m=47104,t=1,p=1$MTZieXRlc2FsdDEyMzQ1IQ$zxJomtvrDxqSHoeiI5Px1qz4zfPZrWu7mVYfGRjvVck" (Text.toUtf8 "wrong-password")

> verifyCorrect
> verifyWrong
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + verifyCorrect : Boolean
  + verifyWrong   : Boolean

  Run `update` to apply these changes to your codebase.

    7 | > verifyCorrect
          ⧩
          true

    8 | > verifyWrong
          ⧩
          false
```

## Test argon2HashAutoWith (auto-generated salt)

``` unison
-- Hash with auto-generated salt (uses IO to generate random salt)
hashAuto : '{IO, Exception} (Either Failure Text)
hashAuto = do
  password = Text.toUtf8 "another-password"
  crypto.argon2HashAutoWith 47104 1 1 32 password

-- Note: We can't show the exact output since salt is random,
-- but we can verify it produces a valid hash that can be verified
testAutoHash : '{IO, Exception} Boolean
testAutoHash = do
  password = Text.toUtf8 "test-password"
  match crypto.argon2HashAutoWith 47104 1 1 32 password with
    Left _ -> false
    Right hash -> crypto.argon2Verify hash password
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + hashAuto     : '{IO, Exception} Either Failure Text
  + testAutoHash : '{IO, Exception} Boolean

  Run `update` to apply these changes to your codebase.
```

## Test error handling

``` unison
-- Salt too short (must be at least 8 bytes)
shortSaltHash = crypto.argon2HashWith 47104 1 1 32 (Text.toUtf8 "password") (Text.toUtf8 "short")

> shortSaltHash
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + shortSaltHash : Either Failure Text

  Run `update` to apply these changes to your codebase.

    4 | > shortSaltHash
          ⧩
          Left
            (Failure
              (typeLink CryptoFailure)
              "argon2: salt too short (minimum 8 bytes)"
              (Any ()))
```
