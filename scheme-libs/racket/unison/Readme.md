# Racket unison!

To load these libraries into a racket runtime, racket should be invoked like this:
```bash
$ racket -S scheme-libs/racket
Welcome to Racket v8.7 [cs].
> (require unison/core)
> ; now you can try out the definitions in core.ss!
```

## crypto
NOTE: you must have the `crypto` racket library installed, which can be optained via `raco pkg install crypto-lib`.

```clj
$ racket -S scheme-libs/racket
> (require unison/crypto)
> (unison-FOp-crypto.hashBytes (unison-FOp-crypto.HashAlgorithm.Sha1) #"")
#"\3329\243\356^kK\r2U\277\357\225`\30\220\257\330\a\t"
> (require openssl/sha1)
> (bytes->hex-string (unison-FOp-crypto.hashBytes (unison-FOp-crypto.HashAlgorithm.Sha1) #""))
"da39a3ee5e6b4b0d3255bfef95601890afd80709"
```