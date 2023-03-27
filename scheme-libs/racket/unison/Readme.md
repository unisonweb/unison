# Racket unison!

To load these libraries into a racket runtime, racket should be invoked like this:
```bash
$ racket -S scheme-libs/racket
Welcome to Racket v8.7 [cs].
> (require unison/core)
> ; now you can try out the definitions in core.ss!
```

## crypto
NOTE: Our crypto functions require on both `libcrypto` (from openssl) and `libb2`. You may have to tell racket where to find `libb2`, by adding an entry to the hash table in your [`config.rktd` file](https://docs.racket-lang.org/raco/config-file.html). This is what I had, for an M1 mac w/ libb2 installed via Homebrew:
```
(lib-search-dirs . (#f "/opt/homebrew/Cellar/libb2/0.98.1/lib/"))
```

You can then run the tests with
```bash
$ raco test scheme-libs/racket/unison/crypto.rkt
```
On success, it has no output.

You'll also need to install `x509-lib` with `raco pkg install x509-lib`
