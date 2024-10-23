cryptonite
==========

[![Join the chat at https://gitter.im/vincenthz/cryptonite](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/vincenthz/cryptonite?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/haskell-crypto/cryptonite.png?branch=master)](https://travis-ci.org/haskell-crypto/cryptonite)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Cryptonite is a haskell repository of cryptographic primitives. Each crypto
algorithm has specificities that are hard to wrap in common APIs and types,
so instead of trying to provide a common ground for algorithms, this package
provides a non-consistent low-level API.

If you have no idea what you're doing, please do not use this directly.
Instead, rely on higher level protocols or implementations.

Documentation: [cryptonite on hackage](http://hackage.haskell.org/package/cryptonite)

Stability
---------

Cryptonite APIs are stable, and we only strive to add, not change or remove.
Note that because the API exposed is wide and also expose internals things (for
power users and flexibility), certains APIs can be revised in extreme cases
where we can't just add.

Versioning
----------

Next version of `0.x` is `0.(x+1)`. There's no exceptions, or API related meaning
behind the numbers.

Each versions of stackage (going back 3 stable LTS) has a cryptonite version
that we maintain with security fixes when necessary and are versioned with the
following `0.x.y` scheme.

Coding Style
------------

The coding style of this project mostly follows:
[haskell-style](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

Support
-------

See [Haskell packages guidelines](https://github.com/vincenthz/haskell-pkg-guidelines/blob/master/README.md#support)

Known Building Issues
---------------------

On OSX <= 10.7, the system compiler doesn't understand the '-maes' option, and
with the lack of autodetection feature builtin in .cabal file, it is left on
the user to disable the aesni. See the [Disabling AESNI] section

On CentOS 7 the default C compiler includes intrinsic header files incompatible
with per-function target options.  Solutions are to use GCC >= 4.9 or disable
flag *use_target_attributes* (see flag configuration examples below).

Disabling AESNI
---------------

It may be useful to disable AESNI for building, testing or runtime purposes.
This is achieved with the *support_aesni* flag.

As part of configure of cryptonite:

```
  cabal configure --flag='-support_aesni'
```

or as part of an installation:

```
  cabal install --constraint="cryptonite -support_aesni"
```

For help with cabal flags, see: [stackoverflow : is there a way to define flags for cabal](http://stackoverflow.com/questions/23523869/is-there-any-way-to-define-flags-for-cabal-dependencies)

Enabling PCLMULDQ
-----------------

When the C toolchain supports it, enabling flag *support_pclmuldq* can bring
additional security and performance for AES GCM.  A CPU with the necessary
instruction set will use an alternate implementation selected at runtime.

Links
-----

* [ChaCha](http://cr.yp.to/chacha.html)
* [ChaCha-test-vectors](https://github.com/secworks/chacha_testvectors.git)
* [Poly1305](http://cr.yp.to/mac.html)
* [Poly1305-test-vectors](http://tools.ietf.org/html/draft-nir-cfrg-chacha20-poly1305-06#page-12)
* [Salsa](http://cr.yp.to/snuffle.html)
* [Salsa128-test-vectors](https://github.com/alexwebr/salsa20/blob/master/test_vectors.128)
* [Salsa256-test-vectors](https://github.com/alexwebr/salsa20/blob/master/test_vectors.256)
* [XSalsa](https://cr.yp.to/snuffle/xsalsa-20081128.pdf)
* [PBKDF2](http://tools.ietf.org/html/rfc2898)
* [PBKDF2-test-vectors](http://www.ietf.org/rfc/rfc6070.txt)
* [Scrypt](http://www.tarsnap.com/scrypt.html)
* [Curve25519](http://cr.yp.to/ecdh.html)
* [Ed25519](http://ed25519.cr.yp.to/papers.html)
* [Ed448-Goldilocks](http://ed448goldilocks.sourceforge.net/)
* [EdDSA-test-vectors](http://www.ietf.org/rfc/rfc8032.txt)
* [AFIS](http://clemens.endorphin.org/cryptography)

