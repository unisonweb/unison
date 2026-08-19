Change log for `crypton-socks`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.6.2

* Move library modules to directory `src` and example application to directory
  `example`.
* Change data types `SocksHello` and `SocksHelloResponse` (each with a single,
  unary data constructor without strictness annotation) to `newtype`.
* Add missing top-level signatures to library.
* Name the example application `crypton-socks-example`, and move it being built
  behind Cabal flag `example` (default: false).
* Export the `SocksFQDN` type synonym, representing fully-qualified domain names
  (FQDN) under the SOCKS Protocol Version 5. The API assumes that such values
  comprise only ASCII characters. Domain names that include other Unicode code
  points should be Punycode encoded.
* Remove dependency on the `basement` package.
* Deprecate `defaultSocksConfFromSockAddr`, soft deprecated from 22 April 2019.

## 0.6.1

* Rename `socks-0.6.1` package as `crypton-socks-0.6.1`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
