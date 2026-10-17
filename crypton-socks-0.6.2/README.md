crypton-socks
=============

Originally forked from [socks-0.6.1](https://hackage.haskell.org/package/socks-0.6.1).

Haskell library implementing the SOCKS Protocol Version 5.

Usage
-----

See `example/Example.hs` for really simple and straighforward examples. The
main API of the library is three functions:

* `socksConnectWithSocket` which connects to a `SocksAddress` specifying a
  `SocksHostAddress` (`SocksAddrIPV4`, `SocksAddrDomainName` or
  `SocksAddrIPV6`). The name resolution is done on the client side.
* `socksConnect` connects a new socket to a SOCKS server, with
  `socksConnectWithSocket`.
* `socksConnectName` which connects to a fully qualified domain name (FQDN) (for
   example, `www.example.com`). The name resolution is done by the proxy server.

History
-------

The [`socks`](https://hackage.haskell.org/package/socks) package was originated
and then maintained by Vincent Hanquez. For published reasons, he does not
intend to develop the package further after version 0.6.1 but he also does not
want to introduce other maintainers.
