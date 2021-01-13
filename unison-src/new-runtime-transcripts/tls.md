# Tests for TLS builtins 

## Setup

You can skip the section which is just needed to make the transcript self-contained.

```ucm:hide
.> builtins.merge
.> builtins.mergeio
.> cd builtin
```

```unison:hide
use .builtin.io2 Failure

filter: (a -> Boolean) -> [a] -> [a]
filter f all =
  go acc = cases
    a +: as -> if (f a) then go (cons a acc) as else go acc as
    [] -> acc
  go [] all

ability Exception e where raise : e ->{Exception e} a

toException : Either e a ->{Exception e} a
toException = cases
    Left e  -> raise e
    Right a -> a

Exception.toEither.handler : Request {Exception e} a -> Either e a
Exception.toEither.handler = cases
    { a }          -> Right a
    {raise e -> _} -> Left e

Exception.toEither : '{g, Exception e} a ->{g} Either e a
Exception.toEither a = handle !a with Exception.toEither.handler

isNone = cases
  Some _ -> false
  None -> true

ability Stream a where
   emit: a -> ()

Stream.toList.handler : Request {Stream a} r -> [a]
Stream.toList.handler =
  go : [a] -> Request {Stream a} r -> [a]
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { _ } -> acc

  go []

Stream.toList : '{Stream a} r -> [a]
Stream.toList s = handle !s with toList.handler

Stream.collect.handler : Request {Stream a} r -> ([a],r)
Stream.collect.handler =
  go : [a] -> Request {Stream a} r -> ([a],r)
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { r } -> (acc, r)

  go []

Stream.collect : '{e, Stream a} r -> {e} ([a],r)
Stream.collect s =
  handle !s with Stream.collect.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, returns the Results and the result of the test
evalTest: '{Stream Result, Exception Failure, io2.IO} a -> ([Result], Either Failure a)
evalTest a = handle
               (handle !a with Exception.toEither.handler)
             with Stream.collect.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, but ignore the produced value and only return the test Results
runTest: '{Stream Result, Exception Failure, io2.IO} a -> [Result]
runTest t = match evalTest t with
              (results, Right _) -> results
              (results, Left (Failure _ t)) -> results :+ (Fail t)


--
-- convenience functions for emitting test results
--
expect : (a -> Text) -> (a -> a -> Boolean) -> Text -> a -> a -> {Stream Result} ()
expect toText compare msg expected actual = let
  if (compare expected actual) then 
    emit (Ok msg) 
  else let
    failMsg = msg ++ "expected : " ++ (toText expected) ++ " actual: " ++ (toText actual)
    emit (Fail failMsg)

expectU : (a -> Text) -> Text -> a -> a -> {Stream Result} ()
expectU toText msg expected actual = expect toText (==) msg expected actual

check: Text -> Boolean -> {Stream Result} ()
check msg test = if test then emit (Ok msg) else emit (Fail msg)

-- generated with:
-- openssl req -newkey rsa:2048 -subj '/CN=test.unison.cloud/O=Unison/C=US' -nodes -keyout key.pem -x509 -days 365 -out cert.pem

self_signed_key_pem = "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQD3b8hiFGk+/F1K\nvXTkyJhB1IRlVB6rkUHVnDAmbWBl/3KvDnjMvU0Rtdf5SU5b3DRZAhKVCK76B/R1\n76qqq+EE1UXWfceMkoDQ3M2x3/FWcdceULDpt2TU3UmGc3o6Vc9JS8bvyzMjYuM+\nWw3i6rVt8OJz7yAd0tlrb+CHyE616CqYWtvMGfafPc1R0DJc0IbryN5hM0DdPDmU\ntrh5Hw2bJ+ePGCjO1f/99RdxibEzuIlOzSE4trC42MNQq/A1lVj07z74v6eimUoD\nSEglwEOgtsKqFFiikYFPvbUQK0x2whGLcgQAJfNdwvrPRigpSJoJ4+zFIElCuEeY\nPoB/vdSbAgMBAAECggEBALtGDYG+NRII64sf7ZMjB6n7cOFIU5ZC8HL+LatH6UgJ\nmQFJkMBVHjmjSf1RME9yUOR1Hx9RXS28S5ffd2qJgFuqF5uJwzhvwbNTCxPvjLeC\nnfBB3kRmG5jIi1eJmx7/x02QXtcrEl9b/uY5AD+YqQy23o4klEg+R3R7TpM2Vexy\neJ4IhQJEGkCMCwYLYlzOEraVWcVRk5xxIITrLuozPbcS1GoLB4oPncm8z+wEfvBR\n/1iO15puuZNgA9Z9QorVZ+RkEnMlF+lNKsDMLHInnes942tES23IfyAURxGH8evO\nL6Xf2gzQbzuauxmS2f8ZRy1yizoFmAaAph5UTQy9YqECgYEA/UjHDjXc8JKda9Nk\nFLhq6R7PHVsYq/YDF2vijSWiwVKal8Inw31GDShK0lum20Rx6uV0a3Nx5SNohx7J\nEwOoZDaMRcRLtf55XIZp/154AVAR9oACu8FyC8OAOlCCHLvCa6jg/fJ2ZzfP34gF\ngn9lZ1M+XXkyMQaXYDaMRw5tWykCgYEA+hb0VKCwYae5znmMVEQer1r05pfTQC3F\nd/en0NW61IDd7jMxeqtmhCSgZ1SE0QRhPcsOHwothMs4EOQKhDes4fK1uM8Xc619\n3pKKQWV6u+p/IOdBQZ7oFh3OcCNzSXRfstQmJfn6bJfa2y0WChk54diL0BNZnC6/\nXdLLQkNBLiMCgYAHbJexGfEU+Cc3HuYjGVjA5o9Jm/74I2jpydbFLmI76nDvm9hQ\niZ6BCjpPOjKE4Ne5++nSsHqZLdWFiGh6bzQDY/nunZdeiUwDY0k+HAXGfBNat1vd\nJwlxwNREYVG/B93mC/g0WWQRGHi2Y6VNsvBE/BKq6L+I4lmTj2XDO3AHKQKBgQD1\nTeys0c0QjK9RWqINyZ97izEqugNpMTw/NzlxOyN960VErww6ccMlnLOFhz6bsrsb\nHIAmbBpH5fmBf9zc4at/GU2sD93GBoeu+HgKO1rO1e2KvaqhjhfCEcx67BraEv8H\nbtZYa8F6IN14HNpAX4wJ/hjtqP09mBD3G5tpzIQzlwKBgCxZPZyK9HcdeTaHzoyU\n2WdTmOh1ZzOAMzKaahOYeGOOf4NB7SeNpjiFL1stXcxCqmiNTfWquU+46naRFUFa\nomdGtr1kSkDBqNC67qp8rbmXmwKrW/kJRr+ttl5qhyll/g850KFwsjiCR+htMoON\nBlI43oxBPLgtdV2K5YnN1oqL\n-----END PRIVATE KEY-----\n"

self_signed_cert_pem = "-----BEGIN CERTIFICATE-----\nMIIC6zCCAdOgAwIBAgIUPs4hT3b3/d9OqX9AKPNAYiQ3B/swDQYJKoZIhvcNAQEL\nBQAwGjEYMBYGA1UEAwwPYmFzaWwudmlyZW8ub3JnMB4XDTIwMDIwMzIwMjEyMloX\nDTMwMDEzMTIwMjEyMlowGjEYMBYGA1UEAwwPYmFzaWwudmlyZW8ub3JnMIIBIjAN\nBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwJ4CdM+SI18Deysh53bczKqzswDW\nq+5Yrr2ZlliESFijHnx3c32wNT9712F7sdh4X8oVc9PVIlcrak8ONPEAyErZtgf6\nCH4EjW9NGIyB0YrgdnX7qSMFJVXjQW7SXnhtyktY/yH75sM3qXWvgq9mnXyaiYQ9\nM4E6w2VPOQezE+SzV1Mr3NRQeR2OZ94EBR3Ii7/qlldpTkWSDxaimSNzRqJZfRZ8\n02xThtDfQzZRFaKgdsN23ooMik0bhpqCDH4sSTMF3IqHDPU8UZjz3nXNBuHSFjNy\nq87ALoatYjMTod8oa52RsO8tQ4WXq/6tGUCtWryLkcT+A2ostkysS20QMwIDAQAB\noykwJzAJBgNVHRMEAjAAMBoGA1UdEQQTMBGCD2Jhc2lsLnZpcmVvLm9yZzANBgkq\nhkiG9w0BAQsFAAOCAQEAKclHpw5qf5hmmvF4P+GHtn8yHhn3zR+DTG1MOyf0ZlPJ\n8JIw/ZRKMvFiDa7CeepBdIZFRAvorOeIqxFV6TY4c09PHxSuM3VlQzizzvvldjTN\ngwmcdaQn4+hq+NN6jqKQ71TCdMM85pgmkXl4XoeHDuve4U5QI/q2TqaOVWXOevgn\nAUTMZrLPSHaJ+pJGciAiyr8I6G97yCmhdGZiTYoLYFAsWzv9M4whilpJ3Q89oHcE\nwpFGAt6m0acKJPT0MrOE88W4rYehg0y/Uu3oIT52ioMxKEjetseKBbiNH8lyZEXc\nG0FaiSWNaYRwLvwBzidNke5G/Zpi4W/dkWvHD9tspQ==\n-----END CERTIFICATE-----\n"
```

```ucm:hide
.> add
```

# Using an alternative certificate store

First lets make sure we can load our cert

``` unison
test> match (decodeCert (toUtf8 self_signed_cert_pem) with 
  Left (Failure _ t) -> [Fail t]
  Right _ -> [Ok "succesfully decoded self_signed_pem"]
```

