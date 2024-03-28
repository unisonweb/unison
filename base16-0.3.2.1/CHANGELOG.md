# Revision history for base16

## 0.3.2.1

* Add support for GHC 9.4
* Added `decodeBase16'` to `Data.ByteString.Base16*` (thanks to @shlevy [#20](https://github.com/emilypi/Base16/pull/20))
## 0.3.2.0

* Fix incorrect behavior in `Data.ByteString.Short.Base16#encodeBase16`
* Drop base GHC <8.10

## 0.3.0.1

* Allow for mixed-case lenient decoding + validation
* Bump testing for mixed-case hex

## 0.3.0.0

* API for `decodeBase16With` has changed to require `ByteString` instead of `Text`. This is in alignment with work done on `base64`, which reflects
  the same API. This will be the final design for these types of conversions.
* Test coverage now at 94%
* Add NFData, Exception, and Generic instances for Base16Error + @since annotations for new instances. ([#5](https://github.com/emilypi/Base16/pull/5))
* Doc improvements and add -XTrustworty and -XSafe annotations where needed. ([#5](https://github.com/emilypi/Base16/pull/5))
* Optimized inner loop for short text and bytestrings ([#4](https://github.com/emilypi/Base16/pull/4))
* Changed `encodeBase16` in `ByteString.Short` to produce `ShortText`, instead of `Text`.

## 0.2.1

* Added support for `Text.Short` and `ByteString.Short` values

## 0.2.0.1

* Improved performance. Decode and encode are now 3.5x-5x the next best lib.

## 0.2.0

* Add lenient decoders
* Fix bug in `Text` `decodeBase16` which failed on invalid UTF-8 values as a result of decoding
* Add `decodeBase16With` combinators

## 0.1.3

* Add lazy variants for `Text` and `ByteString` values

## 0.1.2.1 -- 2020-02-17

* Documentation now references correct RFC section

## 0.1.2 -- 2020-02-17

* Unmask loops - now correct.

## 0.1.1 -- 2020-02-17

* Mask `Word32` and `Word64` loops (flaky)

## 0.1.0.0 -- 2020-02-16

* First version. Released on an unsuspecting world.
