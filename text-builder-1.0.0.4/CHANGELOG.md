# 1

Major performance optimization achieved by getting rid of the "length" function. Since this was an unavoidable breaking change, other breaking changes were added as well.

Major redesign done to declutter the API from experimental features, dropping some and isolating others to "text-builder-dev" to reduce the need for further major releases of this library down the road. Order brought to naming conventions of the formatter-functions.

## Breaking changes and migration instructions

- `Text.Builder` module renamed to `TextBuilder` to fully conform to the [convention](https://www.reddit.com/r/haskell/comments/1qrilm/packages_and_namespaces_naming_convention/) of having the root namespace of the library map to its name.
- `Builder` type was renamed to `TextBuilder`.
- `run` renamed to `toText`.
- `length` removed to increase performance of the whole abstraction.
- `null` renamed to `isEmpty` to leave declarative naming only for constructor-functions.
- `putToStdOut`, `putToStdErr`, `putLnToStdOut`, `putLnToStdErr` removed to reduce the bloat of the API. Just use similar functions on `Text`.
- `padFromLeft` and `padFromRight` moved to "text-builder-dev-0.4".
- `asciiByteString` renamed to `unsafeUtf8ByteString`. ASCII is a subset of UTF-8 and the previous implementation was unsafe any way.
- `hexData` moved to "text-builder-dev-0.4" as `byteStringHexEncoding`.
- `unicodeCodePoint` renamed to `unicodeCodepoint`.
- `utf16CodeUnits1`, `utf16CodeUnits2`, `utf8CodeUnits1`, `utf8CodeUnits2`, `utf8CodeUnits3`, `utf8CodeUnits4` moved to "text-builder-core-0.1" and removed from the public API to reduce the bloat. Nobody seems to have been using them. If you need them, open a ticket in "text-builder-core".
- `unsignedDecimal` - removed because it was unsafe. Use `decimal`.
- `thousandSeparatedUnsignedDecimal` - removed because it was unsafe. Use `thousandSeparatedDecimal`.
- `dataSizeInBytesInDecimal` moved to "text-builder-dev-0.4" as `approximateDataSize`.
- `unsignedBinary` and `unsignedPaddedBinary` replaced with `binary`, which is now safe and handles negative values.
- `hexadecimal` and `unsignedHexadecimal` replaced with `hexadecimal`, which is now safe and handles negative values.
- `decimalDigit` and `hexadecimalDigit` removed. Seemed to not be useful. If you think otherwise open a ticket.
- `fixedDouble` moved to "text-builder-dev-0.4" as `doubleFixedPoint`. It does not have an efficient implementation yet is based on `printf`.
- `doublePercent` moved to "text-builder-dev-0.4" as `doubleFixedPointPercent`. Same reasons.
- `intervalInSeconds` moved to "text-builder-dev-0.4" as `diffTimeSeconds` and `picoseconds`.

## Non-breaking

- `unicodeCodepoint` is now safe. It replaces invalid codepoints with a default one.
- `char` is now safe for the same reasons.

# 0.6.8

- Migrated to the namespacing convention where the root namespace matches the package name 1-1 with no special cases. The support for previous naming convention is still provided though.
