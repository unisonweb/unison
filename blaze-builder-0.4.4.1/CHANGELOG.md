* 0.4.4.1 2025-08-28
  - Drop unused dependency `deepseq`
  - Tested with GHC 8.0 - 9.14 alpha1

* 0.4.4 2025-07-31
  - Optimization:
    `Blaze.ByteString.Builder.Char.Utf8.fromText = Data.Text.Encoding.encodeUtf8Builder`
    rather than going through `String`.
    (Alex Biehl, PR #11 https://github.com/blaze-builder/blaze-builder/pull/11)
  - Tested with GHC 8.0 - 9.12.2

* 0.4.3 2025-05-15
  - Fix computation of max buffer overhead on 32 bit platforms
    (sternenseemann, PR #8 https://github.com/blaze-builder/blaze-builder/pull/8)
  - Drop support for GHC 7, bytestring < 0.10.4 and text < 1.1.2
  - Tested with GHC 8.0 - 9.12.2

* 0.4.2.3 2023-08-27
  - Fix compilation warnings concerning non-canonical mappend
  - Support bytestring-0.12
  - Support text-2.1
  - Tested with GHC 7.0.4 to 9.8.1 alpha3

* 0.4.2.2
  - Support GHC 9.2

* 0.4.2.1
  - Bump cabal file to Cabal >= 1.10

* 0.4.2.0
  - Make semigroup instances unconditional
  - Support bytestring-0.11
  - Support semigroups-0.19

* 0.4.1.0
  - Gain compatibility with the Semigroup/Monoid proposal
  - Add Word8 HTML escaping builders
  - Speed up `fromHtmlEscapedText` and `fromHtmlEscapedLazyText`

* 0.4.0.2
  - Fixed warnings on GHC 7.10,  courtesy of Mikhail Glushenkov.

* 0.4.0.1
  - Tightened the version constraints on the bytestring package for GHC 7.8

* 0.4.0.0
  - This is now a compatibility shim for the new bytestring builder.  Most
    of the old internal modules are gone.   See this blog post for more
    information:

    <http://blog.melding-monads.com/2015/02/12/announcing-blaze-builder-0-4/>

  - The 'Blaze.ByteString.Builder.Html.Utf8.fromHtmlEscaped*' functions now
    strip out any ASCII control characters present in their inputs.  See
    <https://github.com/lpsmith/blaze-builder/issues/1> for more
    information.

* 0.3.3.0
  - exposed the 'Buffer' constructor to enable keeping around a pool of
    buffers.

* 0.3.2.0
  - added 'writeToByteString' to construct a strict bytestring in a single
    step. We can actually view 'Write's as strict-bytestring builders.

* 0.3.1.1
  - Changed imports of Foreign.Unsafe to make it GHC 7.8 compatible
  - -Wall clean on GHC 7.0 - 7.6

* 0.3.1.0
  - Widened dependencies on text and bytestring

* 0.3.0.1

  - Fix build warning in Blaze.ByteString.Builder.Word
    (contributed by Greg Weber)

* 0.3.0.1

  - Remove comparison to the 'text' library encoding functions of
    'Blaze.Builder.Char.Utf8.fromText' and
    'Blaze.Builder.Char.Utf8.fromLazyText'. Bryan O'Sullivan reported that on
    his 64-bit system with GHC 7.0.3 the 'text' library is 5x faster than the
    'blaze-builder' library.

* 0.3.0.0

  - Renamings in internal modules: WriteIO -> Poke and associated functions.

* 0.2.1.4

  - Fixed bug: appending to 'chunkedTransferEncoding somebuilder' also encoded
    the appended builder, which is obviously wrong.

* 0.2.1.3

  - Fixed bug: 'chunkedTransferTerminator' is now correctly set to "0\r\n\r\n".

* 0.2.1.2

  - Add 'MonoPatBinds' language extension to all relevant files to solve the
    issues caused by GHC bug http://hackage.haskell.org/trac/ghc/ticket/4498

* 0.2.1.1

  - Reexport 'Write' datatype and 'fromWriteList', 'fromWriteSingleton',
    'fromWrite' functions together with writes and builders for storables.
  - Add 'MonoPatBinds' language extension to (hopefully) solve the issues
    caused by GHC bug http://hackage.haskell.org/trac/ghc/ticket/4498

* 0.2.1.0

  Incorporated several design changes:
    - Writable buffer range is now represented in a packed form. This improves
      speed slightly, as less currying is used.
    - Writes are abstracted such that their internal representation can be
      exchanged without breaking other library code.
    - Writes are represented in a form that allows for efficient monoid
      instances for branching code like UTF-8 encoding. For single character
      encoding this results currently in a slight slowdown due to GHC not
      recognizing the strictness of the returned value. This will be fixed in
      the future.
    - BuildSteps support returning a result in `Done`, which enables to
      implement a `Put` monad using CPS.
    - chunked list writes were removed, as they result in worse performance
      when writing non-trivial lists. (cf. benchmarks)
    - An internal buffering abstraction is introduced, which is used both
      by the adaption of the `binary` package, as well as by the
      `blaze-builder-enumeratee` package, to execute puts and builders.
      It will be used later also by the execution functions of the
      `blaze-builder` package.

  Implemented new functionality
    - `Blaze.ByteString.Builder.HTTP` provides a builder transformer for
       doing in-buffer chunked HTTP encoding of an arbitary other builder.
    - `Blaze.ByteString.Builder.Char8` provides functions to serialize the
       lower 8-bits of characters similiar to what `Data.ByteString.Char8`
       provides for bytestrings.

* 0.2.0.3

  Loosen 'text' dependency to '>= 0.10 && < 0.12'

* 0.2.0.2

  Fixed bug: use &#39; instead of &apos; for HTML escaping '

* 0.2.0.1

  Added a missing benchmark file.

* blaze-builder-0.2.0.0

  Heavily restructured 'blaze-builder' such that 'Blaze.ByteString.Builder' serves as
  a drop-in replacement of 'binary:Data.Binary.Builder' which it improves upon
  with respect to both speed as well as expressivity. See the documentation and
  the benchmarks for details on improvements and new functionality.

  Changed module structure:
    Blaze.ByteString.Builder.Core -> Blaze.ByteString.Builder
    Blaze.ByteString.Builder.Utf8 -> Blaze.ByteString.Builder.Char.Utf8
    Blaze.ByteString.Builder.Html -> Blaze.ByteString.Builder.Html.Utf8

  Changed function names:
    writeByte     -> writeWord8
    fromByte      -> fromWord8
    fromWriteList -> fromWrite1List

  Possibly performance sensitive implementation changes:
    - 'fromByteString' and 'fromLazyByteString' check now if a direct insertion
      of the bytestring(s) would be cheaper than copying it. See their
      documentation on how to recover the old behaviour.

  Deprecated functions:
    'empty'    : use 'mempty' instead
    'singleton': use 'fromWord8' instead
    'append'   : use 'mappend' instead


* blaze-builder-0.1

  This is the first version of 'blaze-builder'. It is explicitely targeted at
  fast generation of UTF-8 encoded HTML documents in the 'blaze-html' and the
  'hamlet' HTML templating libraries.
