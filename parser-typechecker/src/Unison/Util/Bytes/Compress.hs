module Unison.Util.Bytes.Compress where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Unison.Util.Bytes (Bytes, fromLazyByteString, toLazyByteString)

zlibCompress :: Bytes -> Bytes
zlibCompress = fromLazyByteString . Zlib.compress . toLazyByteString

gzipCompress :: Bytes -> Bytes
gzipCompress = fromLazyByteString . GZip.compress . toLazyByteString

gzipDecompress :: Bytes -> Bytes
gzipDecompress = fromLazyByteString . GZip.decompress . toLazyByteString

zlibDecompress :: Bytes -> Bytes
zlibDecompress = fromLazyByteString . Zlib.decompress . toLazyByteString
