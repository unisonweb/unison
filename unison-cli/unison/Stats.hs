module Stats
  ( RtsStatsPath (..),
    recordRtsStats,
  )
where

import Control.Exception (finally)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Data.String (IsString)
import GHC.Stats

newtype RtsStatsPath
  = RtsStatsPath FilePath
  deriving stock (Show, Eq)
  deriving newtype (IsString)

recordRtsStats :: RtsStatsPath -> IO a -> IO a
recordRtsStats (RtsStatsPath fp) action = do
  r0 <- getRTSStats
  action `finally` do
    r1 <- getRTSStats
    BL.writeFile fp (encode (produceStats r0 r1))
  where
    produceStats r0 r1 =
      object
        [ "gcs" .= on (-) gcs r1 r0,
          "major_gcs" .= on (-) major_gcs r1 r0,
          "allocated_bytes" .= on (-) allocated_bytes r1 r0,
          "max_live_bytes" .= on (-) max_live_bytes r1 r0,
          "copied_bytes" .= on (-) copied_bytes r1 r0,
          "mutator_cpu_ns" .= on (-) mutator_cpu_ns r1 r0,
          "mutator_elapsed_ns" .= on (-) mutator_elapsed_ns r1 r0,
          "gc_cpu_ns" .= on (-) mutator_cpu_ns r1 r0,
          "gc_elapsed_ns" .= on (-) mutator_elapsed_ns r1 r0,
          "cpu_ns" .= on (-) cpu_ns r1 r0,
          "elapsed_ns" .= on (-) cpu_ns r1 r0
        ]
