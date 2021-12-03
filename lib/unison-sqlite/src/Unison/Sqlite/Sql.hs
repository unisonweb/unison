{-# LANGUAGE TemplateHaskellQuotes #-}

module Unison.Sqlite.Sql
  ( Sql (..),
    sqlToText,
    sql,
  )
where

import Control.Monad.IO.Unlift
import Data.IORef
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import System.IO.Unsafe (unsafePerformIO)
import Unison.Prelude

-- | A SQL snippet.
data Sql = Sql
  { -- A unique identifier for this snippet.
    uniqueId :: !Int,
    -- The actual SQL
    string :: !Text
  }

instance Show Sql where
  show Sql {string} = show string

sqlToText :: Sql -> Text
sqlToText Sql {string} =
  string

sql :: TH.QuasiQuoter
sql =
  TH.QuasiQuoter
    { quoteExp = sqlExp,
      quoteDec = undefined,
      quotePat = undefined,
      quoteType = undefined
    }
  where
    sqlExp :: String -> TH.Q TH.Exp
    sqlExp s = do
      n <- liftIO (atomicModifyIORef' nextUniqueIdRef \n -> (n + 1, n))
      [|Sql n (Text.pack s)|]

nextUniqueIdRef :: IORef Int
nextUniqueIdRef =
  unsafePerformIO (newIORef 0)
{-# NOINLINE nextUniqueIdRef #-}
