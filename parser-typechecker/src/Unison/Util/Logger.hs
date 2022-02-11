{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
-- | Small logging library. Typical usage, import qualified:
--
--   import qualified Unison.Util.Logger as L
--
--   do
--     logger <- L.atomic . L.atInfo . L.scope "worker" . L.toHandle $ stderr
--     L.warn logger "WARNING!!!"
--     L.debug logger "Debug message, will be ignored"
--     let logger2 = L.atDebug logger
--     L.debug logger2 "Debug message, will be printed"
--     logger' <- L.at L.warnLevel
--
module Unison.Util.Logger where

import Unison.Prelude

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.List
import System.IO (Handle, hPutStrLn, hGetLine, stdout, stderr)
import System.IO.Error (isEOFError)

type Level = Int
type Scope = [String]

data Logger =
  Logger { getScope :: !Scope
         , prefix :: String -> String
         , getLevel :: !Level
         , raw :: String -> IO () }

-- | Ensure at most one message is logged at the same time
atomic :: Logger -> IO Logger
atomic logger = do
  lock <- newMVar ()
  pure $
    let raw' msg = bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> raw logger msg)
    in logger { raw = raw' }

toHandle :: Handle -> Logger
toHandle h = logger (hPutStrLn h)

toStandardError :: Logger
toStandardError = toHandle stderr

toStandardOut :: Logger
toStandardOut = toHandle stdout

logHandleAt :: Logger -> Level -> Handle -> IO ()
logHandleAt logger lvl h
  | lvl > getLevel logger = pure ()
  | otherwise = void . forkIO $ loop where
    loop = do
      line <- try (hGetLine h)
      case line of
        Left ioe | isEOFError ioe -> logAt (scope "logHandleAt" logger) 3 "EOF"
                 | otherwise      -> logAt (scope "logHandleAt" logger) 2 (show ioe)
        Right line -> logAt logger lvl line >> loop

logAt' :: Logger -> Level -> IO String -> IO ()
logAt' logger lvl msg | lvl <= getLevel logger = msg >>= \msg -> raw logger (prefix logger msg)
                      | otherwise              = pure ()

logAt :: Logger -> Level -> String -> IO ()
logAt logger lvl msg | lvl <= getLevel logger = raw logger (prefix logger msg)
                     | otherwise              = pure ()

scope :: String -> Logger -> Logger
scope s (Logger s0 _ lvl raw) = Logger s' prefix' lvl raw where
  prefix' msg = prefix ++ msg
  prefix = "[" ++ intercalate " " s' ++ "] "
  s' = s:s0

scope' :: [String] -> Logger -> Logger
scope' s l = foldr scope l s

logger :: (String -> IO ()) -> Logger
logger log = Logger [] id 0 log

error, warn, info, debug, trace :: Logger -> String -> IO ()
error l = logAt l errorLevel
warn l = logAt l warnLevel
info l = logAt l infoLevel
debug l = logAt l debugLevel
trace l = logAt l traceLevel

error', warn', info', debug', trace' :: Logger -> IO String -> IO ()
error' l = logAt' l errorLevel
warn' l = logAt' l warnLevel
info' l = logAt' l infoLevel
debug' l = logAt' l debugLevel
trace' l = logAt' l traceLevel

errorLevel, warnLevel, infoLevel, debugLevel, traceLevel :: Level
(errorLevel, warnLevel, infoLevel, debugLevel, traceLevel) = (1,2,3,4,5)

at :: Level -> Logger -> Logger
at lvl logger = logger { getLevel = lvl }

atError, atWarn, atInfo, atDebug, atTrace :: Logger -> Logger
(atError, atWarn, atInfo, atDebug, atTrace) =
  (at errorLevel, at warnLevel, at infoLevel, at debugLevel, at traceLevel)

increment :: Logger -> Logger
increment (Logger s p n l) = Logger s p (n+1) l

decrement :: Logger -> Logger
decrement (Logger s p n l) = Logger s p (n-1) l
