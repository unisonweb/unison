-- | The optimizations a process can switch on or off, all under one
-- environment variable, @UNISON_OPTIMIZATIONS@: a comma-separated list of
-- names, each on its own or after @+@ (on) or @-@ (off), and the words
-- @all@ and @none@. Later entries override earlier ones, so
-- @none,explode_returns@ keeps only the return workers. A name the variable
-- does not mention keeps its default, which may depend on the JIT mode
-- (the wide convention and the replacements follow @UNISON_JIT@). An
-- unknown name is an error, so a misspelling cannot pass as a measurement.
--
-- Read once, on first use, like the settings it feeds ('Unison.Runtime.Wide',
-- 'Unison.Runtime.JIT.Config', 'Unison.Runtime.MCode.Explode'). Every
-- optimization here changes only the code the runtime runs, never a stored
-- value or hash, which is why a stray setting is allowed to take effect
-- (the debugging switches that weaken the runtime stay under
-- @UNISON_JIT_DISABLE@, read only with @UNISON_JIT_DEBUG@).
module Unison.Runtime.Optimizations
  ( Optimization (..),
    optimizationName,
    optimizationSetting,
    parseOptimizations,
  )
where

import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Data.Map.Strict qualified as M
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

data Optimization
  = -- | @explode_args@: loop state passed as fields to a specialised worker
    -- (Unison.Runtime.MCode.Explode, design.md "Loop state exploded")
    ExplodeArgs
  | -- | @explode_returns@: a product result returned as its leaves, in
    -- several values, by a product-return worker (design.md "Results
    -- exploded")
    ExplodeReturns
  | -- | @known_constructor@: a product or sum built and matched in the
    -- same body is matched where it is built, the let dropped and the
    -- arm's variables renamed to the fields; a match on the value of a
    -- match whose every tail builds a constructor is moved into those
    -- tails, as a call of a join point per arm (Unison.Runtime.MCode.Explode,
    -- design.md "Known constructors and case of case")
    KnownConstructor
  | -- | @wide@: the wide calling convention, small constructors in stack
    -- slots between a return and a match (Unison.Runtime.Wide); on by
    -- default exactly when the JIT is
    Wide
  | -- | @replacements@: the Haskell replacements of base library
    -- definitions (Unison.Runtime.JIT.Config); by default all of them
    -- under the interpreter and the tagged ones under the JIT, so @on@
    -- here is all and @off@ none
    Replacements
  deriving (Eq, Ord, Show, Enum, Bounded)

optimizationName :: Optimization -> String
optimizationName = \case
  ExplodeArgs -> "explode_args"
  ExplodeReturns -> "explode_returns"
  KnownConstructor -> "known_constructor"
  Wide -> "wide"
  Replacements -> "replacements"

-- | What @UNISON_OPTIMIZATIONS@ says about one optimization: 'Nothing'
-- when it is not mentioned, so the default applies.
optimizationSetting :: Optimization -> Maybe Bool
optimizationSetting o = M.lookup o settings

settings :: M.Map Optimization Bool
settings = unsafePerformIO $ do
  v <- lookupEnv "UNISON_OPTIMIZATIONS"
  pure $ maybe M.empty (either error id . parseOptimizations) v
{-# NOINLINE settings #-}

-- | The settings a value of the variable gives, or the complaint about it.
parseOptimizations :: String -> Either String (M.Map Optimization Bool)
parseOptimizations = go M.empty . map (map toLower . trim) . splitOn ','
  where
    go m [] = Right m
    go m ("" : rest) = go m rest
    go _ ("all" : rest) = go (everything True) rest
    go _ ("none" : rest) = go (everything False) rest
    go m (('-' : n) : rest) = named m n False rest
    go m (('+' : n) : rest) = named m n True rest
    go m (n : rest) = named m n True rest
    named m n on rest = case lookup n byName of
      Just o -> go (M.insert o on m) rest
      Nothing ->
        Left $
          "UNISON_OPTIMIZATIONS: no optimization is called "
            ++ show n
            ++ "; the names are "
            ++ intercalate ", " (map fst byName)
            ++ ", each on its own or after '-' to switch it off, and all or none"
    byName = [(optimizationName o, o) | o <- [minBound .. maxBound]]
    everything on = M.fromList [(o, on) | o <- [minBound .. maxBound]]
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest
