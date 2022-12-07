{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl
  ( PrettyPrintEnvDecl (..),
    biasTo,
    empty,
    addFallback,
  )
where

import Unison.Name (Name)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import qualified Unison.PrettyPrintEnv as PPE

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDecl m = PrettyPrintEnvDecl
  { unsuffixifiedPPE :: PrettyPrintEnv m,
    suffixifiedPPE :: PrettyPrintEnv m
  }
  deriving (Show)

-- | Lifts 'biasTo' over a PrettyPrintEnvDecl m
biasTo :: Functor m => [Name] -> PrettyPrintEnvDecl m -> PrettyPrintEnvDecl m
biasTo targets PrettyPrintEnvDecl {unsuffixifiedPPE, suffixifiedPPE} =
  PrettyPrintEnvDecl
    { unsuffixifiedPPE = PPE.biasTo targets unsuffixifiedPPE,
      suffixifiedPPE = PPE.biasTo targets suffixifiedPPE
    }

empty :: Applicative m => PrettyPrintEnvDecl m
empty = PrettyPrintEnvDecl PPE.empty PPE.empty

addFallback :: Monad m => PrettyPrintEnvDecl m -> PrettyPrintEnvDecl m -> PrettyPrintEnvDecl m
addFallback (PrettyPrintEnvDecl unsuff1 suff1) (PrettyPrintEnvDecl unsuff2 suff2) =
  PrettyPrintEnvDecl (unsuff1 `PPE.addFallback` unsuff2) (suff1 `PPE.addFallback` suff2)
