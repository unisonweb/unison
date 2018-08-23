{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Test.Typechecker.TypeError where

import           Data.Foldable                (toList)
import           Data.Maybe                   (isJust)
import           EasyTest
import qualified Unison.FileParsers           as FileParsers
import           Unison.Parser                (Ann)
import           Unison.Result                (Result (..))
import qualified Unison.Result                as Result
import qualified Unison.Typechecker.Context   as C
import           Unison.Typechecker.Extractor (NoteExtractor)
import qualified Unison.Typechecker.Extractor as Ex
-- import           Unison.Typechecker.TypeError
import           Unison.Var                   (Var)

test :: Test ()
test = scope "extractor" . tests $
  [ --y "and true 3"

  ]

noYieldsError :: Var v => String -> NoteExtractor v Ann a -> Bool
noYieldsError s ex = not $ yieldsError s ex

yieldsError :: forall v a. Var v => String -> NoteExtractor v Ann a -> Bool
yieldsError s ex = let
  Result notes (Just _) = FileParsers.parseAndSynthesizeAsFile "test" s
  notes' :: [C.Note v Ann]
  notes' = [ n | Result.Typechecking n <- toList notes ]-- ugly, should NoteExtractor operate on top-level notes?
  in any (isJust . Ex.run ex) notes'
