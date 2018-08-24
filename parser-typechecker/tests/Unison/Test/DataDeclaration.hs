{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Unison.Test.DataDeclaration where

import EasyTest
import Text.RawString.QQ
import Unison.UnisonFile (UnisonFile(..))
import Unison.Symbol (Symbol)
import qualified Unison.Var as Var
import qualified Data.Map as Map
import Unison.Parser (Ann, penv0)
import Unison.Parsers (unsafeParseFile)
import Unison.DataDeclaration (hashDecls)

test :: Test ()
test = scope "datadeclaration" $
  let hashes = hashDecls . (snd <$>) . dataDeclarations $ file
      hashMap = Map.fromList $ fmap (\(a,b,_) -> (a,b)) hashes
      hashOf k = Map.lookup (Var.named k) hashMap
  in tests [
    scope "Bool == Bool'" . expect $ hashOf "Bool" == hashOf "Bool'",
    scope "Bool != Option'" . expect $ hashOf "Bool" /= hashOf "Option'",
    scope "Option == Option'" . expect $ hashOf "Option" == hashOf "Option'",
    scope "List == List'" . expect $ hashOf "List" == hashOf "List'",
    scope "List != SnocList" . expect $ hashOf "List" /= hashOf "SnocList",
    scope "Ping != Pong" . expect $ hashOf "Ping" /= hashOf "Pong",
    scope "Ping == Ling'" . expect $ hashOf "Ping" == hashOf "Ling'",
    scope "Pong == Long'" . expect $ hashOf "Pong" == hashOf "Long'"
  ]

file :: UnisonFile Symbol Ann
file = snd . flip unsafeParseFile penv0 $ [r|

type Bool = True | False
type Bool' = False | True

type Option a = Some a | None
type Option' b = Nothing | Just b

type List a = Nil | Cons a (List a)
type List' b = Prepend b (List' b) | Empty
type SnocList a = Snil | Snoc (List a) a

type ATree a = Tree a (List (ATree a)) | Leaf (Option a)

type Ping a = Ping a (Pong a)
type Pong a = Pnong | Pong (Ping a)

type Long' a = Long' (Ling' a) | Lnong
type Ling' a = Ling' a (Long' a)
()
|]


-- faketest = scope "termparser" . tests . map parses $
--   ["x"
--   , "case x of\n" ++
--     "  {Pair x y} -> 1\n" ++
--     "  {State.set 42 -> k} -> k 42\n"
--   ]
--
-- builtins = Map.fromList
--   [("Pair", (R.Builtin "Pair", 0)),
--    ("State.set", (R.Builtin "State", 0))]
--
-- parses s = scope s $ do
--   let p = unsafeParseTerm s builtins :: Term Symbol
--   noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
--   ok
