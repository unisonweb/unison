-- | Dictionary selection for already determined, monomorphic goals.
module Unison.Typechecker.GivenResolver
  ( GivenName (..),
    Scope (..),
    Given (..),
    ResolveError (..),
    resolve,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Unison.Reference (TermReference)
import Unison.Type (Type)
import Unison.Var (Var)

-- | Local binding identities must be freshened variables, never printed names.
data GivenName v = Local v | Global TermReference
  deriving stock (Eq, Ord, Show)

-- | Larger depths are more local. Namespace candidates have the lowest priority.
data Scope = Ambient | Lexical Int
  deriving stock (Eq, Ord, Show)

data Given v loc = Given
  { givenName :: GivenName v,
    givenConclusion :: Type v loc,
    givenScope :: Scope
  }
  deriving stock (Show)

data ResolveError v loc
  = NoGiven (Type v loc)
  | Ambiguous (Type v loc) (NonEmpty (GivenName v))
  deriving stock (Show)

-- | Only matching candidates participate in shadowing. Repeated namespace
-- aliases identify the same dictionary and do not create ambiguity.
resolve :: (Var v) => [Given v loc] -> Type v loc -> Either (ResolveError v loc) (GivenName v)
resolve pool goal =
  case Map.toAscList matches of
    [] -> Left (NoGiven goal)
    candidates@((_, firstScope) : rest) ->
      let depth = foldl' max firstScope (map snd rest)
       in case [name | (name, scope) <- candidates, scope == depth] of
            [name] -> Right name
            name : names -> Left (Ambiguous goal (name :| names))
            [] -> Left (NoGiven goal)
  where
    matches =
      Map.fromListWith
        max
        [ (givenName, givenScope)
        | Given {givenName, givenConclusion, givenScope} <- pool,
          givenConclusion == goal
        ]
