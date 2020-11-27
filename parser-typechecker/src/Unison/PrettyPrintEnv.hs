{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ConstructorType as CT
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names3 (Names)
import qualified Unison.Names3 as Names
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Util.List (safeHead)

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests
    terms :: Referent -> Maybe HashQualified,
    -- names for types
    types :: Reference -> Maybe HashQualified
  }

patterns :: PrettyPrintEnv -> Reference -> Int -> Maybe HashQualified
patterns ppe r cid =
  terms ppe (Referent.Con r cid CT.Data)
    <|> terms ppe (Referent.Con r cid CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

fromNames :: Int -> Names -> PrettyPrintEnv
fromNames len names = PrettyPrintEnv terms' types'
  where
    terms' r = shortestName . Set.map HQ'.toHQ $ Names.termName len r names
    types' r = shortestName . Set.map HQ'.toHQ $ Names.typeName len r names
    shortestName ns = safeHead $ HQ.sortByLength (toList ns)

fromSuffixNames :: Int -> Names -> PrettyPrintEnv
fromSuffixNames len names = fromNames len (Names.suffixify names)

fromNamesDecl :: Int -> Names -> PrettyPrintEnvDecl
fromNamesDecl len names =
  PrettyPrintEnvDecl (fromNames len names) (fromSuffixNames len names)

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDecl = PrettyPrintEnvDecl
  { unsuffixifiedPPE :: PrettyPrintEnv,
    suffixifiedPPE :: PrettyPrintEnv
  }
  deriving (Show)

-- declarationPPE uses the full name for references that are
-- part the same cycle as the input reference, used to ensures
-- recursive definitions are printed properly, for instance:
--
-- foo.bar x = foo.bar x
-- and not
-- foo.bar x = bar x
declarationPPE :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnv
declarationPPE ppe rd = PrettyPrintEnv tm ty
  where
    comp = Reference.members (Reference.componentFor rd)
    tm r0@(Referent.Ref r) =
      if Set.member r comp
        then terms (unsuffixifiedPPE ppe) r0
        else terms (suffixifiedPPE ppe) r0
    tm r = terms (suffixifiedPPE ppe) r
    ty r =
      if Set.member r comp
        then types (unsuffixifiedPPE ppe) r
        else types (suffixifiedPPE ppe) r

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft e1 e2 =
  PrettyPrintEnv
    (\r -> terms e1 r <|> terms e2 r)
    (\r -> types e1 r <|> types e2 r)

assignTermName :: Referent -> HashQualified -> PrettyPrintEnv -> PrettyPrintEnv
assignTermName r name = (fromTermNames [(r, name)] `unionLeft`)

fromTypeNames :: [(Reference, HashQualified)] -> PrettyPrintEnv
fromTypeNames types =
  let m = Map.fromList types
   in PrettyPrintEnv (const Nothing) (`Map.lookup` m)

fromTermNames :: [(Referent, HashQualified)] -> PrettyPrintEnv
fromTermNames tms =
  let m = Map.fromList tms
   in PrettyPrintEnv (`Map.lookup` m) (const Nothing)

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: PrettyPrintEnv -> Referent -> HashQualified
termName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReferent r) (terms env r)

typeName :: PrettyPrintEnv -> Reference -> HashQualified
typeName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReference r) (types env r)

patternName :: PrettyPrintEnv -> Reference -> Int -> HashQualified
patternName env r cid =
  case patterns env r cid of
    Just name -> name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r cid

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const Nothing) (const Nothing)
  mappend = unionLeft

instance Semigroup PrettyPrintEnv where
  (<>) = mappend

-- Type aliases relating to Fully-Qualified Names, e.g. 'Acme.API.foo'
-- Used primarily by the FQN elision code - see TermPrinter.PrintAnnotation.

-- Note that a Suffix can include dots.
type Suffix = Text

-- Each member of a Prefix list is dot-free.
type Prefix = [Text]

-- Keys are FQNs, values are shorter names which are equivalent, thanks to use
-- statements that are in scope.
type Imports = Map Name Suffix

-- Give the shortened version of an FQN, if there's been a `use` statement for that FQN.
elideFQN :: Imports -> HQ.HashQualified -> HQ.HashQualified
elideFQN imports hq =
  let hash = HQ.toHash hq
      name' = do
        name <- HQ.toName hq
        let hit = fmap Name.unsafeFromText (Map.lookup name imports)
        -- Cut out the "const id $" to get tracing of FQN elision attempts.
        let t = const id $ trace ("hit: " ++ show hit ++ " finding: " ++ show hq ++ " in imports: " ++ show imports)
        t (pure $ fromMaybe name hit)
   in HQ.fromNameHash name' hash
