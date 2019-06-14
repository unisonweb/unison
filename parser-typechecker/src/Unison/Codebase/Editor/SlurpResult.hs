{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.SlurpResult where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Set (Set)
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import Unison.Name ( Name )
import Unison.Parser ( Ann )
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DeclPrinter
import qualified Unison.HashQualified as HQ
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.TypePrinter as TP
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Pretty as P
import qualified Unison.Var as Var

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data SlurpResult v = SlurpResult {
  -- The file that we tried to add from
    originalFile :: UF.TypecheckedUnisonFile v Ann
  -- Extra definitions that were added to satisfy transitive closure,
  -- beyond what the user specified.
  , extraDefinitions :: SlurpComponent v
  -- Previously existed only in the file; now added to the codebase.
  , adds :: SlurpComponent v
  -- Exists in the branch and the file, with the same name and contents.
  , duplicates :: SlurpComponent v
  -- Not added to codebase due to the name already existing
  -- in the branch with a different definition.
  , collisions :: SlurpComponent v
  -- Not added to codebase due to the name existing
  -- in the branch with a conflict (two or more definitions).
  , conflicts :: SlurpComponent v
  -- Names that already exist in the branch, but whose definitions
  -- in `originalFile` are treated as updates.
  , updates :: SlurpComponent v
  -- Names of terms in `originalFile` that couldn't be updated because
  -- they refer to existing constructors. (User should instead do a find/replace,
  -- a constructor rename, or refactor the type that the name comes from).
  , termExistingConstructorCollisions :: Set v
  , constructorExistingTermCollisions :: Set v
  -- -- Already defined in the branch, but with a different name.
  , termAlias :: Map v (Set Name)
  , typeAlias :: Map v (Set Name)
  , defsWithBlockedDependencies :: SlurpComponent v
  } deriving (Show)

-- Remove `removed` from the slurp result, and move any defns with transitive
-- dependencies on the removed component into `defsWithBlockedDependencies`.
-- Also removes `removed` from `extraDefinitions`.
subtractComponent :: forall v. Var v => SlurpComponent v -> SlurpResult v -> SlurpResult v
subtractComponent removed sr =
  sr { adds = SC.difference (adds sr) (removed <> blocked)
     , updates = SC.difference (updates sr) (removed <> blocked)
     , defsWithBlockedDependencies = blocked
     , extraDefinitions = SC.difference (extraDefinitions sr) blocked
     }
  where
  -- for each v in adds, move to blocked if transitive dependency in removed
  blocked = defsWithBlockedDependencies sr <>
    SC.difference (blockedTerms <> blockedTypes) removed

  uf = originalFile sr
  constructorsFor v = case UF.lookupDecl v uf of
    Nothing -> mempty
    Just (_, e) -> Set.fromList . DD.constructorVars $ either DD.toDataDecl id e

  blockedTypes = foldMap doType . SC.types $ adds sr <> updates sr where
    -- include this type if it or any of its dependencies are removed
    doType :: v -> SlurpComponent v
    doType v =
      if null (Set.intersection (SC.types removed) (SC.types (SC.closeWithDependencies uf vc)))
         && null (Set.intersection (SC.terms removed) (constructorsFor v))
      then mempty else vc
      where vc = mempty { types = Set.singleton v }

  blockedTerms = foldMap doTerm . SC.terms $ adds sr <> updates sr where
    doTerm :: v -> SlurpComponent v
    doTerm v =
      if mempty == SC.intersection removed (SC.closeWithDependencies uf vc)
      then mempty else vc
      where vc = mempty { terms = Set.singleton v }

-- Move `updates` to `collisions`, and move any dependents of those updates to `*WithBlockedDependencies`.
-- Subtract stuff from `extraDefinitions` that isn't in `adds` or `updates`
disallowUpdates :: forall v. Var v => SlurpResult v -> SlurpResult v
disallowUpdates sr =
  let sr2 = subtractComponent (updates sr) sr
  in sr2 { collisions = collisions sr2 <> updates sr }

isNonempty :: Ord v => SlurpResult v -> Bool
isNonempty s = Monoid.nonEmpty (adds s) || Monoid.nonEmpty (updates s)

data Status =
  Add | Update | Duplicate | Alias | Collision | Conflicted |
  TermExistingConstructorCollision | ConstructorExistingTermCollision |
  ExtraDefinition |
  BlockedDependency
  deriving (Ord,Eq,Show)

isFailure :: Status -> Bool
isFailure s = case s of
  TermExistingConstructorCollision -> True
  ConstructorExistingTermCollision -> True
  BlockedDependency -> True
  Collision -> True
  Conflicted -> True
  _ -> False

prettyStatus :: Status -> P.Pretty P.ColorText
prettyStatus s = case s of
  Add -> P.green "+"
  Update -> P.green "Δ"
  Duplicate -> P.hiBlack "d"
  Collision -> P.red "x"
  Conflicted -> P.red "c"
  TermExistingConstructorCollision -> P.red "y"
  ConstructorExistingTermCollision -> P.red "z"
  BlockedDependency -> P.hiRed "b"
  ExtraDefinition -> P.purple "e"
  Alias -> P.blue "a"

statusLegend :: Set Status -> P.Pretty P.ColorText
statusLegend s =
  P.indentN 3 "Legend:" <> "\n" <>
  (P.sep "\n" $ map go (toList s))
  where
  go s = P.group (prettyStatus s <> " " <> desc s)
  desc s = " " <> case s of
    Add -> "a new definition with a new name"
    Update -> "a new definition using an existing name (ok to `update`)"
    Duplicate -> "already in the codebase with the same name"
    Collision -> "couldn't be added due to name collision (use `update`)"
    Conflicted -> "can't be updated because the name is conflicted"
    TermExistingConstructorCollision -> "a term with the same name as a constructor"
    ConstructorExistingTermCollision -> "a constructor with the same name as a term"
    BlockedDependency -> "blocked because of one of its dependencies"
    ExtraDefinition -> "added because an extra definition"
    Alias -> "a new name for an existing definition"

pretty :: Var v => PPE.PrettyPrintEnv -> SlurpResult v -> P.Pretty P.ColorText
pretty ppe sr = case pretty' ppe sr of
  (pp, s) | Set.null s -> mempty
          | otherwise  -> P.lines [statusLegend s, "", "", pp]

{-
  + = added, Δ = updated, a = alias, d = duplicate (use `help messages.add` to learn more)

  +  Stream.unfold : s -> (s -> Optional (a, s)) -> Stream s
  Δ  Stream.range  : Nat -> Nat -> Stream s Nat
  Δ  Stream.map    : (a -> b) -> Stream a -> Stream b
  a  blah, curName : Foo
  a  blah2, olName : Bar
  d  dup1          : Nat
  d  dup2          : Nat

  +  ability Woot
-}
pretty' :: Var v => PPE.PrettyPrintEnv -> SlurpResult v -> (P.Pretty P.ColorText, Set Status)
pretty' ppe sr = let
  tms = UF.hashTerms (originalFile sr)
  termLineFor status v = case Map.lookup v tms of
    Just (_,_,ty) ->
      [(prettyStatus status <> "  " <> lhs,
       ": " <> P.indentNAfterNewline 2 (TP.prettyTop ppe ty))]
      where
      lhs = case Map.lookup v (termAlias sr) of
        Nothing -> P.bold (P.text $ Var.name v)
        Just ns -> P.sep "," (P.bold (P.text $ Var.name v) : (P.shown <$> toList ns))
    Nothing ->
      if isFailure status then [(prettyStatus status <> "  " <> P.text (Var.name v), "")]
      else []
  typeLineFor status v = case UF.lookupDecl v (originalFile sr) of
    Just (_, dd) ->
      prettyStatus status <> "  " <> DeclPrinter.prettyDeclHeader (HQ.fromVar v) dd
    Nothing ->
      prettyStatus status <> "  "
        <> P.bold (P.text (Var.name v))
        <> P.red (P.wrap ("(Unison bug, unknown type)"))

  termMsgs = P.column2 . join $
    (termLineFor Add <$> toList (terms (adds sr))) ++
    (termLineFor ExtraDefinition <$> toList (terms (extraDefinitions sr))) ++
    (termLineFor Update <$> toList (terms (updates sr))) ++
    (termLineFor Alias <$> Map.keys (termAlias sr)) ++
    (termLineFor Duplicate <$> toList (terms (duplicates sr))) ++
    (termLineFor Conflicted <$> toList (terms (conflicts sr))) ++
    (termLineFor Collision <$> toList (terms (collisions sr))) ++
    (termLineFor TermExistingConstructorCollision <$> toList (termExistingConstructorCollisions sr)) ++
    (termLineFor ConstructorExistingTermCollision <$> toList (constructorExistingTermCollisions sr)) ++
    (termLineFor BlockedDependency <$> toList (terms (defsWithBlockedDependencies sr)))

  typeMsgs = P.lines $
    (typeLineFor Add <$> toList (types (adds sr))) ++
    (typeLineFor ExtraDefinition <$> toList (types (extraDefinitions sr))) ++
    (typeLineFor Update <$> toList (types (updates sr))) ++
    (typeLineFor Alias <$> Map.keys (typeAlias sr)) ++
    (typeLineFor Duplicate <$> toList (types (duplicates sr))) ++
    (typeLineFor Conflicted <$> toList (types (conflicts sr))) ++
    (typeLineFor Collision <$> toList (types (collisions sr))) ++
    (typeLineFor BlockedDependency <$> toList (types (defsWithBlockedDependencies sr)))

  statuses = Set.fromList . join $
    [ if SC.isEmpty (adds sr) then [] else [Add]
    , if SC.isEmpty (extraDefinitions sr) then [] else [ExtraDefinition]
    , if SC.isEmpty (updates sr) then [] else [Update]
    , if Map.null (termAlias sr) && Map.null (typeAlias sr) then [] else [Alias]
    , if SC.isEmpty (duplicates sr) then [] else [Duplicate]
    , if SC.isEmpty (conflicts sr) then [] else [Conflicted]
    , if SC.isEmpty (collisions sr) then [] else [Collision]
    , if Set.null (termExistingConstructorCollisions sr) then [] else [TermExistingConstructorCollision]
    , if Set.null (constructorExistingTermCollisions sr) then [] else [ConstructorExistingTermCollision]
    , if SC.isEmpty (defsWithBlockedDependencies sr) then [] else [BlockedDependency]
    ]

  in (P.sepNonEmpty "\n\n" [termMsgs, typeMsgs], statuses)
