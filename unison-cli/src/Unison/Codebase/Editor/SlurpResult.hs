{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.SlurpResult where

import Unison.Prelude

import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import Unison.Name ( Name )
import Unison.Parser.Ann ( Ann )
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.DeclPrinter as DeclPrinter
import qualified Unison.HashQualified as HQ
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.TypePrinter as TP
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import qualified Unison.Var as Var

-- `oldRefNames` are the previously existing names for the old reference
--   (these names will all be pointed to a new reference)
-- `newRefNames` are the previously existing names for the new reference
--   (the reference that all the old names will point to after the update)
data Aliases
  = AddAliases (Set Name)
  | UpdateAliases { oldRefNames :: Set Name
                  , newRefNames :: Set Name }
  deriving (Show, Eq, Ord)

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
  -- I.e. an update is required but we're performing an add.
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
  , termAlias :: Map v Aliases
  , typeAlias :: Map v Aliases
  , defsWithBlockedDependencies :: SlurpComponent v
  } deriving (Show)

hasAddsOrUpdates :: Ord v => SlurpResult v -> Bool
hasAddsOrUpdates s =
  -- We intentionally ignore constructors here since they are added as part of adding their
  -- types.
  let SC.SlurpComponent{terms=termAdds, types=typeAdds} = adds s
      SC.SlurpComponent{terms=termUpdates, types=typeUpdates} = updates s
   in not . null $ termAdds <> typeAdds <> termUpdates <> typeUpdates

data Status =
  Add | Update | Duplicate | Collision | Conflicted |
  TermExistingConstructorCollision | ConstructorExistingTermCollision |
  ExtraDefinition | BlockedDependency
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
  Add                              -> "added"
  Update                           -> "updated"
  Collision                        -> "needs update"
  Conflicted                       -> "conflicted"
  Duplicate                        -> "duplicate"
  TermExistingConstructorCollision -> "term/ctor collision"
  ConstructorExistingTermCollision -> "ctor/term collision"
  BlockedDependency                -> "blocked"
  ExtraDefinition                  -> "extra dependency"

type IsPastTense = Bool

prettyVar :: Var v => v -> P.Pretty P.ColorText
prettyVar = P.text . Var.name

aliasesToShow :: Int
aliasesToShow = 5

pretty
  :: forall v
   . Var v
  => IsPastTense
  -> PPE.PrettyPrintEnv
  -> SlurpResult v
  -> P.Pretty P.ColorText
pretty isPast ppe sr =
  let
    tms      = UF.hashTerms (originalFile sr)
    goodIcon = P.green "⍟ "
    badIcon  = P.red "x "
    plus     = P.green "  "
    oxfordAliases shown sz end =
      P.oxfordCommasWith end $ (P.shown <$> shown) ++ case sz of
        0 -> []
        n -> [P.shown n <> " more"]
    okType v = (plus <>) $ case UF.lookupDecl v (originalFile sr) of
      Just (_, dd) ->
        P.syntaxToColor (DeclPrinter.prettyDeclHeader (HQ.unsafeFromVar v) dd)
          <> if null aliases
               then mempty
               else P.newline <> P.indentN 2 (P.lines aliases)
        where aliases = aliasesMessage . Map.lookup v $ typeAlias sr
      Nothing -> P.bold (prettyVar v) <> P.red " (Unison bug, unknown type)"

    aliasesMessage aliases = case aliases of
      Nothing -> []
      Just (AddAliases (splitAt aliasesToShow . toList -> (shown, rest))) ->
        [ P.indentN 2 . P.wrap $
            P.hiBlack "(also named " <> oxfordAliases
              shown
              (length rest)
              (P.hiBlack ")")
        ]
      Just (UpdateAliases oldNames newNames) ->
        let oldMessage =
                let (shown, rest) = splitAt aliasesToShow $ toList oldNames
                    sz            = length oldNames
                in  P.indentN
                      2
                      (  P.wrap
                      $  P.hiBlack
                           (  "(The old definition "
                           <> (if isPast then "was" else "is")
                           <> " also named "
                           )
                      <> oxfordAliases shown (length rest) (P.hiBlack ".")
                      <> P.hiBlack
                           (case (sz, isPast) of
                             (1, True ) -> "I updated this name too.)"
                             (1, False) -> "I'll update this name too.)"
                             (_, True ) -> "I updated these names too.)"
                             (_, False) -> "I'll update these names too.)"
                           )
                      )
            newMessage =
                let (shown, rest) = splitAt aliasesToShow $ toList newNames
                    sz            = length rest
                in  P.indentN
                      2
                      (  P.wrap
                      $  P.hiBlack "(The new definition is already named "
                      <> oxfordAliases shown sz (P.hiBlack " as well.)")
                      )
        in  (if null oldNames then mempty else [oldMessage])
              ++ (if null newNames then mempty else [newMessage])

    -- The second field in the result is an optional second column.
    okTerm :: v -> [(P.Pretty P.ColorText, Maybe (P.Pretty P.ColorText))]
    okTerm v = case Map.lookup v tms of
      Nothing ->
        [(P.bold (prettyVar v), Just $ P.red "(Unison bug, unknown term)")]
      Just (_, _, _, ty) ->
        ( plus <> P.bold (prettyVar v)
          , Just $ ": " <> P.indentNAfterNewline 2 (TP.pretty ppe ty)
          )
          : ((, Nothing) <$> aliases)
       where
        aliases = fmap (P.indentN 2) . aliasesMessage . Map.lookup v $ termAlias sr
    ok _ _ sc | null (SC.terms sc) && null (SC.types sc) = mempty
    ok past present sc =
      let header = goodIcon <> P.indentNAfterNewline
            2
            (P.wrap (if isPast then past else present))
          updatedTypes = P.lines $ okType <$> toList (SC.types sc)
          updatedTerms = P.mayColumn2 . (=<<) okTerm . Set.toList $ SC.terms sc
      in  header <> "\n\n" <> P.linesNonEmpty [updatedTypes, updatedTerms]
    okToUpdate = ok
      (P.green "I've updated these names to your new definition:")
      (  P.green
      $  "These names already exist. You can `update` them "
      <> "to your new definition:"
      )
    okToAdd = ok (P.green "I've added these definitions:")
                 (P.green "These new definitions are ok to `add`:")
    notOks _past _present sr | isOk sr = mempty
    notOks past present sr =
      let
        header = badIcon <> P.indentNAfterNewline
          2
          (P.wrap (if isPast then past else present))
        typeLineFor status v = case UF.lookupDecl v (originalFile sr) of
          Just (_, dd) ->
            ( prettyStatus status
            , P.syntaxToColor
              $ DeclPrinter.prettyDeclHeader (HQ.unsafeFromVar v) dd
            )
          Nothing ->
            ( prettyStatus status
            , prettyVar v <> P.red (P.wrap " (Unison bug, unknown type)")
            )
        typeMsgs =
          P.column2
            $  (typeLineFor Conflicted <$> toList (types (conflicts sr)))
            ++ (typeLineFor Collision <$> toList (types (collisions sr)))
            ++ (   typeLineFor BlockedDependency
               <$> toList (types (defsWithBlockedDependencies sr))
               )
        termLineFor status v = case Map.lookup v tms of
          Just (_ref, _wk, _tm, ty) ->
            ( prettyStatus status
            , P.bold (P.text $ Var.name v)
            , ": " <> P.indentNAfterNewline 6 (TP.pretty ppe ty)
            )
          Nothing -> (prettyStatus status, P.text (Var.name v), "")
        termMsgs =
          P.column3sep "  "
            $  (termLineFor Conflicted <$> toList (terms (conflicts sr)))
            ++ (termLineFor Collision <$> toList (terms (collisions sr)))
            ++ (   termLineFor TermExistingConstructorCollision
               <$> toList (termExistingConstructorCollisions sr)
               )
            ++ (   termLineFor ConstructorExistingTermCollision
               <$> toList (constructorExistingTermCollisions sr)
               )
            ++ (   termLineFor BlockedDependency
               <$> toList (terms (defsWithBlockedDependencies sr))
               )
      in
        header
        <> "\n\n"
        <> P.hiBlack "  Reason"
        <> "\n"
        <> P.indentN 2 (P.linesNonEmpty [typeMsgs, termMsgs])
        <> "\n\n"
        <> P.indentN
             2
             (P.column2 [("Tip:", "Use `help filestatus` to learn more.")])
    dups = Set.toList (SC.terms (duplicates sr) <> SC.types (duplicates sr))
    more i =
      "... "
        <> P.bold (P.shown i)
        <> P.hiBlack " more."
        <> "Try moving these below the `---` \"fold\" in your file."
  in
    P.sepNonEmpty
      "\n\n"
      [ if null (terms (duplicates sr)) && null (types (duplicates sr))
        then mempty
        else
          (if isPast
              then "⊡ Ignored previously added definitions: "
              else "⊡ Previously added definitions will be ignored: "
            )
            <> P.indentNAfterNewline
                 2
                 (P.wrap $ P.excerptSep' (Just 7)
                                         more
                                         " "
                                         (P.hiBlack . prettyVar <$> dups)
                 )
      , okToAdd (adds sr)
      , okToUpdate (updates sr)
      , notOks
        (P.red "These definitions failed:")
        (P.wrap $ P.red "These definitions would fail on `add` or `update`:")
        sr
      ]

isOk :: Ord v => SlurpResult v -> Bool
isOk SlurpResult {..} =
  SC.isEmpty collisions &&
  SC.isEmpty conflicts &&
  Set.null termExistingConstructorCollisions &&
  Set.null constructorExistingTermCollisions &&
  SC.isEmpty defsWithBlockedDependencies

isAllDuplicates :: Ord v => SlurpResult v -> Bool
isAllDuplicates SlurpResult {..} =
  emptyIgnoringConstructors adds &&
  emptyIgnoringConstructors updates &&
  emptyIgnoringConstructors extraDefinitions &&
  SC.isEmpty collisions &&
  SC.isEmpty conflicts &&
  Map.null typeAlias &&
  Map.null termAlias &&
  Set.null termExistingConstructorCollisions &&
  Set.null constructorExistingTermCollisions &&
  emptyIgnoringConstructors defsWithBlockedDependencies
    where
      emptyIgnoringConstructors :: SlurpComponent v -> Bool
      emptyIgnoringConstructors SlurpComponent{types, terms} =
        null types && null terms

-- stack repl
--
-- λ> import Unison.Util.Pretty
-- λ> import Unison.Codebase.Editor.SlurpResult
-- λ> putStrLn $ toANSI 80 ex
ex :: P.Pretty P.ColorText
ex = P.indentN 2 $ P.lines ["",
  P.green "▣ I've added these definitions: ", "",
  P.indentN 2 . P.column2 $ [("a", "Nat"), ("map", "(a -> b) -> [a] -> [b]")],
  "",
  P.green "▣ I've updated these definitions: ", "",
  P.indentN 2 . P.column2 $ [("c", "Nat"), ("flatMap", "(a -> [b]) -> [a] -> [b]")],
  "",
  P.wrap $ P.red "x" <> P.bold "These definitions couldn't be added:", "",
  P.indentN 2 $
    P.lines [
      P.column2 [(P.hiBlack
                  "Reason for failure    Symbol ",     P.hiBlack "Type"),
                 ("ctor/term collision   foo ",        "Nat"),
                 ("failed dependency     zoot ",       "[a] -> [a] -> [a]"),
                 ("term/ctor collision   unique type Foo ", "f x")],
      "", "Tip: use `help filestatus` to learn more."
    ],
  "",
  "⊡ Ignoring previously added definitions: " <>
     P.indentNAfterNewline 2 (
     P.hiBlack (P.wrap $ P.sep " " ["zonk", "anotherOne", "List.wrangle", "oatbag", "blarg", "mcgee", P.group "ability Woot"])),
  ""
  ]
