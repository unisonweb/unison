module Unison.PartialDeclNameLookup
  ( PartialDeclNameLookup (..),
    Unison.PartialDeclNameLookup.empty,
    expectDeclName,
    expectConstructorNames,
    toDeclNameLookup,
  )
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Name (Name)
import Unison.Prelude

-- | Like a @DeclNameLookup@, but "partial" / more lenient - because we don't require the LCA of a merge to have a full
-- @DeclNameLookup@.
data PartialDeclNameLookup = PartialDeclNameLookup
  { constructorToDecl :: !(Map Name Name),
    declToConstructors :: !(Map Name [Maybe Name])
  }
  deriving stock (Generic)

empty :: PartialDeclNameLookup
empty =
  PartialDeclNameLookup Map.empty Map.empty

expectDeclName :: (HasCallStack) => PartialDeclNameLookup -> Name -> Name
expectDeclName PartialDeclNameLookup {constructorToDecl} x =
  case Map.lookup x constructorToDecl of
    Nothing -> error (reportBug "E874908" ("Expected constructor name key " <> show x <> " in partial decl name lookup"))
    Just y -> y

expectConstructorNames :: (HasCallStack) => PartialDeclNameLookup -> Name -> [Maybe Name]
expectConstructorNames PartialDeclNameLookup {declToConstructors} x =
  case Map.lookup x declToConstructors of
    Nothing -> error (reportBug "E800097" ("Expected decl name key " <> show x <> " in partial decl name lookup"))
    Just y -> y

-- | Turn a partial decl name lookup into a total decl name lookup.
--
-- This isn't very sensible, but in certain cases we do find ourselves in the unfortunate circumstance of needing to
-- render a type declaration that doesn't have a name for one or more of its constructors (as when rendering the LCA
-- file of a difftool or mergetool, since we do allow the LCA to have missing constructors).
--
-- This function just assigns bogus names like "Unnamed" for rendering.
toDeclNameLookup :: (Text -> Name) -> PartialDeclNameLookup -> DeclNameLookup
toDeclNameLookup unsafeParseText partialDeclNameLookup =
  DeclNameLookup
    { constructorToDecl = partialDeclNameLookup.constructorToDecl,
      declToConstructors =
        makeTotal <$> partialDeclNameLookup.declToConstructors
    }
  where
    makeTotal :: [Maybe Name] -> [Name]
    makeTotal names0 =
      case sequence names0 of
        Just names -> names
        Nothing ->
          snd $
            List.mapAccumL
              makeSomethingUp
              (foldMap (maybe Set.empty Set.singleton) names0)
              names0

    makeSomethingUp :: Set Name -> Maybe Name -> (Set Name, Name)
    makeSomethingUp taken = \case
      Just name -> (taken, name)
      Nothing ->
        let name = freshen 0 "Unnamed"
            !taken1 = Set.insert name taken
         in (taken1, name)
      where
        freshen :: Int -> Text -> Name
        freshen i name0
          | Set.member name taken = freshen (i + 1) name0
          | otherwise = name
          where
            name :: Name
            name =
              unsafeParseText (name0 <> if i == 0 then Text.empty else Text.pack (show i))
