module Unison.Suffixifier
  ( Suffixifier (..),
    dontSuffixify,
    suffixifyByName,
    suffixifyByHash,
  )
where

import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names (..))
import Unison.Names3 (Names3 (..))
import Unison.Util.Relation qualified as Relation

-- | A "suffixifier" shortens a name, e.g. maps "base.List.map" to "List.map".
data Suffixifier = Suffixifier
  { suffixifyTerm :: Name -> Name,
    suffixifyType :: Name -> Name
  }

-- Suffixification logic implemented below: if something has a name in the set (local + direct deps), then ignore
-- indirect deps entirely (matching the behavior of the parser). That is, if we have
--
--   * lib.base_1.foo = #foo1
--   * lib.something.lib.base_2.foo = #foo2
--
-- then we want to be able to refer to `lib.base_1.foo` by the suffix `foo`.
--
-- Otherwise, suffixify against the entire set (local + direct deps + indirect deps).

dontSuffixify :: Suffixifier
dontSuffixify =
  Suffixifier id id

suffixifyByName :: Names3 -> Suffixifier
suffixifyByName names =
  Suffixifier {suffixifyTerm, suffixifyType}
  where
    suffixifyTerm :: Name -> Name
    suffixifyTerm name
      | Relation.memberDom name localPlusDirectDeps.terms = Name.suffixifyByName name localPlusDirectDeps.terms
      | otherwise = Name.suffixifyByName name localPlusDirectDepsPlusIndirectDeps.terms

    suffixifyType :: Name -> Name
    suffixifyType name
      | Relation.memberDom name localPlusDirectDeps.types = Name.suffixifyByName name localPlusDirectDeps.types
      | otherwise = Name.suffixifyByName name localPlusDirectDepsPlusIndirectDeps.types

    localPlusDirectDeps :: Names
    localPlusDirectDeps =
      names.local <> names.directDeps

    localPlusDirectDepsPlusIndirectDeps :: Names
    localPlusDirectDepsPlusIndirectDeps =
      localPlusDirectDeps <> names.indirectDeps

suffixifyByHash :: Names3 -> Suffixifier
suffixifyByHash names =
  Suffixifier {suffixifyTerm, suffixifyType}
  where
    suffixifyTerm :: Name -> Name
    suffixifyTerm name
      | Relation.memberDom name localPlusDirectDeps.terms = Name.suffixifyByHash name localPlusDirectDeps.terms
      | otherwise = Name.suffixifyByHash name localPlusDirectDepsPlusIndirectDeps.terms

    suffixifyType :: Name -> Name
    suffixifyType name
      | Relation.memberDom name localPlusDirectDeps.types = Name.suffixifyByHash name localPlusDirectDeps.types
      | otherwise = Name.suffixifyByHash name localPlusDirectDepsPlusIndirectDeps.types

    localPlusDirectDeps :: Names
    localPlusDirectDeps =
      names.local <> names.directDeps

    localPlusDirectDepsPlusIndirectDeps :: Names
    localPlusDirectDepsPlusIndirectDeps =
      localPlusDirectDeps <> names.indirectDeps
