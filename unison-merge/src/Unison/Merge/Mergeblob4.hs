module Unison.Merge.Mergeblob4
  ( Mergeblob4 (..),
    makeMergeblob4,
  )
where

import Unison.Merge.Mergeblob3 (Mergeblob3 (..))
import Unison.Names (Names (..))
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser (ParsingEnv (..), UniqueName)
import Unison.Syntax.Parser qualified as Parser
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Defns (Defns (..))
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation

data Mergeblob4 = Mergeblob4
  { dependencies :: Set Reference,
    file :: UnisonFile Symbol Ann
  }

makeMergeblob4 :: Mergeblob3 -> UniqueName -> Either (Parser.Err Symbol) Mergeblob4
makeMergeblob4 blob uniqueName = do
  let stageOneNames =
        Names (Relation.fromMap blob.stageOne.terms) (Relation.fromMap blob.stageOne.types) <> blob.libdeps

      parsingEnv =
        ParsingEnv
          { uniqueNames = uniqueName,
            -- The codebase names are disjoint from the file names, i.e. there aren't any things that
            -- would be classified as an update upon parsing. So, there's no need to try to look up any
            -- existing unique type GUIDs to reuse.
            uniqueTypeGuid = \_ -> Identity Nothing,
            names = stageOneNames
          }
  file <- runIdentity (Parsers.parseFile "<merge>" (Pretty.toPlain 80 blob.unparsedFile) parsingEnv)
  Right
    Mergeblob4
      { dependencies = UnisonFile.dependencies file,
        file
      }
