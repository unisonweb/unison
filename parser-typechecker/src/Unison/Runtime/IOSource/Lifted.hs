{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Runtime.IOSource.Lifted
  ( -- copyrightHolderRef,
    -- authorRef,
    -- guidRef,
    -- isPropagatedValue,
    -- isPropagatedReference,
    codeLookupM,
    -- pattern Doc2Word,
    -- pattern Doc2Code,
    -- pattern Doc2CodeBlock,
    -- pattern Doc2Bold,
    -- pattern Doc2Italic,
    -- pattern Doc2Strikethrough,
    -- pattern Doc2Style,
    -- pattern Doc2Anchor,
    -- pattern Doc2Blockquote,
    -- pattern Doc2Blankline,
    -- pattern Doc2Linebreak,
    -- pattern Doc2SectionBreak,
    -- pattern Doc2Tooltip,
    -- pattern Doc2Aside,
    -- pattern Doc2Callout,
    -- pattern Doc2Table,
    -- pattern Doc2Folded,
    -- pattern Doc2Paragraph,
    -- pattern Doc2BulletedList,
    -- pattern Doc2NumberedList,
    -- pattern Doc2Section,
    -- pattern Doc2NamedLink,
    -- pattern Doc2Image,
    -- pattern Doc2Special,
    -- pattern Doc2Join,
    -- pattern Doc2UntitledSection,
    -- pattern Doc2Column,
    -- pattern Doc2Group,
  )
where

import Control.Lens (view, _1)
import Control.Monad.Morph (hoist)
import Data.List (elemIndex, genericIndex)
import qualified Data.Map as Map
import qualified Language.Haskell.TH.Syntax as TH
import Text.RawString.QQ (r)
import qualified Unison.Builtin as Builtin
import Unison.Codebase.CodeLookup (CodeLookup (..))
import qualified Unison.Codebase.CodeLookup.Util as CL
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.DataDeclaration as DD
import qualified Unison.DataDeclaration.ConstructorId as DD
import Unison.FileParsers (parseAndSynthesizeFile)
import qualified Unison.NamesWithHistory as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.Reference as R
import qualified Unison.Result as Result
import qualified Unison.Runtime.IOSource as IOSource
import Unison.Symbol
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Term as Term
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import qualified Unison.Var as Var

deriving instance TH.Lift (TypecheckedUnisonFile v a)

deriving instance TH.Lift Symbol

deriving instance TH.Lift Ann

-- codeLookup :: CodeLookup Symbol Identity Ann
-- codeLookup = CL.fromTypecheckedUnisonFile $(TH.lift IOSource.typecheckedFile)

codeLookupM :: Applicative m => CodeLookup Symbol m Ann
codeLookupM = hoist (pure . runIdentity) codeLookup
