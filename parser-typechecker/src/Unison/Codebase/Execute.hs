{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Execute a computation of type '{IO} () that has been previously added to
-- the codebase, without setting up an interactive environment.
--
-- This allows one to run standalone applications implemented in the Unison
-- language.
module Unison.Codebase.Execute where

import Control.Exception (finally)
import System.Exit (die)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.Init as Init
import Unison.Codebase.MainTerm (getMainTerm)
import qualified Unison.Codebase.MainTerm as MainTerm
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Symbol (Symbol)

execute ::
  Codebase.Codebase IO Symbol Ann ->
  Runtime Symbol ->
  String ->
  IO ()
execute codebase runtime mainName =
  (`finally` Runtime.terminate runtime) $ do
    root <- Codebase.getRootBranch codebase
    let parseNames = Names.makeAbsolute (Branch.toNames (Branch.head root))
        loadTypeOfTerm = Codebase.getTypeOfTerm codebase
    let mainType = Runtime.mainType runtime
    mt <- getMainTerm loadTypeOfTerm parseNames mainName mainType
    case mt of
      MainTerm.NotAFunctionName s -> die ("Not a function name: " ++ s)
      MainTerm.NotFound s -> die ("Not found: " ++ s)
      MainTerm.BadType s _ -> die (s ++ " is not of type '{IO} ()")
      MainTerm.Success _ tm _ -> do
        void . Init.withOpenCodebase SC.init "bundled" "./bundled" $ \bundledCodebase -> do
          let codeLookup = Codebase.toCodeLookup codebase <> Codebase.toCodeLookup bundledCodebase
              ppe = PPE.empty
          void $ Runtime.evaluateTerm codeLookup ppe runtime tm
