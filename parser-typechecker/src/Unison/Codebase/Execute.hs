{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute a computation of type '{IO} () that has been previously added to
-- the codebase, without setting up an interactive environment.
--
-- This allows one to run standalone applications implemented in the Unison
-- language.

module Unison.Codebase.Execute where

import Unison.Prelude

import           Unison.Codebase.MainTerm      ( getMainTerm )
import qualified Unison.Codebase.MainTerm      as MainTerm
import qualified Unison.Codebase               as Codebase
import Unison.Parser.Ann (Ann)
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.Codebase.Runtime       ( Runtime )
import           Unison.Var                    ( Var )
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import           System.Exit (die)
import           Control.Exception (finally)
import qualified Unison.Names as Names

execute
  :: Var v
  => Codebase.Codebase IO v Ann
  -> Runtime v
  -> String
  -> IO ()
execute codebase runtime mainName =
  (`finally` Runtime.terminate runtime) $ do
    root <- Codebase.getRootBranch codebase >>= \case
      Right r -> pure r
      Left Codebase.NoRootBranch ->
        die "Couldn't identify a root namespace."
      Left (Codebase.CouldntLoadRootBranch h) ->
        die ("Couldn't load root branch " ++ show h)
      Left (Codebase.CouldntParseRootBranch h) ->
        die ("Couldn't parse root branch head " ++ show h)
    let parseNames = Names.makeAbsolute (Branch.toNames (Branch.head root))
        loadTypeOfTerm = Codebase.getTypeOfTerm codebase
    let mainType = Runtime.mainType runtime
    mt <- getMainTerm loadTypeOfTerm parseNames mainName mainType
    case mt of
      MainTerm.NotAFunctionName s -> die ("Not a function name: " ++ s)
      MainTerm.NotFound s -> die ("Not found: " ++ s)
      MainTerm.BadType s _ -> die (s ++ " is not of type '{IO} ()")
      MainTerm.Success _ tm _ -> do
        let codeLookup = Codebase.toCodeLookup codebase
            ppe = PPE.PrettyPrintEnv (const Nothing) (const Nothing)
        void $ Runtime.evaluateTerm codeLookup ppe runtime tm
