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
import Control.Monad.Except
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as Branch
import Unison.Codebase.MainTerm (getMainTerm)
import qualified Unison.Codebase.MainTerm as MainTerm
import Unison.Codebase.Runtime (Runtime)
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P

execute ::
  Codebase.Codebase IO Symbol Ann ->
  Runtime Symbol ->
  String ->
  IO (Either Runtime.Error ())
execute codebase runtime mainName = do
  (`finally` Runtime.terminate runtime) $
    runExceptT $ do
      root <-
        liftIO (Codebase.getRootBranch codebase) >>= \case
          Right r -> pure r
          Left Codebase.NoRootBranch ->
            throwError "Couldn't identify a root namespace."
          Left (Codebase.CouldntLoadRootBranch h) ->
            throwError ("Couldn't load root branch " <> P.shown h)
          Left (Codebase.CouldntParseRootBranch h) ->
            throwError ("Couldn't parse root branch head " <> P.shown h)
      let parseNames = Names.makeAbsolute (Branch.toNames (Branch.head root))
          loadTypeOfTerm = Codebase.getTypeOfTerm codebase
      let mainType = Runtime.mainType runtime
      mt <- liftIO $ getMainTerm loadTypeOfTerm parseNames mainName mainType
      case mt of
        MainTerm.NotAFunctionName s -> throwError ("Not a function name: " <> P.string s)
        MainTerm.NotFound s -> throwError ("Not found: " <> P.string s)
        MainTerm.BadType s _ -> throwError (P.string s <> " is not of type '{IO} ()")
        MainTerm.Success _ tm _ -> do
          let codeLookup = Codebase.toCodeLookup codebase
              ppe = PPE.PrettyPrintEnv (const Nothing) (const Nothing)
          void . ExceptT $ Runtime.evaluateTerm codeLookup ppe runtime tm
