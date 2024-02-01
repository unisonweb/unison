-- | Execute a computation of type '{IO} () that has been previously added to
-- the codebase, without setting up an interactive environment.
--
-- This allows one to run standalone applications implemented in the Unison
-- language.
module Unison.Codebase.Execute where

import Control.Exception (finally)
import Control.Monad.Except
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.MainTerm (getMainTerm)
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Runtime (Runtime)
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Symbol (Symbol)
import Unison.Util.Pretty qualified as P

execute ::
  Codebase.Codebase IO Symbol Ann ->
  Runtime Symbol ->
  Text ->
  IO (Either Runtime.Error ())
execute codebase runtime mainName =
  (`finally` Runtime.terminate runtime) . runExceptT $ do
    root <- liftIO $ Codebase.getRootBranch codebase
    let parseNames = Names.makeAbsolute (Branch.toNames (Branch.head root))
        loadTypeOfTerm = Codebase.getTypeOfTerm codebase
    let mainType = Runtime.mainType runtime
    mt <- liftIO $ Codebase.runTransaction codebase $ getMainTerm loadTypeOfTerm parseNames mainName mainType
    case mt of
      MainTerm.NotAFunctionName s -> throwError ("Not a function name: " <> P.text s)
      MainTerm.NotFound s -> throwError ("Not found: " <> P.text s)
      MainTerm.BadType s _ -> throwError (P.text s <> " is not of type '{IO} ()")
      MainTerm.Success _ tm _ -> do
        let codeLookup = Codebase.toCodeLookup codebase
            ppe = PPE.empty
        (liftIO $ Runtime.evaluateTerm codeLookup ppe runtime tm) >>= \case
          Left err -> throwError err
          Right _ -> pure ()
