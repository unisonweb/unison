module Unison.Test.Codebase.Serialization where

import           EasyTest
import           Control.Monad                    ( void )
import           Control.Monad.IO.Class           ( liftIO )
import qualified Data.Map                         as Map
import           Data.Text                        ( unpack )
import           Data.Text.IO                     ( readFile )
import           System.FilePath                  ( joinPath, splitPath )
import qualified Unison.ABT                       as ABT
import           Unison.Codebase.Serialization    ( getFromBytes, putBytes )
import qualified Unison.Codebase.Serialization.V1 as V1
import           Unison.DataDeclaration           ( DataDeclaration
                                                  , DataDeclaration'
                                                  , EffectDeclaration
                                                  , EffectDeclaration'
                                                  )
import           Unison.Parser                    ( Ann )
import           Unison.Reference                 ( Reference )
import           Unison.Symbol                    ( Symbol )
import qualified Unison.Term                      as Term
import           Unison.Term                      ( AnnotatedTerm, Term )
import           Unison.Test.Common               ( parseAndSynthesizeAsFile )
import qualified Unison.Test.Typechecker          as Typechecker
import           Unison.Type                      ( Type )
import qualified Unison.UnisonFile                as UF
import qualified Unison.Var                       as Var

test :: Test ()
test = do
  files <- liftIO Typechecker.shouldPassNow
  scope "serialization" $ tests (makeTest <$> files)

makeTest
  :: FilePath -> Test ()
makeTest filepath = scope shortName $ do
  source <- io $ unpack <$> Data.Text.IO.readFile filepath
  let r = Typechecker.decodeResult source $ parseAndSynthesizeAsFile [] shortName source
  case r of
    Right uf -> tests . concat $
      [ map testDataDeclaration (Map.toList $ UF.dataDeclarations' uf)
      , map testEffectDeclaration (Map.toList $ UF.effectDeclarations' uf)
      , map testTerm (Map.toList $ UF.hashTerms uf)
      ]
      where
        putUnit :: Monad m => () -> m ()
        putUnit () = pure ()
        getUnit :: Monad m => m ()
        getUnit = pure ()
        testDataDeclaration :: (Symbol, (Reference, DataDeclaration' Symbol Ann)) -> Test ()
        testDataDeclaration (name, (_, decl)) = scope (Var.nameStr name) $
          let decl' :: DataDeclaration Symbol
              decl' = void decl
              bytes = putBytes (V1.putDataDeclaration V1.putSymbol putUnit) decl'
              decl'' = getFromBytes (V1.getDataDeclaration V1.getSymbol getUnit) bytes
          in expectEqual decl'' (Just decl')
        testEffectDeclaration :: (Symbol, (Reference, EffectDeclaration' Symbol Ann)) -> Test ()
        testEffectDeclaration (name, (_, decl)) = scope (Var.nameStr name) $
          let decl' :: EffectDeclaration Symbol
              decl' = void decl
              bytes = putBytes (V1.putEffectDeclaration V1.putSymbol putUnit) decl'
              decl'' = getFromBytes (V1.getEffectDeclaration V1.getSymbol getUnit) bytes
          in expectEqual decl'' (Just decl')
        testTerm :: (Symbol, (Reference, AnnotatedTerm Symbol Ann, Type Symbol Ann)) -> Test ()
        testTerm (name, (_, tm, tp)) = scope (Var.nameStr name) $
          let tm' :: Term Symbol
              tm' = Term.amap (const ()) tm
              tp' :: Type Symbol ()
              tp' = ABT.amap (const ()) tp
              tmBytes = putBytes (V1.putTerm V1.putSymbol putUnit) tm'
              tpBytes = putBytes (V1.putType V1.putSymbol putUnit) tp'
              tm'' = getFromBytes (V1.getTerm V1.getSymbol getUnit) tmBytes
              tp'' = getFromBytes (V1.getType V1.getSymbol getUnit) tpBytes
          in tests
            [ scope "type" $ expectEqual tp'' (Just tp')
            , scope "term" $ expectEqual tm'' (Just tm')
            ]
    Left e -> crash e
  where shortName = joinPath . drop 1 . splitPath $ filepath