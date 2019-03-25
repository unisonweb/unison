{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DeclPrinter where

import           Data.Maybe                     ( fromMaybe )
import           Unison.DataDeclaration         ( DataDeclaration' )
import qualified Unison.DataDeclaration        as DD
import           Unison.HashQualified           ( HashQualified )
import qualified Unison.HashQualified          as HQ
import qualified Unison.Name                   as Name
import           Unison.NamePrinter             ( prettyHashQualified )
import           Unison.PrettyPrintEnv          ( PrettyPrintEnv )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Type                   as Type
import qualified Unison.TypePrinter            as TypePrinter
import           Unison.Util.Pretty             ( Pretty
                                                , ColorText
                                                )
import qualified Unison.Util.Pretty            as P
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var

prettyDataDecl
  :: Var v
  => PrettyPrintEnv
  -> Reference
  -> HashQualified
  -> DataDeclaration' v a
  -> Pretty ColorText
prettyDataDecl env r name dd =
  P.hang header . P.sep (" |" <> P.softbreak) $ constructor <$> zip
    [0 ..]
    (DD.constructors' dd)
 where
  constructor (n, (_, _, (Type.ForallsNamed' _ t))) = constructor' n t
  constructor (n, (_, _, t)                       ) = constructor' n t
  constructor' n t =
    P.sep " "
      $ prettyHashQualified
          ( HQ.stripNamespace (fromMaybe "" $ Name.toText <$> HQ.toName name)
          $ PPE.patternName env r n
          )
      : (TypePrinter.pretty env 10 <$> case Type.unArrows t of
          Nothing -> mempty
          Just ts -> init ts
        )
  header =
    P.sep " " (prettyDataHeader name : (P.text . Var.name <$> DD.bound dd))
      <> " ="

prettyDataHeader :: HashQualified -> Pretty ColorText
prettyDataHeader name = P.bold "type " <> prettyHashQualified name

prettyEffectHeader :: HashQualified -> Pretty ColorText
prettyEffectHeader name = P.bold "ability " <> prettyHashQualified name
