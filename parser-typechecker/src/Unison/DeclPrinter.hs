{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DeclPrinter where

import           Data.Maybe                     ( fromMaybe )
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                , toDataDecl
                                                )
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

prettyEffectDecl
  :: Var v
  => PrettyPrintEnv
  -> Reference
  -> HashQualified
  -> EffectDeclaration' v a
  -> Pretty ColorText
prettyEffectDecl ppe r name = prettyGADT ppe r name . toDataDecl

prettyGADT
  :: Var v
  => PrettyPrintEnv
  -> Reference
  -> HashQualified
  -> DataDeclaration' v a
  -> Pretty ColorText
prettyGADT env r name dd = P.hang header . P.lines $ constructor <$> zip
  [0 ..]
  (DD.constructors' dd)
 where
  constructor (n, (_, _, t)) =
    prettyPattern env r name n
      <>       " :"
      `P.hang` TypePrinter.pretty env (-1) t
  header =
    P.sep " " (prettyEffectHeader name (DD.EffectDeclaration dd) : (P.text . Var.name <$> DD.bound dd))
      <> " where"

prettyPattern
  :: PrettyPrintEnv -> Reference -> HashQualified -> Int -> Pretty ColorText
prettyPattern env r namespace n = prettyHashQualified
  ( HQ.stripNamespace (fromMaybe "" $ Name.toText <$> HQ.toName namespace)
  $ PPE.patternName env r n
  )

prettyDataDecl
  :: Var v
  => PrettyPrintEnv
  -> Reference
  -> HashQualified
  -> DataDeclaration' v a
  -> Pretty ColorText
prettyDataDecl env r name dd =
  (header <>) . P.sep (" | " `P.orElse` "\n  | ") $ constructor <$> zip
    [0 ..]
    (DD.constructors' dd)
 where
  constructor (n, (_, _, (Type.ForallsNamed' _ t))) = constructor' n t
  constructor (n, (_, _, t)                       ) = constructor' n t
  constructor' n t = case Type.unArrows t of
    Nothing -> prettyPattern env r name n
    Just ts -> P.group . P.hang' (prettyPattern env r name n) "      "
             $ P.spaced (TypePrinter.pretty env 10 <$> init ts)
  header =
    P.sep " " (prettyDataHeader name dd : (P.text . Var.name <$> DD.bound dd))
      <> (" = " `P.orElse` "\n  = ")

prettyModifier :: DD.Modifier -> Pretty ColorText
prettyModifier DD.Structural = mempty
prettyModifier (DD.Unique uid) =
  P.bold "unique" <> P.hiBlack ("[" <> P.text uid <> "] ")

prettyDataHeader :: HashQualified -> DD.DataDeclaration' v a -> Pretty ColorText
prettyDataHeader name dd =
  prettyModifier (DD.modifier dd) <> P.bold "type " <> prettyHashQualified name

prettyEffectHeader :: HashQualified -> DD.EffectDeclaration' v a -> Pretty ColorText
prettyEffectHeader name ed =
  prettyModifier (DD.modifier (DD.toDataDecl ed)) <> P.bold "ability " <> prettyHashQualified name
