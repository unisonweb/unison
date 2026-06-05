module Unison.TypeTagRepr
  ( TypeTagRepr (..),
    typeToRepr,
    typeTagRefs,
    updateTypeTagRepr,
    renderTypeTagRepr,
  )
where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Unison.ABT qualified as ABT
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Reference (TypeReference, Id' (..), pattern Builtin, pattern DerivedId)
import Unison.Type qualified as Type

data TypeTagRepr
  = TTRef !TypeReference
  | TTApp !TypeTagRepr !TypeTagRepr
  | TTArrow !TypeTagRepr !TypeTagRepr
  | TTEffect ![TypeTagRepr] !TypeTagRepr
  deriving (Eq, Ord, Show, Generic)

updateTypeTagRepr :: Map.Map TypeReference TypeReference -> TypeTagRepr -> TypeTagRepr
updateTypeTagRepr m = go
  where
    go (TTRef r) = TTRef (Map.findWithDefault r r m)
    go (TTApp f x) = TTApp (go f) (go x)
    go (TTArrow i o) = TTArrow (go i) (go o)
    go (TTEffect es t) = TTEffect (map go es) (go t)

typeTagRefs :: TypeTagRepr -> [TypeReference]
typeTagRefs = go
  where
    go (TTRef r) = [r]
    go (TTApp f x) = go f ++ go x
    go (TTArrow i o) = go i ++ go o
    go (TTEffect es t) = concatMap go es ++ go t

typeToRepr :: (Ord v) => Type.Type v a -> TypeTagRepr
typeToRepr = go
  where
    go ty = case ty of
      Type.Ref' r -> TTRef r
      Type.App' f x -> TTApp (go f) (go x)
      Type.Arrow' i o -> TTArrow (go i) (go o)
      Type.Effect1' e t -> TTEffect (goEffects e) (go t)
      Type.Effects' es -> case es of
        [e] -> go e
        _ -> TTRef (error "TypeTagRepr: bare Effects node in monomorphic type")
      Type.ForallNamed' _ body -> go body
      Type.Ann' t _ -> go t
      _ -> case ABT.out ty of
        ABT.Tm f -> case f of
          Type.Ref r -> TTRef r
          Type.App a b -> TTApp (go a) (go b)
          Type.Arrow a b -> TTArrow (go a) (go b)
          Type.Effect e t -> TTEffect (goEffects e) (go t)
          _ -> TTRef (error "TypeTagRepr: unexpected type constructor in monomorphic type")
        _ -> TTRef (error "TypeTagRepr: unexpected ABT node in monomorphic type")

    goEffects ty = case ty of
      Type.Effects' es -> map go es
      _ -> [go ty]

renderTypeTagRepr :: TypeTagRepr -> Text
renderTypeTagRepr = renderOuter
  where
    renderOuter = \case
      TTArrow i o -> renderArrowArg i <> " -> " <> renderOuter o
      TTEffect es t -> "{" <> Text.intercalate ", " (map renderOuter es) <> "} " <> renderOuter t
      other -> renderApp other

    renderArrowArg = \case
      TTArrow i o -> "(" <> renderArrowArg i <> " -> " <> renderOuter o <> ")"
      other -> renderApp other

    renderApp = \case
      TTApp f x -> renderApp f <> " " <> renderAtom x
      other -> renderAtom other

    renderAtom = \case
      TTRef r -> renderRef r
      TTArrow i o -> "(" <> renderArrowArg i <> " -> " <> renderOuter o <> ")"
      TTEffect es t -> "({" <> Text.intercalate ", " (map renderOuter es) <> "} " <> renderOuter t <> ")"
      TTApp f x -> "(" <> renderApp f <> " " <> renderAtom x <> ")"

    renderRef (Builtin name) = name
    renderRef (DerivedId (Id h i)) =
      "#" <> Text.take 8 (Hash.toBase32HexText h) <> if i == 0 then "" else "#" <> Text.pack (show i)
