--{-# LANGUAGE PatternSynonyms #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE OverloadedStrings #-}

module Unison.TermPrinter where

--import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
--import           Data.Foldable (fold)
--import           Data.Maybe (isJust)
import           Unison.Reference (Reference(..))
import           Unison.Term
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

pretty :: Var v => (Reference -> Text) -> Int -> AnnotatedTerm v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
pretty n _ = \case
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r)
  Cycle' _ _   -> l"todo"
  Abs' _       -> l"todo"
  Ann' _ _     -> l"todo"
  Int64' n     -> l $ show n   -- todo
  UInt64' n    -> l $ show n   -- todo
  Float' n     -> l $ show n   -- todo
  Boolean' b   -> l $ show b   -- todo
  Text' s      -> l $ show s   -- todo
  Blank' _     -> l"todo"   -- todo
  Constructor' ref n -> l $ (show ref <> show n)   -- todo
  Request' ref n -> l $ (show ref <> show n)   -- todo
  Handle' h body -> l $ (show h <> show body)   -- todo
  Apps' f args -> l $ (show f <> show args)   -- todo
  Vector' xs   -> l $ show xs   -- todo
  If' cond t f -> l $ (show cond <> show t <> show f)   -- todo
  And' x y     -> l $ (show x <> show y)   -- todo
  Or' x y      -> l $ (show x <> show y)   -- todo
  LamNamed' v body -> l $ (show v <> show body)   -- todo
  LetRecNamed' bs e -> l $ (show bs <> show e)   -- todo
  Lets' bs e -> l $ (show bs <> show e)   -- todo
  Match' scrutinee branches -> l $ (show scrutinee <> show branches)   -- todo
  _ -> l"error"

  {-App' (Ref' (Builtin "Sequence")) x -> PP.Group $ l"[" <> pretty n 0 x <> l"]"
  Tuple' [x]   -> parenNest (p >= 10) $ PP.Group $ l"Pair" <> b" " <> pretty n 10 x <> b" " <> l"()"
  Tuple' xs    -> parenNest True $ commaList xs
  Apps' f xs   -> parenNest (p >= 10) $ PP.Group $ pretty n 9 f <> appArgs xs
  Effect1' e t -> parenNest (p >= 10) $ pretty n 9 e <> l" " <> pretty n 10 t
  Effects' es  -> effects (Just es)
  ForallNamed' v body ->
    if (p <= 0)
    then pretty n p body
    else paren True $ l"∀ " <> l (Text.unpack (Var.name v)) <> l". " <> PP.Nest " " (PP.Group $ pretty n 0 body)
  --TODO undo generalizeEffects before printing - see Type.ungeneralizeEffects
  EffectfulArrows' (Ref' (Builtin "()")) rest -> arrows True True rest
  EffectfulArrows' fst rest -> parenNest (p >= 0) $ pretty n 0 fst <> arrows False False rest
  _ -> l"error" -}
  where {-commaList xs = fold $ intersperse (l"," <> b" ") (map (pretty n 0) xs)

        appArgs (x : xs) = b" " <> pretty n 10 x <> appArgs xs
        appArgs [] = Empty

        paren True s = PP.Group $ l"(" <> s <> l")"
        paren False s = PP.Group s

        parenNest useParen contents = PP.Nest " " $ paren useParen contents -}
        l = Literal
        --b = Breakable

pretty' :: Var v => (Reference -> Text) -> AnnotatedTerm v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t


{-
pattern Lam' subst <- ABT.Tm' (Lam (ABT.Abs' subst))
pattern Builtin' r <- (ABT.out -> ABT.Tm (Ref (Builtin r)))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern RequestOrCtor' ref n <- (unReqOrCtor -> Just (ref, n))
pattern Let1' b subst <- (unLet1 -> Just (b, subst))
pattern Let1Named' v b e <- (ABT.Tm' (Let b (ABT.out -> ABT.Abs v e)))
pattern LetRecNamed' bs e <- (unLetRecNamed -> Just (bs,e))
pattern LetRecNamedAnnotated' ann bs e <- (unLetRecNamedAnnotated -> Just (ann, bs,e))
-}
