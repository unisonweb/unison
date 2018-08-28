module Unison.Typechecker.Extractor3 where

-- import           Control.Monad              (ap, liftM)
-- import           Data.Foldable
-- import qualified Data.List                  as List
import qualified Unison.Typechecker.Context as C
-- import           Unison.Util.Monoid         (whenM)

data SubseqExtractor v loc a =
  SubseqExtractor { run :: C.Note v loc -> [Ranged a] }

data Ranged a = Ranged a Range

data Range = Range { startMin :: Maybe Int
                   , startMax :: Maybe Int
                   , endMin   :: Maybe Int
                   , endMax   :: Maybe Int
                   }

-- rangeAdjacent :: Range -> Range -> Bool
-- a `rangeAdjacent` b = startMin

-- instance Functor (SubseqExtractor v loc) where
--   fmap = liftM
--
-- instance Applicative (SubseqExtractor v loc) where
--   pure = return
--   (<*>) = ap
--
-- instance Monad (SubseqExtractor v loc) where
--   return a = SubseqExtractor $ \_ -> [Pure a]
--   xa >>= f = SubseqExtractor $ \note ->
--     let as = run xa note in do
--       ra <- as
--       case ra of
--         Pure a -> run (f a) note
--         ra -> let bs = run (f a) note in do
--           rb <- bs
--             case rb of
--               Pure b -> [Pure b]
--               rb -> whenM () $
--                       (pure (Closed b startA endB))
--               -- OpenR b startB upperB ->
--               --   whenM (startB == endA + 1) (pure (OpenR b startA upperB))
--               -- OpenLR b startLowerBoundB endUpperBoundB ->
--               --   whenM (startLowerBoundB )
--               -- OpenLLRR b startLowerBoundB startUpperBoundB
--               --            endUowerBoundB endUpperBoundB ->
--               --   -- whenM (lowerBoundB <= endA + 1 && upperBoundB > endA + 1)
--               --   whenM (startLowerBound <= endA + 1 && endA + 1 <= startUpperBound
--               --       && endLowerBound <= endA + 1 && endA + 1 <= endUpperBound) $
--               --       pure OpenR b startA (endA + 1) endUpperBound
--         -- OpenR
--
-- -- | collects the regions where `xa` doesn't match
-- no :: SubseqExtractor v loc a -> SubseqExtractor v loc ()
-- no xa = SubseqExtractor $ \note ->
--   let as = run xa note in
--     if null [ a | Pure a <- as ] then -- results are not full
--       if null as then [Pure ()] -- results are empty, make them full
--       -- not full and not empty, find the negation
--       else reverse . fst $ foldl' go ([], Nothing) (List.sort $ fmap toPairs as)
--     else [] -- results were full, make them empty
--   where
--   toPairs :: Ranged a -> (Int, Int)
--   toPairs (Pure _) = error "this case should be avoided by the if!"
--   toPairs (Closed _ start end) = (start, end)
--   -- toPairs (OpenR _ start end) = (start, end)
--   -- toPairs (OpenLR start end) = (start, end)
--
--   go :: ([Ranged ()], Maybe Int) -> (Int, Int) -> ([Ranged ()], Maybe Int)
--   go ([], Nothing) (0, r) = ([], Just (r + 1))
--   go ([], Nothing) (l, r) = ([Closed () 0 (l - 1)], Just r)
--   go (_:_, Nothing) _    = error "state machine bug in Extractor2.no"
--   go (rs, Just r0) (l, r) =
--     (if r0 + 1 <= l - 1 then Closed () (r0 + 1) (l - 1) : rs else rs, Just r)
--
--
-- followedBy :: SubseqExtractor v loc a
--            -> SubseqExtractor v loc b
--            -> SubseqExtractor v loc (a,b)
-- followedBy = error "todo"
--
-- any :: SubseqExtractor v loc ()
-- any = SubseqExtractor $ \note -> Pure () : do
--   let last = pathLength note - 1
--   start <- [0..last]
--   end <- [0..last]
--   pure $ Closed () start end
--
-- pathLength :: C.Note v loc -> Int
-- pathLength = length . toList . C.path
--
-- many :: SubseqExtractor v loc a -> SubseqExtractor v loc [a]
-- many = error "todo"
--
-- {-
-- do
--   no InSynthesizeApp
--   a <- InSynthesizeApp
--   b <- InAndApp
--   pure (a,b)
-- -}
