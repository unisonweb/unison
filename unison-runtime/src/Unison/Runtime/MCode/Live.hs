-- | Which slots of a frame the code that resumes there reads: the
-- liveness that native code's exits use to write back only the slots the
-- interpreter will read (JIT/design.md, "Only the live slots are
-- written"), and that the serializer uses to write a captured frame's dead
-- slots as unit, since a dead slot holds whatever an earlier frame left
-- there ('Unison.Runtime.Machine.reflectValue').
module Unison.Runtime.MCode.Live
  ( liveAt,
    instrReads,
    argReads,
    slotAt,
    (<+>),
  )
where

import Data.IntSet qualified as IS
import Data.Map qualified as Map
import Data.Primitive.PrimArray (primArrayToList)
import Data.Word (Word64)
import Unison.Runtime.MCode
import Unison.Runtime.Wide (wideOn, wideWidth)
import Unison.Util.EnumContainers qualified as EC

-- | The union of two slot sets, where Nothing (every slot) absorbs.
(<+>) :: Maybe IS.IntSet -> Maybe IS.IntSet -> Maybe IS.IntSet
a <+> b = IS.union <$> a <*> b

infixr 5 <+>

-- | The slots (frame offsets, 1 and up) the interpreter reads when it
-- runs @sect@ at depth @d@ with the frame base at offset @base@, and in
-- whatever of the same frame runs after it; Nothing when that can't be
-- told statically, and every slot then has to count. Only these need to
-- be on the Unison stack when native code exits there: the others are
-- never read again (a slot dead here is dead at every later point of the
-- same path; a continuation captured later copies the frame as it is,
-- dead slots included, and never reads them either). Depths follow
-- 'genSection': MCode's "slot @i@ from the top" is offset @d - i@, an
-- instruction pushes 'pushCount' values, a @Let@'s body starts at its
-- combinator's arity, a data match arm pushes the constructor's fields.
-- An offset of 0 or below is the caller's (a pending argument), not ours.
--
-- A wide variable's field slots (Unison.Runtime.Wide) are read by a data
-- match on it and by its 'Box'; once boxed, in place, the object holds
-- the fields and the slots are dead, so the walk carries the slots boxed
-- so far and a match on one of those reads its value slot only.
liveAt :: (Word64 -> Maybe Int) -> Int -> Int -> GSection comb -> Maybe IS.IntSet
liveAt bodyArityOf base = go IS.empty
  where
    go bx d = \case
      App _ r args -> refReads d r <+> argReads base d args
      Call _ _ _ args -> argReads base d args
      Jump i args -> slotAt d i <+> argReads base d args
      Match i br -> slotAt d i <+> arms bx d br
      NMatch _ i br -> slotAt d i <+> arms bx d br
      DMatch _ i br -> slotAt d i <+> (if IS.member (d - i) bx then Just IS.empty else wideFields d i) <+> arms bx d br
      -- the pure request has one field, the value; a request arm pushes
      -- its constructor's fields
      RMatch i pu br ->
        slotAt d i
          <+> go bx (d + 1) pu
          <+> foldr (\(_, gb) acc -> arms bx d gb <+> acc) (Just IS.empty) (EC.mapToList br)
      Yield args -> argReads base d args
      Ins (Box i) rest ->
        let fields = IS.fromList [d - i - j | j <- [1 .. wideWidth - 1], d - i - j >= 1]
         in slotAt d i <+> Just fields <+> fmap (`IS.difference` fields) (go (IS.insert (d - i) bx) d rest)
      Ins i rest -> instrReads base d i <+> (pushCount i >>= \n -> go bx (d + n) rest)
      Let b (CIx _ _ w) _ body _ -> case bodyArityOf w of
        Just bodyArity | bodyArity >= d -> go bx d b <+> go bx bodyArity body
        _ -> Nothing
      Die _ -> Just IS.empty
      Exit -> Just IS.empty
    refReads d = \case
      Stk i -> slotAt d i
      _ -> Just IS.empty
    wideFields d i = if wideOn then foldr (<+>) (Just IS.empty) [slotAt d (i + j) | j <- [1 .. wideWidth - 1]] else Just IS.empty
    -- the arms of a match: each runs with the fields it binds pushed
    -- (none, for a match on an unboxed value), the default arm with
    -- nothing pushed
    arms bx d = \case
      Test1 _ a b -> arm bx d a <+> go bx d b
      Test2 _ a _ b c -> arm bx d a <+> arm bx d b <+> go bx d c
      TestW df m -> foldr (\(_, a) acc -> arm bx d a <+> acc) (go bx d df) (EC.mapToList m)
      TestT df m -> foldr (\a acc -> go bx d a <+> acc) (go bx d df) (Map.elems m)
      TestY df m -> foldr (\a acc -> go bx d a <+> acc) (go bx d df) (Map.elems m)
    arm bx d (Arm n a) = go bx (d + n) a

-- | The slots an instruction reads (see 'liveAt').
instrReads :: Int -> Int -> GInstr comb -> Maybe IS.IntSet
instrReads base d = \case
  Prim1 _ i -> slotAt d i
  Prim2 _ i j -> slotAt d i <+> slotAt d j
  RefCAS i j k -> slotAt d i <+> slotAt d j <+> slotAt d k
  ForeignCall _ _ args -> argReads base d args
  DLLCall -> Nothing
  SetAff _ i j -> slotAt d i <+> slotAt d j
  Capture _ -> Nothing
  Discard _ -> Nothing
  Name r args -> (case r of Stk i -> slotAt d i; _ -> Just IS.empty) <+> argReads base d args
  Info _ -> Just IS.empty
  Pack _ args -> argReads base d args
  PackWide _ _ args -> argReads base d args
  Box i -> foldr (<+>) (slotAt d i) [slotAt d (i + j) | j <- [1 .. wideWidth - 1]]
  RecPack _ args -> argReads base d args
  RecUnpack _ i -> slotAt d i
  RecSet _ args -> argReads base d args
  Lit _ -> Just IS.empty
  Print i -> slotAt d i
  Reset _ i mi -> slotAt d i <+> maybe (Just IS.empty) (slotAt d) mi
  InLocal i -> slotAt d i
  Fork i -> slotAt d i
  Atomically i -> slotAt d i
  Seq args -> argReads base d args
  TryForce i -> slotAt d i
  SandboxingFailure _ -> Nothing
  KeepAlive i -> slotAt d i
  NewForeignPtr i j -> slotAt d i <+> slotAt d j
  AddFinalizer i j -> slotAt d i <+> slotAt d j
  MkCallback i j -> slotAt d i <+> slotAt d j

-- | The slots an argument list reads, as 'argSources' resolves them.
argReads :: Int -> Int -> Args -> Maybe IS.IntSet
argReads base d = \case
  ZArgs -> Just IS.empty
  VArg1 i -> slotAt d i
  VArg2 i j -> slotAt d i <+> slotAt d j
  VArgR i l -> Just (IS.fromList [d - i - k | k <- [0 .. l - 1], d - i - k >= 1])
  VArgN v -> Just (IS.fromList [d - i | i <- primArrayToList v, d - i >= 1])
  VArgV i -> Just (IS.fromList [d - k | k <- [0 .. (d - base) - i - 1], d - k >= 1])

-- | Slot @i@ from the top at depth @d@, if it is one of ours.
slotAt :: Int -> Int -> Maybe IS.IntSet
slotAt d i
  | d - i >= 1 = Just (IS.singleton (d - i))
  | otherwise = Just IS.empty
