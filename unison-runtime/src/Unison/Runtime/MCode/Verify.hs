-- | A structural check of MCode, run once when a group enters a code cache
-- ('Unison.Runtime.Machine.cacheAdd0', 'Unison.Runtime.Interface.restoreCache')
-- and on values as they are decoded ('Unison.Runtime.Machine.reifyValue').
-- It exists because the runtime otherwise believes what the code says
-- about itself: a slot index, a frame size, a call's arity, how many fields
-- a match arm binds. MCode produced from typechecked Unison by 'emitCombs'
-- satisfies every rule here; MCode from a hand-made ANF value, or read from
-- a compiled program file, need not, and a slot index past the frame would
-- read or write past the stack, in the interpreter as in native code.
--
-- What is *not* checked, by design: how many fields a constructor has. No
-- declaration of a type is consulted. Every reader of a data object takes
-- the field count from the object itself (the interpreter pushes what the
-- closure holds; native code reads the pointer tag, or a wide
-- constructor's array lengths, and takes an arm only when the object has
-- the arm's count), so a @Pack@ or a decoded value with an unusual count
-- builds an unusual object and nothing reads past it (design.md, "What
-- native code trusts about MCode"). This is also what lets code whose
-- types the runtime never saw declared (code sent over the network) be
-- verified and compiled.
--
-- The rules:
--
-- 1. Every stack index refers to a slot within the frame at that point.
-- 2. The frame never grows past the combinator's declared frame size.
-- 3. Calls pass as many arguments as the callee takes; a @Yield@ into a
--    @Let@ body yields as many values as the body takes.
-- 4. A @Pack@'s tag names the type the instruction says (a forged tag
--    would have the value matched as another type's).
-- 5. Every data and request match arm carries its field count, which is
--    what its slots are checked against (rule 1).
-- 6. A @RecPack@ has as many arguments as its shape has fields.
-- 7. Every combinator referred to exists.
-- 8. For values: a partial application has fewer arguments than its
--    function's arity, a record its schema's field count.
--
-- Frame tracking follows the interpreter: the frame starts with the
-- arguments, each instruction pushes what 'pushCount' says, a match arm
-- pushes the fields it binds, a @Let@ body starts above the binding's
-- results. The same table drives the JIT's generator (Codegen.hs), so the
-- two cannot disagree about what an instruction pushes.
module Unison.Runtime.MCode.Verify
  ( Rule (..),
    VerifyError (..),
    VerifyEnv (..),
    verifyGroup,
    verifyValue,
    renderVerifyError,
  )
where

import Data.Map qualified as M
import Data.Primitive.PrimArray (primArrayToList)
import Data.Vector qualified as V
import Data.Word (Word64)
import Unison.Reference (Reference)
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.MCode
import Unison.Runtime.TypeTags (RTag (..), unpackTags)
import Unison.Util.EnumContainers qualified as EC

data Rule
  = SlotOutOfFrame
  | FrameTooSmall
  | CallArity
  | PackTag
  | MatchArm
  | RecordArity
  | UnknownReference
  | ValueArity
  | BadInstruction
  deriving (Eq, Show)

data VerifyError = VerifyError
  { -- | the combinator (or, for a value, the group reference its error is
    -- nearest to) the error is in
    veWhere :: String,
    veRule :: Rule,
    veDetail :: String
  }
  deriving (Eq, Show)

renderVerifyError :: VerifyError -> String
renderVerifyError (VerifyError w r d) = "code verification failed in " ++ w ++ ": " ++ show r ++ ": " ++ d

-- | What the rules check against: the cache's numberings and its loaded
-- combinators, all in hand where groups are added.
data VerifyEnv = VerifyEnv
  { -- | the number a type reference has in the cache
    veTypeNumber :: Reference -> Maybe Word64,
    -- | the group number a term reference has in the cache
    veTermNumber :: Reference -> Maybe Word64,
    -- | the arity and frame size of a combinator, in the group being
    -- checked or already loaded; Nothing if there is no such combinator
    veCombInfo :: CombIx -> Maybe (Int, Int),
    -- | the field count of a record schema
    veSchemaSize :: ANF.RecordSchema -> Maybe Int
  }

-- | Checks every combinator of a group.
verifyGroup :: VerifyEnv -> Reference -> Word64 -> EC.EnumMap Word64 (GComb val comb) -> Either VerifyError ()
verifyGroup env ref grp combs = mapM_ check (EC.mapToList combs)
  where
    check (i, CachedVal _ _) = if i == 0 then Right () else Left (VerifyError (name i) BadInstruction "a cached value at a section other than 0")
    check (i, Comb (LamI arity fsize entry _)) = do
      maxD <- walk env (name i) (Frame arity fsize) Nothing 0 arity entry
      -- rule 2: the deepest point of the frame fits what the interpreter's
      -- `ensure` guarantees at entry: the arguments plus fsize + 1 slots
      -- (`sp + fsize + 1 < size`, Stack.hs)
      if maxD > arity + fsize + 1
        then Left (VerifyError (name i) FrameTooSmall ("the frame reaches " ++ show maxD ++ " slots, " ++ show (arity + fsize + 1) ++ " guaranteed"))
        else Right ()
    name i = show ref ++ "#" ++ show grp ++ "." ++ show i

-- | The combinator's own frame: its arity and declared frame size.
data Frame = Frame !Int !Int

-- | Walks a section at frame depth @d@ and returns the deepest frame it
-- reaches (counting what each instruction pushes). @expected@ is the number
-- of values a 'Yield' here must produce (inside a 'Let' binding), or
-- Nothing at the top level, where a yield returns from the combinator.
-- @base@ is the depth the interpreter's frame pointer stands at: 0 for a
-- combinator's own frame (its arguments are in it), and the depth of the
-- @Let@ for a binding, which the interpreter runs in a frame of its own
-- (@saveFrame@); a @VArgV i@ means every slot above @base + i@.
walk :: VerifyEnv -> String -> Frame -> Maybe Int -> Int -> Int -> GSection comb -> Either VerifyError Int
walk env here frame expected base = go
  where
    err :: Rule -> String -> Either VerifyError a
    err rule detail = Left (VerifyError here rule detail)
    -- rule 1: index i from the top at depth d is slot d - 1 - i
    slot d i
      | i >= 0 && i < d = Right ()
      | otherwise = err SlotOutOfFrame ("index " ++ show i ++ " at depth " ++ show d)
    slots d = mapM_ (slot d)
    argSlots d = \case
      ZArgs -> Right ()
      VArg1 i -> slot d i
      VArg2 i j -> slot d i >> slot d j
      VArgR i l
        | l < 0 -> err SlotOutOfFrame ("a negative argument count " ++ show l)
        | otherwise -> slots d [i .. i + l - 1]
      VArgN v -> slots d (primArrayToList v)
      -- everything above frame index i: nothing below the frame
      VArgV i | i >= 0 && i <= d - base -> Right ()
      VArgV i -> err SlotOutOfFrame ("VArgV " ++ show i ++ " at depth " ++ show d ++ " in a frame starting at " ++ show base)
    count d = \case
      VArgV i -> Right (d - base - i)
      args -> Right (countArgs args)
    combInfo :: CombIx -> Either VerifyError (Int, Int)
    combInfo cix = maybe (err UnknownReference (show cix)) Right (veCombInfo env cix)

    go d = \case
      Ins instr rest -> do
        instrOk d instr
        case pushCount instr of
          -- the interpreter takes over here (a capture, a sandboxing
          -- failure); nothing after it runs in this frame
          Nothing -> pure d
          Just n -> go (d + n) rest
      Call skip cix _ args -> do
        argSlots d args
        n <- count d args
        (arity, fsize) <- combInfo cix
        if n /= arity then err CallArity ("a call with " ++ show n ++ " arguments to a function of arity " ++ show arity) else Right ()
        -- a call that skips the callee's stack check promises the callee's
        -- frame already fits in this one
        let Frame a f = frame
        if skip && d + fsize > a + f
          then err FrameTooSmall ("a call that skips the stack check, to a frame of " ++ show fsize ++ " at depth " ++ show d)
          else Right ()
        pure d
      -- rule 5: an arm runs with the fields it binds pushed, and the match
      -- says how many for each constructor it has an arm for
      DMatch mr i ns br -> do
        slot d i
        arms d br $ \ctor -> case EC.lookup ctor ns of
          Just n | n >= 0 -> Right n
          _ -> err MatchArm ("the arm for constructor " ++ show ctor ++ " of " ++ maybe "an unnamed type" show mr ++ " has no field count")
      NMatch _ i br -> slot d i >> arms d br (const (Right 0))
      Match i br -> slot d i >> arms d br (const (Right 0))
      RMatch i pureCase ns cases -> do
        slot d i
        -- the pure case sees the value
        m1 <- go (d + 1) pureCase
        ms <-
          mapM
            ( \(ability, br) -> arms d br $ \ctor -> case EC.lookup ability ns >>= EC.lookup ctor of
                Just n | n >= 0 -> Right n
                _ -> err MatchArm ("the arm for request " ++ show ctor ++ " of ability " ++ show ability ++ " has no field count")
            )
            (EC.mapToList cases)
        pure (maximum (m1 : ms))
      Let bnd cix _ body _ -> do
        -- the body is a combinator that takes the whole frame as its
        -- arguments: what it takes beyond the frame here is what the
        -- binding yields
        (bodyArity, _) <- combInfo cix
        let results = bodyArity - d
        if results < 0 then err CallArity ("a Let whose body takes " ++ show bodyArity ++ " arguments at depth " ++ show d) else Right ()
        m1 <- walk env here frame (Just results) d d bnd
        m2 <- go (d + results) body
        pure (max m1 m2)
      Yield args -> do
        argSlots d args
        n <- count d args
        case expected of
          Just m | n /= m -> err CallArity ("a binding yields " ++ show n ++ " values where its body takes " ++ show m)
          _ -> pure d
      App _ ref args -> do
        argSlots d args
        refOk d ref
        -- over-application and under-application are both the interpreter's
        -- business at an App; nothing to check about the count
        pure d
      Jump i args -> slot d i >> argSlots d args >> pure d
      Die _ -> pure d
      Exit -> pure d

    -- the arms of a branch: a constructor arm runs with that constructor's
    -- fields pushed, the default arm with nothing
    arms d br fields = case br of
      Test1 v cv df -> maximum <$> sequence [arm v cv, go d df]
      Test2 v cv w cw df -> maximum <$> sequence [arm v cv, arm w cw, go d df]
      TestW df cs -> maximum <$> sequence (go d df : map (uncurry arm) (EC.mapToList cs))
      TestT df cs -> maximum <$> sequence (go d df : map (go d) (M.elems cs))
      TestY df cs -> maximum <$> sequence (go d df : map (go d) (M.elems cs))
      where
        arm ctor body = fields ctor >>= \n -> go (d + n) body

    refOk d = \case
      Stk i -> slot d i
      Env cix _ -> combInfo cix >> Right ()
      Dyn _ -> Right ()

    instrOk d instr = do
      -- rule 1 for the instruction's reads
      case instr of
        Prim1 _ i -> slot d i
        Prim2 _ i j -> slot d i >> slot d j
        RefCAS i j k -> slots d [i, j, k]
        ForeignCall _ _ args -> argSlots d args
        DLLCall -> Right ()
        SetAff _ i j -> slot d i >> slot d j
        Capture _ -> Right ()
        Discard i -> slot d i
        Name r args -> refOk d r >> argSlots d args
        Info _ -> Right ()
        Pack _ _ args -> argSlots d args
        RecPack _ args -> argSlots d args
        RecUnpack _ i -> slot d i
        RecSet _ args -> argSlots d args
        Lit _ -> Right ()
        Print i -> slot d i
        Reset _ i mi -> slot d i >> maybe (Right ()) (slot d) mi
        InLocal i -> slot d i
        Fork i -> slot d i
        Atomically i -> slot d i
        Seq args -> argSlots d args
        TryForce i -> slot d i
        SandboxingFailure _ -> Right ()
        KeepAlive i -> slot d i
        NewForeignPtr i j -> slot d i >> slot d j
        AddFinalizer i j -> slot d i >> slot d j
        MkCallback i j -> slot d i >> slot d j
      -- rules 3, 4, 6, 7 for what the instruction claims
      case instr of
        Pack r tag _ -> do
          let (RTag rt, _) = unpackTags tag
          -- the tag must be the type's own: a Pack carrying another type's
          -- number would be matched as that type. Its field count is not
          -- checked: the object gets however many fields the Pack gives
          -- it, and every reader takes the count from the object
          case veTypeNumber env r of
            Just t | t /= rt -> err PackTag ("a Pack of " ++ show r ++ " with the tag of type number " ++ show rt)
            Nothing -> err UnknownReference ("a Pack of " ++ show r ++ ", which has no number")
            _ -> Right ()
        RecPack shape args -> do
          n <- count d args
          let m = V.length (shapeFields shape)
          if n /= m then err RecordArity ("a RecPack of " ++ show n ++ " values into a shape of " ++ show m ++ " fields") else Right ()
        RecSet _ args -> do
          n <- count d args
          if n /= 2 then err RecordArity ("a RecSet with " ++ show n ++ " arguments") else Right ()
        RecUnpack fields _
          | V.null fields -> err RecordArity "a RecUnpack of no fields"
          | otherwise -> Right ()
        Name (Env cix _) args -> do
          n <- count d args
          (arity, _) <- combInfo cix
          if n > arity then err CallArity ("a Name with " ++ show n ++ " arguments of a function of arity " ++ show arity) else Right ()
        _ -> Right ()

-- | Checks a value as it is decoded: partial application sizes and record
-- sizes, recursively (a data value's field count is its own business, see
-- the module comment). Continuation frames are checked for their values;
-- their frame sizes are the interpreter's own business when it rebuilds
-- them.
verifyValue :: VerifyEnv -> ANF.Value Reference -> Either VerifyError ()
verifyValue env = go
  where
    err :: Rule -> String -> Either VerifyError a
    err rule detail = Left (VerifyError "a value" rule detail)
    go = \case
      ANF.Partial (ANF.GR r i) vs -> do
        mapM_ go vs
        case veTermNumber env r >>= \n -> veCombInfo env (CIx r n i) of
          Just (arity, _) | length vs >= arity -> err ValueArity ("a partial application of " ++ show r ++ " to " ++ show (length vs) ++ " arguments, arity " ++ show arity)
          _ -> Right ()
      ANF.Data _ _ vs -> mapM_ go vs
      ANF.Record rs vals -> do
        case veSchemaSize env rs of
          Just m | m /= length vals -> err RecordArity ("a record of " ++ show (length vals) ++ " values for a schema of " ++ show m ++ " fields")
          _ -> Right ()
        mapM_ go vals
      ANF.Cont vs k -> mapM_ go vs >> cont k
      ANF.BLit _ -> Right ()
    cont = \case
      ANF.KE -> Right ()
      ANF.Mark _ _ vs k -> mapM_ (go . snd) vs >> cont k
      ANF.Push _ _ _ k -> cont k
