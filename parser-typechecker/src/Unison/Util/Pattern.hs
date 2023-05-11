{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Unison.Util.Pattern where

data Pattern t pt
  = Join [Pattern t pt] -- sequencing of patterns
  | Or (Pattern t pt) (Pattern t pt) -- left-biased choice: tries second pattern only if first fails
  | Capture (Pattern t pt) -- capture all the text consumed by the inner pattern, discarding its subcaptures
  | CaptureAs t (Pattern t pt) -- capture the given text, discarding its subcaptures, and name the capture
  | Many (Pattern t pt) -- zero or more repetitions (at least 1 can be written: Join [p, Many p])
  | Replicate Int Int (Pattern t pt) -- m to n occurrences of a pattern, optional = 0-1
  | Eof -- succeed if given the empty text, fail otherwise
  | Literal t -- succeed if input starts with the given text, advance by that text
  | Predicate pt -- succeed if input starts with a char matching the given pattern, advance by 1 char
  deriving (Show, Eq, Ord)

type Compiled r = (Stack r -> r -> r) -> (Stack r -> r -> r) -> Stack r -> r -> r

class Compile t pt where
  compilePattern :: pt -> Compiled t
  compileSize :: t -> Int
  compileTake :: Int -> t -> t
  compileDrop :: Int -> t -> t

data CharClass
  = AlphaNum -- alphabetic or numeric characters
  | Upper -- uppercase alphabetic characters
  | Lower -- lowercase alphabetic characters
  | Whitespace -- whitespace characters (space, tab, newline, etc.)
  | Control -- non-printing control characters
  | Printable -- letters, numbers, punctuation, symbols, spaces
  | MarkChar -- accents, diacritics, etc.
  | Number -- numeric characters in any script
  | Punctuation -- connectors, brackets, quotes
  | Symbol -- symbols (math, currency, etc.)
  | Separator -- spaces, line separators, paragraph separators
  | Letter -- letters in any script
  deriving (Show, Eq, Ord)


-- Wrapper type. Holds a pattern together with its compilation. This is used as
-- the semantic value of a unison `Pattern a`. Laziness avoids building the
-- matcher until it actually needs to be used, and also avoids recalculating the
-- match function if a `CPattern` is 'run' multiple times, while allowing the
-- builtin runner to just take two arguments, and not try to build a partial
-- application by hand.
--
data CPattern t pt = CP (Pattern t pt) (t -> Maybe ([t], t))

instance Eq (CPattern t pt) where
  CP p _ == CP q _ = p == q

instance Ord (CPattern t pt) where
  CP p _ `compare` CP q _ = compare p q

cpattern :: Pattern t pt -> CPattern t pt
cpattern p = CP p (run p)

run :: Pattern t pt -> t -> Maybe ([t], t)
run p =
  let cp = compile p (\_ _ -> Nothing) (\acc rem -> Just (s acc, rem))
      s = reverse . capturesToList . stackCaptures
   in cp (Empty emptyCaptures)

-- Stack used to track captures and to support backtracking.
-- A `try` will push a `Mark` that allows the old state
-- (both the list of captures and the current remainder)
-- to be restored on failure.
data Stack t = Empty !(Captures t) | Mark !(Captures t) !t !(Stack t)

-- A difference list for representing the captures of a pattern.
-- So that capture lists can be appended in O(1).
type Captures t = [t] -> [t]

stackCaptures :: Stack t -> Captures t
stackCaptures (Mark cs _ _) = cs
stackCaptures (Empty cs) = cs
{-# INLINE stackCaptures #-}

pushCaptures :: Captures t -> Stack t -> Stack t
pushCaptures c (Empty cs) = Empty (appendCaptures c cs)
pushCaptures c (Mark cs t s) = Mark (appendCaptures c cs) t s
{-# INLINE pushCaptures #-}

pushCapture :: t -> Stack t -> Stack t
pushCapture txt = pushCaptures (txt :)
{-# INLINE pushCapture #-}

appendCaptures :: Captures t -> Captures t -> Captures t
appendCaptures c1 c2 = c1 . c2
{-# INLINE appendCaptures #-}

emptyCaptures :: Captures t
emptyCaptures = id

capturesToList :: Captures t -> [t]
capturesToList c = c []

compile :: Compile t pt => Pattern t pt -> Compiled t
compile Eof !err !success = go
  where
    go acc t
      | compileSize t == 0 = success acc t
      | otherwise = err acc t
compile (Literal txt) !err !success = go
    where
      go acc t
        | compileTake (compileSize txt) t == txt = success acc (compileDrop (compileSize txt) t)
        | otherwise = err acc t

compile (Predicate p) !err !success = compilePattern p err success
compile (CaptureAs t p) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 _ = success (pushCapture t acc0) rem
    compiled = compile p err' success'
    go acc t = compiled acc t acc t

compile (Capture c) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 t0 = success (pushCapture (compileTake (compileSize t0 - compileSize rem) t0) acc0) rem
    compiled = compile c err' success'
    go acc t = compiled acc t acc t

compile (Or p1 p2) err success = cp1
  where
    cp2 = compile p2 err success
    cp1 = try "Or" (compile p1) cp2 success
compile (Join ps) !err !success = go ps
  where
    go [] = success
    go (p : ps) =
      let pc = compile p err psc
          psc = compile (Join ps) err success
       in pc
compile (Many p) !_ !success =
  compile p success success'
  where
    go = compile p success success'
    success' acc rem
      | compileSize rem == 0 = success acc rem
      | otherwise = go acc rem

compile (Replicate m n p) !err !success =
  try "Replicate" (go1 m) err (go2 (n - m))
  where
    go1 0 = \_err success stk rem -> success stk rem
    go1 n = \err success -> compile p err (go1 (n - 1) err success)
    go2 0 = success
    go2 n = try "Replicate" (compile p) success (go2 (n - 1))

-- runs c and if it fails, restores state to what it was before
try :: String -> Compiled r -> Compiled r
try msg c err success stk rem =
  c err' success' (Mark id rem stk) rem
  where
    success' stk rem = case stk of
      Mark caps _ stk -> success (pushCaptures caps stk) rem
      _ -> error $ "Pattern compiler error in: " <> msg
    err' stk _ = case stk of
      Mark _ rem stk -> err stk rem
      _ -> error $ "Pattern compiler error in: " <> msg
{-# INLINE try #-}
