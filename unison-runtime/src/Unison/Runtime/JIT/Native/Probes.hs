{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | The startup checks' way into the C helpers (cbits/jit_rt.c). At
-- startup "Unison.Runtime.JIT.Layout" runs every helper family (lists,
-- texts, bytes, arrays and refs, the hash, the comparison, the handler
-- environment, records, partial applications) against the interpreter
-- before the JIT is allowed on, and these are the calls it makes.
--
-- The protocol is the same for each family. Haskell builds sample values
-- and puts them in a boxed array ('MutableArray' of 'Any', so the C side
-- gets real heap pointers), then:
--
-- * an @init@ call teaches the family its layouts from the samples (info
--   pointers and tags, passed as a list of ints alongside the array) and
--   returns 1 or the number of the check that failed;
-- * a @check@ call asks whether a sample has the structure the helpers
--   assume;
-- * a @test@ call runs one helper on the samples (the operation and its
--   arguments as ints) and leaves the result in the array, where Haskell
--   compares it with what the interpreter computes.
--
-- 'probeClosure' is the first of all: it reads a closure's raw layout, from
-- which 'Layout.probeLayouts' learns where each constructor keeps its fields.
module Unison.Runtime.JIT.Native.Probes
  ( probeClosure,
    closureInit,
    nameTest,
    listInit,
    listCheck,
    listTest,
    textInit,
    textCheck,
    textTest,
    bytesInit,
    bytesCheck,
    bytesTest,
    arrayInit,
    murmurInit,
    murmurTest,
    cmpInit,
    cmpTest,
    envInit,
    recordInit,
  )
where

import Control.Monad.Primitive (PrimState)
import Data.Int (Int64)
import Data.Primitive.Array (MutableArray (..), sizeofMutableArray)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, peekElemOff, pokeElemOff)
import GHC.Exts (Any, MutableArray#)

-- | Passes a list of ints to C as an array.
withInts :: [Int] -> (Ptr Int64 -> IO a) -> IO a
withInts xs k = allocaBytes (8 * max 1 (length xs)) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] xs)
  k p

foreign import ccall unsafe "unison_jit_probe" c_probe :: MutableArray# (PrimState IO) Any -> Int64 -> Ptr Int64 -> IO Int64

-- | Inspects a closure. Element 0 of the array is the sample, the rest are
-- the objects in its pointer fields. Returns the raw probe output: info
-- pointer, pointer tag, closure type, pointer count, non-pointer count,
-- constructor tag, the payload words, then a match index per pointer field.
probeClosure :: MutableArray (PrimState IO) Any -> IO [Int]
probeClosure arr@(MutableArray arr#) = allocaBytes (8 * 64) $ \out -> do
  total <- c_probe arr# (fromIntegral (sizeofMutableArray arr)) out
  vals <- mapM (peekElemOff out) [0 .. 5 + 2 * fromIntegral total]
  pure (map fromIntegral vals)

foreign import ccall unsafe "unison_jit_list_init" c_listInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_list_check" c_listCheck :: MutableArray# (PrimState IO) Any -> IO Int64

foreign import ccall unsafe "unison_jit_list_test" c_listTest :: MutableArray# (PrimState IO) Any -> Int64 -> Int64 -> Int64 -> IO Int64

-- | Teaches the C list helpers the constructors of a list from samples
-- (see unison_jit_list_init), given the info pointers of Foreign, Val,
-- Data1 and Data2. 1 if all is well, else the number of the check that failed.
listInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
listInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_listInit arr# p

-- | Checks the structure of the list in slot 0: -1 if it isn't what the C
-- helpers assume, else the set of constructors met.
listCheck :: MutableArray (PrimState IO) Any -> IO Int
listCheck (MutableArray arr#) = fromIntegral <$> c_listCheck arr#

-- | Runs a C list helper on slot 0 (see unison_jit_list_test); the result
-- lands in slot 3. False if the helper left the case to the interpreter.
listTest :: MutableArray (PrimState IO) Any -> Int -> Int -> Int -> IO Bool
listTest (MutableArray arr#) op a b = (== 1) <$> c_listTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b)

foreign import ccall unsafe "unison_jit_text_init" c_textInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_text_check" c_textCheck :: MutableArray# (PrimState IO) Any -> IO Int64

foreign import ccall unsafe "unison_jit_text_test" c_textTest :: MutableArray# (PrimState IO) Any -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

-- | The same three for the text helpers (see unison_jit_text_init and
-- unison_jit_text_test). 'textTest' returns what the C function does.
textInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
textInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_textInit arr# p

textCheck :: MutableArray (PrimState IO) Any -> IO Bool
textCheck (MutableArray arr#) = (== 1) <$> c_textCheck arr#

textTest :: MutableArray (PrimState IO) Any -> Int -> Int -> Int -> Int -> IO Int
textTest (MutableArray arr#) op a b c = fromIntegral <$> c_textTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b) (fromIntegral c)

foreign import ccall unsafe "unison_jit_bytes_init" c_bytesInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_array_init" c_arrayInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_murmur_init" c_murmurInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

-- the C side returns a pair (ok, hash) by value; the FFI can't, so this
-- wrapper writes it through a pointer
foreign import ccall unsafe "unison_jit_murmur_test_ffi" c_murmurTest :: MutableArray# (PrimState IO) Any -> Int64 -> Int64 -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_env_init" c_envInit :: MutableArray# (PrimState IO) Any -> IO Int64

-- | Teaches the request helpers the handler environment's layout from
-- samples and checks the lookup on them (see unison_jit_env_init). 1 if
-- all is well, else the number of the check that failed.
envInit :: MutableArray (PrimState IO) Any -> IO Int
envInit (MutableArray arr#) = fromIntegral <$> c_envInit arr#

foreign import ccall unsafe "unison_jit_record_init" c_recordInit :: MutableArray# (PrimState IO) Any -> Ptr Int64 -> IO Int64

-- | Teaches the record helpers the record layout from samples and checks
-- the field lookup on them (see unison_jit_record_init). 1 if everything
-- is as expected, else the number of the check that failed.
recordInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
recordInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_recordInit arr# p

foreign import ccall unsafe "unison_jit_cmp_init" c_cmpInit :: Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_cmp_test_ffi" c_cmpTest :: MutableArray# (PrimState IO) Any -> Int64 -> Int64 -> Int64 -> Ptr Int64 -> IO Int64

-- | Hands the comparison helpers the PAp info pointer, the packed tags of
-- boxed Int and Nat, and the type part of Any's packed tag.
cmpInit :: [Int] -> IO Int
cmpInit infos = withInts infos $ \p ->
  fromIntegral <$> c_cmpInit p

-- | The native comparison (or equality, with the flag) of the values in
-- elements 0 and 1, whose unboxed halves are the two ints and whose boxed
-- halves are the elements; Nothing when the helper leaves the pair to the
-- interpreter.
cmpTest :: MutableArray (PrimState IO) Any -> Int -> Int -> Bool -> IO (Maybe Int)
cmpTest (MutableArray arr#) u1 u2 eq = allocaBytes 8 $ \p -> do
  ok <- c_cmpTest arr# (fromIntegral u1) (fromIntegral u2) (if eq then 4 else 0) p
  if ok == 1 then Just . fromIntegral <$> peek p else pure Nothing

-- | Hands the hash helper the type-tag closure in element 0 and the Enum
-- and DataG info pointers.
murmurInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
murmurInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_murmurInit arr# p

-- | The native hash of element 0 (boxed), or of the unboxed value with the
-- type tag in element 0; Nothing when the helper leaves it to the interpreter.
murmurTest :: MutableArray (PrimState IO) Any -> Int -> Bool -> IO (Maybe Word64)
murmurTest (MutableArray arr#) u unboxed = allocaBytes 8 $ \p -> do
  ok <- c_murmurTest arr# (fromIntegral u) (if unboxed then 1 else 0) p
  if ok == 1 then Just . fromIntegral <$> peek p else pure Nothing

-- | Hands the array and ref helpers the wrappers' info pointers and tags
-- (pairs, flattened) and the empty value in element 0.
arrayInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
arrayInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_arrayInit arr# p

foreign import ccall unsafe "unison_jit_bytes_check" c_bytesCheck :: MutableArray# (PrimState IO) Any -> IO Int64

foreign import ccall unsafe "unison_jit_bytes_test" c_bytesTest :: MutableArray# (PrimState IO) Any -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

-- | And for the bytes helpers (see unison_jit_bytes_init and
-- unison_jit_bytes_test).
bytesInit :: MutableArray (PrimState IO) Any -> [Int] -> IO Int
bytesInit (MutableArray arr#) infos = withInts infos $ \p ->
  fromIntegral <$> c_bytesInit arr# p

bytesCheck :: MutableArray (PrimState IO) Any -> IO Bool
bytesCheck (MutableArray arr#) = (== 1) <$> c_bytesCheck arr#

bytesTest :: MutableArray (PrimState IO) Any -> Int -> Int -> Int -> Int -> IO Int
bytesTest (MutableArray arr#) op a b c = fromIntegral <$> c_bytesTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b) (fromIntegral c)

foreign import ccall unsafe "unison_jit_closure_init" c_closureInit :: Ptr Int64 -> IO ()

foreign import ccall unsafe "unison_jit_name_test" c_nameTest :: MutableArray# (PrimState IO) Any -> Int64 -> IO Int64

-- | Gives the partial-application helper the info pointers of PAp and of
-- the boxes around a segment's two arrays.
closureInit :: [Int] -> IO ()
closureInit infos = withInts infos $ \p ->
  c_closureInit p

-- | Runs the helper on slot 0 with n arguments (see unison_jit_name_test).
nameTest :: MutableArray (PrimState IO) Any -> Int -> IO Bool
nameTest (MutableArray arr#) n = (== 1) <$> c_nameTest arr# (fromIntegral n)
