{-# LANGUAGE ExistentialQuantification #-}

module Unison.Runtime.Foreign.Dynamic where

import Control.Exception
import Control.Monad (unless, when)
import Data.Word (Word64)
import Foreign.C.Types (CUInt (..))
import Foreign.ForeignPtr
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI.Internal
import Foreign.Ptr
import Foreign.Storable qualified as Store
import Unison.Runtime.FFI.DLL

data FFType
  = I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | F32
  | D64
  | Void
  | MBArr
  | Ptr
  deriving (Eq, Ord, Show)

-- Nothing denotes an ordinary fixed-arity function. Just n denotes a
-- variadic function with n fixed arguments, even when no optional arguments
-- are supplied. Keep the count as a Word64 until it has been validated.
data FFSpec = FFSpec
  { ffArgs :: ![FFType],
    ffResult :: !FFType,
    ffFixedArgs :: !(Maybe Word64)
  }
  deriving (Eq, Ord, Show)

data CSpec = CSpec
  { cInterface :: !(ForeignPtr CIF),
    cArgumentTypes :: !(ForeignPtr (Ptr CType)),
    numArgs :: !Int,
    ffSpec :: !FFSpec
  }

data CDynFunc
  = forall a.
  CDynFunc
  { cName :: String,
    cSpec :: {-# UNPACK #-} !CSpec,
    cFun :: !(FunPtr a)
  }

cffArgs :: CDynFunc -> [FFType]
cffArgs = ffArgs . ffSpec . cSpec

cffResult :: CDynFunc -> FFType
cffResult = ffResult . ffSpec . cSpec

instance Show CDynFunc where
  show f = "<" ++ cName f ++ ">"

-- Compare pointers for CDynFunc
instance Eq CDynFunc where
  CDynFunc _ _ l == CDynFunc _ _ r = l == castFunPtr r

instance Ord CDynFunc where
  compare (CDynFunc _ _ l) (CDynFunc _ _ r) = compare l (castFunPtr r)

encodeType :: FFType -> Ptr CType
encodeType I8 = ffi_type_sint8
encodeType I16 = ffi_type_sint16
encodeType I32 = ffi_type_sint32
encodeType I64 = ffi_type_sint64
encodeType U8 = ffi_type_uint8
encodeType U16 = ffi_type_uint16
encodeType U32 = ffi_type_uint32
encodeType U64 = ffi_type_uint64
encodeType D64 = ffi_type_double
encodeType F32 = ffi_type_float
encodeType Void = ffi_type_void
encodeType MBArr = ffi_type_pointer
encodeType Ptr = ffi_type_pointer

encodeTypes :: [FFType] -> Ptr (Ptr CType) -> IO ()
encodeTypes [] !_ = pure ()
encodeTypes (t : ts) !p = do
  Store.poke p $ encodeType t
  encodeTypes ts (plusPtr p sz)
  where
    sz = Store.sizeOf (undefined :: Ptr CType)

data PrepException
  = BadVoid
  | BadResult
  | BadInit
  | BadFixedArgs Word64 Int
  | BadVariadicArg Int FFType
  deriving (Eq, Show)

instance Exception PrepException

adjustSpec :: FFSpec -> IO FFSpec
adjustSpec sp@(FFSpec as _ fixed)
  | [Void] <- as, Nothing <- fixed = pure $ sp {ffArgs = []}
  | any (== Void) as = throwIO BadVoid
  | otherwise = pure sp

prepareSpec :: FFSpec -> IO CSpec
prepareSpec spec = do
  ffSpec@(FFSpec args ret fixed) <- adjustSpec spec

  when (ret == MBArr) $
    throwIO BadResult

  let numArgs = length args
      n = fromIntegral numArgs

  case fixed of
    Nothing -> pure ()
    Just count -> do
      when (count == 0 || count > fromIntegral numArgs || count > fromIntegral (maxBound :: CUInt)) $
        throwIO $
          BadFixedArgs count numArgs
      -- C's default argument promotions apply only after the fixed prefix.
      -- The caller must describe the promoted types explicitly.
      mapM_
        (\(i, t) -> when (t `elem` [I8, I16, U8, U16, F32]) $ throwIO $ BadVariadicArg i t)
        (drop (fromIntegral count) $ zip [1 ..] args)

  cInterface <- mallocForeignPtrBytes sizeOf_cif
  -- ffi_cif retains the argument-type array. It must survive preparation
  -- and remain alive throughout every call, just like the CIF itself.
  cArgumentTypes <- mallocForeignPtrArray numArgs
  withForeignPtr cInterface \cif ->
    withForeignPtr cArgumentTypes \argTys -> do
      let retTy = encodeType ret
      encodeTypes args argTys
      status <- case fixed of
        Nothing -> ffi_prep_cif cif ffi_default_abi n retTy argTys
        Just count -> ffi_prep_cif_var cif ffi_default_abi (fromIntegral count) n retTy argTys
      unless (status == ffi_ok) $
        throwIO BadInit

  pure $ CSpec {cInterface, cArgumentTypes, numArgs, ffSpec}

-- The Haskell libffi package exposes only the fixed-arity preparation call.
foreign import ccall unsafe "ffi_prep_cif_var"
  ffi_prep_cif_var :: Ptr CIF -> C_ffi_abi -> CUInt -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO C_ffi_status

loadForeign :: DLL -> FFSpec -> String -> IO CDynFunc
loadForeign dll fspec sym =
  CDynFunc name <$> prepareSpec fspec <*> getDLLSym dll sym
  where
    name = getDLLPath dll ++ "$" ++ sym

-- Calls a foreign function with arguments stored in memory pointed to by
-- the first pointer, and returning the result to the second pointer. The
-- argument pointer should be to a 64-bit type, and it should point to
-- memory with as many arguments as are taken by the specification of the
-- function argument.
--
-- If some of the function's arguments are smaller than 64-bits, they
-- should be written as individual 64-bit locations in the pointer, so
-- that casting the offset pointer to the smaller type gives the pointer
-- used to write the smaller value to the memory. E.G.
--
--     Store.poke (castPtr (plusPtr p i)) <smaller-value>
callForeign :: CDynFunc -> Ptr (Ptr a) -> Ptr r -> IO ()
callForeign (CDynFunc _ CSpec {cInterface, cArgumentTypes} fun) cArgs cRet =
  withForeignPtr cInterface \cif ->
    withForeignPtr cArgumentTypes \_ ->
      ffi_call cif fun (castPtr cRet) (castPtr cArgs)
