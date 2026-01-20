{-# LANGUAGE ExistentialQuantification #-}

module Unison.Runtime.Foreign.Dynamic where

import Control.Exception
import Control.Monad (unless, when)
import Foreign.ForeignPtr
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI.Internal
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable qualified as Store
import Unison.Runtime.FFI.DLL

data FFType
  = I16
  | I32
  | I64
  | U16
  | U32
  | U64
  | F32
  | D64
  | Void
  | MBArr
  deriving (Eq, Ord, Show)

-- arguments and return type
data FFSpec = FFSpec {ffArgs :: ![FFType], ffResult :: !FFType}
  deriving (Eq, Ord, Show)

data CSpec = CSpec
  { cInterface :: !(ForeignPtr CIF),
    numArgs :: !Int,
    ffSpec :: !FFSpec
  }

data CDynFunc = forall a.
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

encodeType :: FFType -> Ptr CType
encodeType I16 = ffi_type_sint16
encodeType I32 = ffi_type_sint32
encodeType I64 = ffi_type_sint64
encodeType U16 = ffi_type_uint16
encodeType U32 = ffi_type_uint32
encodeType U64 = ffi_type_uint64
encodeType D64 = ffi_type_double
encodeType F32 = ffi_type_float
encodeType Void = ffi_type_void
encodeType MBArr = ffi_type_pointer

encodeTypes :: [FFType] -> Ptr (Ptr CType) -> IO ()
encodeTypes [] !_ = pure ()
encodeTypes (t : ts) !p = do
  Store.poke p $ encodeType t
  encodeTypes ts (plusPtr p sz)
  where
    sz = Store.sizeOf (undefined :: Ptr CType)

data PrepException = BadVoid | BadResult | BadInit deriving (Show)

instance Exception PrepException

adjustSpec :: FFSpec -> IO FFSpec
adjustSpec sp@(FFSpec as r)
  | [Void] <- as = pure $ FFSpec [] r
  | any (== Void) as = throwIO BadVoid
  | otherwise = pure sp

prepareSpec :: FFSpec -> IO CSpec
prepareSpec spec = do
  ffSpec@(FFSpec args ret) <- adjustSpec spec

  when (ret == MBArr) $
    throwIO BadResult

  let numArgs = length args
      n = fromIntegral numArgs

  cInterface <- mallocForeignPtrBytes sizeOf_cif
  withForeignPtr cInterface \cif ->
    allocaArray numArgs \argTys -> do
      let retTy = encodeType ret
      encodeTypes args argTys
      status <- ffi_prep_cif cif ffi_default_abi n retTy argTys
      unless (status == ffi_ok) $
        throwIO BadInit

  pure $ CSpec {cInterface, numArgs, ffSpec}

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
callForeign (CDynFunc _ (CSpec cInterface _ _) fun) cArgs cRet =
  withForeignPtr cInterface \cif ->
    ffi_call cif fun (castPtr cRet) (castPtr cArgs)
