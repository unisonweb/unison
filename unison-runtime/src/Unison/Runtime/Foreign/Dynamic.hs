{-# LANGUAGE ExistentialQuantification #-}

module Unison.Runtime.Foreign.Dynamic where

import Control.Monad (unless)
import Data.Tagged (Tagged (..))
import Data.Word
import Foreign.ForeignPtr
import Foreign.LibFFI.FFITypes
import Foreign.LibFFI.Internal
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable qualified as Store
import Unison.Runtime.FFI.DLL
import Unison.Runtime.Foreign
import Unison.Type (ffiTypeRef, ffiSpecRef, ffiFuncRef)

data FFType = I64 | U64 deriving (Eq, Ord, Show)

instance BuiltinForeign FFType where
  foreignName = Tagged "FFI.Type"
  foreignRef = Tagged ffiTypeRef

-- arguments and return type
data FFSpec = FFSpec [FFType] !FFType deriving (Eq, Ord, Show)

instance BuiltinForeign FFSpec where
  foreignName = Tagged "FFI.Spec"
  foreignRef = Tagged ffiSpecRef

type CABI = Word32

data CSpec = CSpec
  { cInterface :: !(ForeignPtr CIF)
  , numArgs :: !Int
  }

data CDynFunc = forall a. CDynFunc
  { cName :: String
  , cResult :: !FFType
  , cSpec :: {-# UNPACK #-} !CSpec
  , cFun :: !(FunPtr a)
  }

instance Show CDynFunc where
  show f = "<" ++ cName f ++ ">"

instance BuiltinForeign CDynFunc where
  foreignName = Tagged "DLL.Func"
  foreignRef = Tagged ffiFuncRef

encodeType :: FFType -> Ptr CType
encodeType I64 = ffi_type_sint64
encodeType U64 = ffi_type_uint64

encodeTypes :: [FFType] -> Ptr (Ptr CType) -> IO ()
encodeTypes []     !_ = pure ()
encodeTypes (t:ts) !p = do
  Store.poke p $ encodeType t
  encodeTypes ts (plusPtr p sz)
  where
    sz = Store.sizeOf (undefined :: Ptr CType)

prepareSpec :: FFSpec -> IO CSpec
prepareSpec (FFSpec args ret) = do
  cInterface <- mallocForeignPtrBytes sizeOf_cif

  withForeignPtr cInterface \cif ->
    allocaArray numArgs \argTys -> do
      let retTy = encodeType ret
      encodeTypes args argTys
      status <- ffi_prep_cif cif ffi_default_abi n retTy argTys
      unless (status == ffi_ok) $
        error "FFI initialization error"

  pure $ CSpec { cInterface, numArgs }
  where
    numArgs = length args
    n = fromIntegral numArgs

loadForeign :: DLL -> FFSpec -> String -> IO CDynFunc
loadForeign dll fspec@(FFSpec _ r) sym =
  CDynFunc name r <$> prepareSpec fspec <*> getDLLSym dll sym
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
callForeign (CDynFunc _ _ (CSpec cInterface _) fun) cArgs cRet =
  withForeignPtr cInterface \cif ->
    ffi_call cif fun (castPtr cRet) (castPtr cArgs)
