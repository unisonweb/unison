{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime
  ( Runtime,
    pattern Runtime,
    terminate,
    evaluate,
    compileTo,
    mainType,
    ioTestTypes,
    Error (..),
  )
where

import Data.Text (Text)
import Unison.Codebase.Runtime qualified as Rt
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference (Reference)
import Unison.Runtime.Decompile (DecompError, DecompResult)
import Unison.Runtime.Exception (RuntimeExn)
import Unison.Runtime.InternalError (CompileExn)
import Unison.Runtime.Stack (RuntimePanic, Val)
import Unison.Symbol (Symbol)

data Error
  = -- | __TODO__: This constructor should go away, but there are still a few places that have unstructured errors.
    UnstructuredError Text
  | CompileExn CompileExn
  | RuntimeExn (Maybe (PrettyPrintEnv, Reference -> Reference, Val -> DecompResult Symbol)) RuntimeExn
  | RuntimePanic PrettyPrintEnv (Val -> DecompResult Symbol) RuntimePanic

type Runtime = Rt.Runtime Error DecompError

pattern Runtime {terminate, evaluate, compileTo, mainType, ioTestTypes} =
  Rt.Runtime {terminate, evaluate, compileTo, mainType, ioTestTypes}
