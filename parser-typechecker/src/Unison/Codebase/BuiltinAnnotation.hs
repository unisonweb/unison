{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (..)) where

import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann

class BuiltinAnnotation a where
  builtinAnnotation :: a

instance BuiltinAnnotation Ann where
  builtinAnnotation = Ann.Intrinsic