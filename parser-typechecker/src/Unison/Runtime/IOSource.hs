{-# LANGUAGE TemplateHaskell #-}

module Unison.Runtime.IOSource
  ( module Unlifted,
  )
where

import qualified Language.Haskell.TH.Syntax as TH
import Unison.Prelude
import qualified Unison.Reference as R
import qualified Unison.Runtime.IOSource.Unlifted as Unlifted

isTestRef :: R.Reference
isTestRef = R.unliftRef $(TH.lift (R.liftRef Unlifted.isTestReference))
