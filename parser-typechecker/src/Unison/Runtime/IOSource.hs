{-# LANGUAGE TemplateHaskell #-}

module Unison.Runtime.IOSource
  ( module Unlifted,
    Unison.Runtime.IOSource.isTestReference,
  )
where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Unison.Reference as R
import Unison.Runtime.IOSource.Unlifted as Unlifted hiding (isTestReference)
import qualified Unison.Runtime.IOSource.Unlifted as UnliftedIOSource

isTestReference :: R.Reference
isTestReference = R.unliftRef $(TH.lift (R.liftRef UnliftedIOSource.isTestReference))
