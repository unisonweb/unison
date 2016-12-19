module Unison.DataDeclaration where

import qualified Unison.ABT as ABT
import qualified Unison.Type as T

newtype Product a = Product [T.F a]

data F a = Constructors [Product a]

-- | DataDeclarations are represented as ABTs over the base functor F, with variables in `v`
type DataDeclaration v = AnnotatedDataDeclaration v ()

-- | Like `DataDeclaration v`, but with an annotation of type `a` at every level in the tree
type AnnotatedDataDeclaration v a = ABT.Term F v a
