{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.MCode.Args
  ( Args' (..),
    Args (..),
    argsToLists,
    ucount,
    bcount,
  )
where

import Data.Primitive.PrimArray
import Unison.Runtime.ANF
  ( internalBug,
  )

data Args'
  = Arg1 !Int
  | Arg2 !Int !Int
  | -- frame index of each argument to the function
    ArgN {-# UNPACK #-} !(PrimArray Int)
  | ArgR !Int !Int
  deriving (Show)

data Args
  = ZArgs
  | UArg1 !Int
  | UArg2 !Int !Int
  | BArg1 !Int
  | BArg2 !Int !Int
  | DArg2 !Int !Int
  | UArgR !Int !Int
  | BArgR !Int !Int
  | DArgR !Int !Int !Int !Int
  | BArgN !(PrimArray Int)
  | UArgN !(PrimArray Int)
  | DArgN !(PrimArray Int) !(PrimArray Int)
  | DArgV !Int !Int
  deriving (Show, Eq, Ord)

ucount, bcount :: Args -> Int
ucount (UArg1 _) = 1
ucount (UArg2 _ _) = 2
ucount (DArg2 _ _) = 1
ucount (UArgR _ l) = l
ucount (DArgR _ l _ _) = l
ucount _ = 0
{-# INLINE ucount #-}
bcount (BArg1 _) = 1
bcount (BArg2 _ _) = 2
bcount (DArg2 _ _) = 1
bcount (BArgR _ l) = l
bcount (DArgR _ _ _ l) = l
bcount (BArgN a) = sizeofPrimArray a
bcount _ = 0
{-# INLINE bcount #-}

argsToLists :: Args -> ([Int], [Int])
argsToLists ZArgs = ([], [])
argsToLists (UArg1 i) = ([i], [])
argsToLists (UArg2 i j) = ([i, j], [])
argsToLists (BArg1 i) = ([], [i])
argsToLists (BArg2 i j) = ([], [i, j])
argsToLists (DArg2 i j) = ([i], [j])
argsToLists (UArgR i l) = (take l [i ..], [])
argsToLists (BArgR i l) = ([], take l [i ..])
argsToLists (DArgR ui ul bi bl) = (take ul [ui ..], take bl [bi ..])
argsToLists (BArgN bs) = ([], primArrayToList bs)
argsToLists (UArgN us) = (primArrayToList us, [])
argsToLists (DArgN us bs) = (primArrayToList us, primArrayToList bs)
argsToLists (DArgV _ _) = internalBug "argsToLists: DArgV"
