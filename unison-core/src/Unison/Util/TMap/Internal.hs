{-
Copyright (c) 2018, Koji Miyazato

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Koji Miyazato nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
from https://github.com/viercc/trie-simple
-}

{- |
This module exposes internal representation of @TMap@.
TMap has one invariant condition:
* Subtrees of an @TMap@ should not be empty.
For example, consider following tree structure which is valid:
  > > fromList [("a",1), ("aa", 2), ("bc", 3)]
  > Root
  >   'a' -> 1
  >     'a' -> 2
  >   'b' ->
  >     'c' -> 3
Adding redundant node which represents empty map does not change
what an @TMap@ represents.
  > Root
  >   'a' -> 1
  >     'a' -> 2
  >   'b' ->
  >     'c' -> 3
  >   'd' ->
  >     'e' ->
  >       'f' ->
But such @TMap@ should not exist because it confuses @Eq@ and @Ord@
instances and @null@ function.
-}
module Unison.Util.TMap.Internal(
  -- * Types
  TMap(..),
  Node(..),
  foldTMap,
)
where

import Unison.Util.TMap.Hidden