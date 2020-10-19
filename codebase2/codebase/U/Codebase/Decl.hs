{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module U.Codebase.Decl where

import Data.Word (Word64)
import U.Codebase.Reference (Reference')
import Data.Text (Text)
import U.Util.Hash (Hash)
import U.Codebase.Type (TypeR)
import qualified U.Util.Hashable as Hashable
import qualified U.Codebase.Type as Type
-- import qualified U.Core.ABT as ABT

type ConstructorId = Word64

data DeclType = Data | Effect
  deriving (Eq, Ord, Show, Enum)

type Decl v = DeclR (Reference' Text Hash) v

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DeclR r v = DataDeclaration {
  declType :: DeclType,
  modifier :: Modifier,
  bound :: [v],
  constructors' :: [(v, TypeR r v)]
}

-- instance Hashable ConstructorType where
--   tokens b = [Tag . fromIntegral $ fromEnum b]

-- * Hashing stuff
constructors :: DeclR r v -> [v]
constructors = fmap fst . constructors'

constructorTypes :: DeclR r v -> [TypeR r v]
constructorTypes = fmap snd . constructors'

-- toABT :: Ord v => Decl v -> ABT.Term F v ()
-- toABT dd = ABT.tm () $ Modified (modifier dd) dd'
--   where
--   dd' = ABT.absChain (bound dd) $
--           ABT.absCycle
--             (constructors dd)
--             (ABT.tm () . Constructors $ ABT.transform Type <$> constructorTypes dd)

data F a
  = Type (Type.FD a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable, Show)

instance Hashable.Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `2` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 2 : case e of
      Type t -> [tag 0, hashed $ Hashable.hash1 hashCycle hash t]
      LetRec bindings body ->
        let (hashes, hash') = hashCycle bindings
        in [tag 1] ++ map hashed hashes ++ [hashed $ hash' body]
      Constructors cs ->
        let (hashes, _) = hashCycle cs
        in tag 2 :  map hashed hashes
      Modified m t ->
        [tag 3, Hashable.accumulateToken m, hashed $ hash t]

instance Hashable.Hashable Modifier where
  tokens Structural = [Hashable.Tag 0]
  tokens (Unique txt) = [Hashable.Tag 1, Hashable.Text txt]
