
module Unison.Runtime.ANF.Serialize.Tags where

import Unison.Runtime.Serialize (Tag (..), unknownTag)

data TmTag
  = VarT
  | ForceT
  | AppT
  | HandleT
  | ShiftT
  | MatchT
  | LitT
  | NameRefT
  | NameVarT
  | LetDirT
  | LetIndT
  | BxLitT

data FnTag
  = FVarT
  | FCombT
  | FContT
  | FConT
  | FReqT
  | FPrimT
  | FForeignT

data MtTag
  = MIntT
  | MTextT
  | MReqT
  | MEmptyT
  | MDataT
  | MSumT
  | MNumT

data LtTag
  = IT
  | NT
  | FT
  | TT
  | CT
  | LMT
  | LYT

data BLTag
  = TextT
  | ListT
  | TmLinkT
  | TyLinkT
  | BytesT
  | QuoteT
  | CodeT
  | BArrT
  | PosT
  | NegT
  | CharT
  | FloatT
  | ArrT
  | CachedCodeT

data VaTag = PartialT | DataT | ContT | BLitT

data CoTag = KET | MarkT | PushT

instance Tag TmTag where
  tag2word = \case
    VarT -> 1
    ForceT -> 2
    AppT -> 3
    HandleT -> 4
    ShiftT -> 5
    MatchT -> 6
    LitT -> 7
    NameRefT -> 8
    NameVarT -> 9
    LetDirT -> 10
    LetIndT -> 11
    BxLitT -> 12
  word2tag = \case
    1 -> pure VarT
    2 -> pure ForceT
    3 -> pure AppT
    4 -> pure HandleT
    5 -> pure ShiftT
    6 -> pure MatchT
    7 -> pure LitT
    8 -> pure NameRefT
    9 -> pure NameVarT
    10 -> pure LetDirT
    11 -> pure LetIndT
    12 -> pure BxLitT
    n -> unknownTag "TmTag" n

instance Tag FnTag where
  tag2word = \case
    FVarT -> 0
    FCombT -> 1
    FContT -> 2
    FConT -> 3
    FReqT -> 4
    FPrimT -> 5
    FForeignT -> 6

  word2tag = \case
    0 -> pure FVarT
    1 -> pure FCombT
    2 -> pure FContT
    3 -> pure FConT
    4 -> pure FReqT
    5 -> pure FPrimT
    6 -> pure FForeignT
    n -> unknownTag "FnTag" n

instance Tag MtTag where
  tag2word = \case
    MIntT -> 0
    MTextT -> 1
    MReqT -> 2
    MEmptyT -> 3
    MDataT -> 4
    MSumT -> 5
    MNumT -> 6

  word2tag = \case
    0 -> pure MIntT
    1 -> pure MTextT
    2 -> pure MReqT
    3 -> pure MEmptyT
    4 -> pure MDataT
    5 -> pure MSumT
    6 -> pure MNumT
    n -> unknownTag "MtTag" n

instance Tag LtTag where
  tag2word = \case
    IT -> 0
    NT -> 1
    FT -> 2
    TT -> 3
    CT -> 4
    LMT -> 5
    LYT -> 6

  word2tag = \case
    0 -> pure IT
    1 -> pure NT
    2 -> pure FT
    3 -> pure TT
    4 -> pure CT
    5 -> pure LMT
    6 -> pure LYT
    n -> unknownTag "LtTag" n

instance Tag BLTag where
  tag2word = \case
    TextT -> 0
    ListT -> 1
    TmLinkT -> 2
    TyLinkT -> 3
    BytesT -> 4
    QuoteT -> 5
    CodeT -> 6
    BArrT -> 7
    PosT -> 8
    NegT -> 9
    CharT -> 10
    FloatT -> 11
    ArrT -> 12
    CachedCodeT -> 13
  {-# INLINE tag2word #-}

  word2tag = \case
    0 -> pure TextT
    1 -> pure ListT
    2 -> pure TmLinkT
    3 -> pure TyLinkT
    4 -> pure BytesT
    5 -> pure QuoteT
    6 -> pure CodeT
    7 -> pure BArrT
    8 -> pure PosT
    9 -> pure NegT
    10 -> pure CharT
    11 -> pure FloatT
    12 -> pure ArrT
    13 -> pure CachedCodeT
    t -> unknownTag "BLTag" t
  {-# INLINE word2tag #-}

instance Tag VaTag where
  tag2word = \case
    PartialT -> 0
    DataT -> 1
    ContT -> 2
    BLitT -> 3
  {-# INLINE tag2word #-}

  word2tag = \case
    0 -> pure PartialT
    1 -> pure DataT
    2 -> pure ContT
    3 -> pure BLitT
    t -> unknownTag "VaTag" t
  {-# INLINE word2tag #-}

instance Tag CoTag where
  tag2word = \case
    KET -> 0
    MarkT -> 1
    PushT -> 2
  word2tag = \case
    0 -> pure KET
    1 -> pure MarkT
    2 -> pure PushT
    t -> unknownTag "CoTag" t

