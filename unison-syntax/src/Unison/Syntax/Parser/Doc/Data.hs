{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell parallel to @unison/base.Doc@.
--
--   These types have two significant parameters: @ident@ and @code@ that are expected to be parameterized by some
--   representation of identifiers and source code of the host language.
--
--   This is much more restricted than @unison/base.Doc@, but it covers everything we can parse from Haskell. The
--   mismatch with Unison is a problem, as someone can create a Unison Doc with explicit constructors or function calls,
--   have it rendered to a scratch file, and then we can’t parse it. Changing the types here to match Unison wouldn’t
--   fix the issue. We have to modify the types and parser in concert (in both Haskell and Unison) to bring them in
--   line.
module Unison.Syntax.Parser.Doc.Data where

import Data.Eq.Deriving (deriveEq1, deriveEq2)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord.Deriving (deriveOrd1, deriveOrd2)
import Text.Show.Deriving (deriveShow1, deriveShow2)
import Unison.Parser.Ann (Annotated (..))
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token (..))

newtype UntitledSection a = UntitledSection [a]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data Top ident code a
  = -- | The first argument is always a `Paragraph`
    Section a [a]
  | Eval code
  | ExampleBlock code
  | CodeBlock (Token String) (Token String)
  | BulletedList (NonEmpty (Column a))
  | NumberedList (NonEmpty (Token Word64, Column a))
  | Paragraph (NonEmpty (Leaf ident code a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data Column a
  = -- | The first is always a `Paragraph`, and the second a `BulletedList` or `NumberedList`
    Column a (Maybe a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data Leaf ident code a
  = Link (EmbedLink ident)
  | -- | first is a Paragraph, second is always a Group (which contains either a single Term/Type link or list of
    --   `Transclude`s & `Word`s)
    NamedLink a (Leaf ident code Void)
  | Example code
  | Transclude code
  | -- | Always a Paragraph
    Bold a
  | -- | Always a Paragraph
    Italic a
  | -- | Always a Paragraph
    Strikethrough a
  | -- | Always a Word
    Verbatim (Leaf ident Void Void)
  | -- | Always a Word
    Code (Leaf ident Void Void)
  | -- | Always a Transclude
    Source (NonEmpty (SourceElement ident (Leaf ident code Void)))
  | -- | Always a Transclude
    FoldedSource (NonEmpty (SourceElement ident (Leaf ident code Void)))
  | EvalInline code
  | Signature (NonEmpty (EmbedSignatureLink ident))
  | SignatureInline (EmbedSignatureLink ident)
  | Word (Token String)
  | Group (Join (Leaf ident code a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifunctor (Leaf ident) where
  bimap f g = \case
    Link x -> Link x
    NamedLink a leaf -> NamedLink (g a) $ first f leaf
    Example code -> Example $ f code
    Transclude code -> Transclude $ f code
    Bold a -> Bold $ g a
    Italic a -> Italic $ g a
    Strikethrough a -> Strikethrough $ g a
    Verbatim leaf -> Verbatim leaf
    Code leaf -> Code leaf
    Source elems -> Source $ fmap (first f) <$> elems
    FoldedSource elems -> FoldedSource $ fmap (first f) <$> elems
    EvalInline code -> EvalInline $ f code
    Signature x -> Signature x
    SignatureInline x -> SignatureInline x
    Word x -> Word x
    Group join -> Group $ bimap f g <$> join

data EmbedLink ident
  = EmbedTypeLink (Token ident)
  | EmbedTermLink (Token ident)
  deriving (Eq, Ord, Show)

data SourceElement ident a = SourceElement (EmbedLink ident) [EmbedAnnotation ident a]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

newtype EmbedSignatureLink ident = EmbedSignatureLink (Token ident)
  deriving (Eq, Ord, Show)

newtype Join a = Join (NonEmpty a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

newtype EmbedAnnotation ident a
  = EmbedAnnotation (Either (Token ident) a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance (Annotated code, Annotated a) => Annotated (Top ident code a) where
  ann = \case
    Section title body -> ann title <> ann body
    Eval code -> ann code
    ExampleBlock code -> ann code
    CodeBlock label body -> ann label <> ann body
    BulletedList items -> ann items
    NumberedList items -> ann $ snd <$> items
    Paragraph leaves -> ann leaves

instance (Annotated a) => Annotated (Column a) where
  ann (Column para list) = ann para <> ann list

instance (Annotated code, Annotated a) => Annotated (Leaf ident code a) where
  ann = \case
    Link link -> ann link
    NamedLink label target -> ann label <> ann target
    Example code -> ann code
    Transclude code -> ann code
    Bold para -> ann para
    Italic para -> ann para
    Strikethrough para -> ann para
    Verbatim word -> ann word
    Code word -> ann word
    Source elems -> ann elems
    FoldedSource elems -> ann elems
    EvalInline code -> ann code
    Signature links -> ann links
    SignatureInline link -> ann link
    Word text -> ann text
    Group (Join leaves) -> ann leaves

instance Annotated (EmbedLink ident) where
  ann = \case
    EmbedTypeLink name -> ann name
    EmbedTermLink name -> ann name

instance (Annotated code) => Annotated (SourceElement ident code) where
  ann (SourceElement link target) = ann link <> ann target

instance Annotated (EmbedSignatureLink ident) where
  ann (EmbedSignatureLink name) = ann name

instance (Annotated code) => Annotated (EmbedAnnotation ident code) where
  ann (EmbedAnnotation a) = either ann ann a

$(deriveEq1 ''Column)
$(deriveOrd1 ''Column)
$(deriveShow1 ''Column)

$(deriveEq1 ''Token)
$(deriveOrd1 ''Token)
$(deriveShow1 ''Token)

$(deriveEq1 ''EmbedAnnotation)
$(deriveOrd1 ''EmbedAnnotation)
$(deriveShow1 ''EmbedAnnotation)
$(deriveEq2 ''EmbedAnnotation)
$(deriveOrd2 ''EmbedAnnotation)
$(deriveShow2 ''EmbedAnnotation)

$(deriveEq1 ''EmbedLink)
$(deriveOrd1 ''EmbedLink)
$(deriveShow1 ''EmbedLink)

$(deriveEq1 ''SourceElement)
$(deriveOrd1 ''SourceElement)
$(deriveShow1 ''SourceElement)
$(deriveEq2 ''SourceElement)
$(deriveOrd2 ''SourceElement)
$(deriveShow2 ''SourceElement)

$(deriveEq1 ''Join)
$(deriveOrd1 ''Join)
$(deriveShow1 ''Join)

$(deriveEq1 ''Leaf)
$(deriveOrd1 ''Leaf)
$(deriveShow1 ''Leaf)
$(deriveEq2 ''Leaf)
$(deriveOrd2 ''Leaf)
$(deriveShow2 ''Leaf)

$(deriveEq1 ''Top)
$(deriveOrd1 ''Top)
$(deriveShow1 ''Top)
$(deriveEq2 ''Top)
$(deriveOrd2 ''Top)
$(deriveShow2 ''Top)
