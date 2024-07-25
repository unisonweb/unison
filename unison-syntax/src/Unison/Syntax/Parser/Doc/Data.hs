-- | Haskell parallel to @unison/base.Doc@.
--
--   This is much more restricted than @unison/base.Doc@, but it covers everything we can parse from Haskell. The
--   mismatch with Unison is a problem, as someone can create a Unison Doc with explicit constructors or function calls,
--   have it rendered to a scratch file, and then we can’t parse it. Changing the types here to match Unison wouldn’t
--   fix the issue. We have to modify the types and parser in concert (in both Haskell and Unison) to bring them in
--   line.
module Unison.Syntax.Parser.Doc.Data where

import Data.Functor.Classes
import Data.List.NonEmpty (NonEmpty)
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Parser.Ann (Annotated (..))
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token (..))

newtype UntitledSection a = UntitledSection [a]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data Top code a
  = -- | The first argument is always a Paragraph
    Section a [a]
  | Eval code
  | ExampleBlock code
  | CodeBlock (Token String) (Token String)
  | BulletedList (NonEmpty (Column a))
  | NumberedList (NonEmpty (Token Word64, Column a))
  | Paragraph (NonEmpty (Leaf code a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Eq2 Top where
  liftEq2 _ _ _ _ = True

instance (Eq code) => Eq1 (Top code)

instance Ord2 Top where
  liftCompare2 _ _ _ _ = LT

instance (Ord code) => Ord1 (Top code)

instance Show2 Top where
  liftShowsPrec2 _ _ _ _ _ _ x = x

instance (Show code) => Show1 (Top code)

data Column a
  = -- | The first is always a Paragraph, and the second a Bulleted or Numbered List
    Column a (Maybe a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

data Leaf code a
  = Link EmbedLink
  | -- | first is a Paragraph, second is always a Group (which contains either a single Term/Type link or list of
    --   Transcludes & Words)
    NamedLink a (Leaf code Void)
  | Example code
  | Transclude code
  | -- | Always a Paragraph
    Bold a
  | -- | Always a Paragraph
    Italic a
  | -- | Always a Paragraph
    Strikethrough a
  | -- | Always a Word
    Verbatim (Leaf Void Void)
  | -- | Always a Word
    Code (Leaf Void Void)
  | Source (NonEmpty (SourceElement code))
  | FoldedSource (NonEmpty (SourceElement code))
  | EvalInline code
  | Signature (NonEmpty EmbedSignatureLink)
  | SignatureInline EmbedSignatureLink
  | Word (Token String)
  | Group (Join code a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifunctor Leaf where
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
    Source elems -> Source $ fmap f <$> elems
    FoldedSource elems -> FoldedSource $ fmap f <$> elems
    EvalInline code -> EvalInline $ f code
    Signature x -> Signature x
    SignatureInline x -> SignatureInline x
    Word x -> Word x
    Group join -> Group $ bimap f g join

data EmbedLink
  = EmbedTypeLink (Token (HQ'.HashQualified Name))
  | EmbedTermLink (Token (HQ'.HashQualified Name))
  deriving (Eq, Ord, Show)

data SourceElement code = SourceElement EmbedLink [EmbedAnnotation code]
  deriving (Eq, Ord, Show, Functor)

newtype EmbedSignatureLink = EmbedSignatureLink (Token (HQ'.HashQualified Name))
  deriving (Eq, Ord, Show)

newtype Join code a = Join (NonEmpty (Leaf code a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifunctor Join where
  bimap f g (Join leaves) = Join $ bimap f g <$> leaves

newtype EmbedAnnotation code
  = -- | Always a Transclude
    EmbedAnnotation (Either (Token (HQ'.HashQualified Name)) (Leaf code Void))
  deriving (Eq, Ord, Show)

instance Functor EmbedAnnotation where
  fmap f (EmbedAnnotation ann) = EmbedAnnotation $ first f <$> ann

instance (Annotated code, Annotated a) => Annotated (Top code a) where
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

instance (Annotated code, Annotated a) => Annotated (Leaf code a) where
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

instance Annotated EmbedLink where
  ann = \case
    EmbedTypeLink name -> ann name
    EmbedTermLink name -> ann name

instance (Annotated code) => Annotated (SourceElement code) where
  ann (SourceElement link target) = ann link <> ann target

instance Annotated EmbedSignatureLink where
  ann (EmbedSignatureLink name) = ann name

instance (Annotated code) => Annotated (EmbedAnnotation code) where
  ann (EmbedAnnotation a) = either ann ann a
