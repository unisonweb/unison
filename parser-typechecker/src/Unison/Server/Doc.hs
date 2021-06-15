{-# LANGUAGE DeriveGeneric #-}

module Unison.Server.Doc where

import Data.Text (Text)
import Data.Word
import GHC.Generics (Generic)
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Server.Syntax (SyntaxText)

type Nat = Word64

data Doc
  = Word Text
  | Code Doc
  | CodeBlock Text Doc
  | Bold Doc
  | Italic Doc
  | Strikethrough Doc
  | Style Text Doc
  | Anchor Text Doc
  | Blockquote Doc
  | Blankline
  | Linebreak
  | SectionBreak
  | Tooltip Doc Doc
  | Aside Doc
  | Callout (Maybe Doc) Doc
  | Table [[Doc]]
  | Folded Bool Doc Doc
  | Paragraph [Doc]
  | BulletedList [Doc]
  | NumberedList Nat [Doc]
  | Section Doc [Doc]
  | NamedLink Doc Doc
  | Image Doc Doc (Maybe Doc)
  | Special SpecialForm
  | Join [Doc]
  | UntitledSection [Doc]
  | Column [Doc]
  | Group Doc
  deriving (Eq,Show,Generic)

type UnisonHash = Text

data Ref a = Term a | Type a deriving (Eq,Show,Generic)

data SpecialForm
  = Source [Ref (UnisonHash, DisplayObject Src)]
  | FoldedSource [Ref (UnisonHash, DisplayObject Src)]
  | Example SyntaxText
  | ExampleBlock SyntaxText
  | Link SyntaxText
  | Signature [SyntaxText]
  | SignatureInline SyntaxText
  | Eval SyntaxText SyntaxText
  | EvalInline SyntaxText SyntaxText
  | Embed SyntaxText
  | EmbedInline SyntaxText
  deriving (Eq,Show,Generic)

-- `Src unfolded folded`
data Src = Src SyntaxText SyntaxText deriving (Eq,Show,Generic)
