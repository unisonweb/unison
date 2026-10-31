-- | Render a doc that the server has evaluated (a 'Doc', the structure Unison Share and the codebase server return) for
-- the console. This is @syntax.docFormatConsole@ in "Unison.Runtime.IOSource", for the evaluated structure, together
-- with how each special form shows up on the console.
module Unison.CommandLine.DocRender
  ( docToConsole,
  )
where

import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.HashQualified qualified as HQ
import Unison.Syntax.Name qualified as Name
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Server.Doc
import Unison.Server.Syntax qualified as Server
import Unison.Util.AnnotatedText (AnnotatedText (..), Segment (..))
import Unison.Util.ColorText (ColorText)
import Unison.Util.ColorText qualified as CT
import Unison.Util.Pretty qualified as P
import Unison.Util.SyntaxText qualified as ST

type Pretty = P.Pretty ColorText

docToConsole :: Doc -> Pretty
docToConsole = go
  where
    go :: Doc -> Pretty
    go = \case
      Word t -> lit t
      Code d -> P.group (lit "`" <> go d <> lit "`")
      CodeBlock typ d -> P.group (lit "``` " <> P.group (lit typ) <> nl <> go d <> nl <> lit "```")
      Italic d -> emphasized "*" d
      Strikethrough d -> emphasized "~~" d
      Bold d -> P.bold (go d)
      Style _ d -> go d
      Anchor _ d -> go d
      Blockquote d -> P.group (indent (lit "> ") (go d))
      Blankline -> P.group (lit "\n\n")
      Linebreak -> P.group (lit "\n")
      SectionBreak -> lit "܍"
      Tooltip inner _ -> go inner
      Aside d -> P.hiBlack (lit "(" <> go d <> lit ")")
      Callout Nothing d -> P.group (indent (lit "  | ") (go d))
      Callout (Just icon) d -> P.group (indent (lit "  | ") (P.sep nl [P.bold (go icon), lit "", go d]))
      Table rows -> P.group (P.table (map (map go) rows))
      Folded _ summary details -> go summary <> go details
      Paragraph ds -> P.wrap' (pure . P.lit) (foldMap go ds)
      BulletedList ds -> P.group (P.sep nl [indent' (lit "* ") (lit "  ") (go d) | d <- ds])
      NumberedList n ds ->
        let dot = ". "
            w = Text.length (tShow (n + fromIntegral (length ds))) + Text.length dot
            num k = lit (Text.justifyRight w ' ' (tShow k <> dot))
            pad = lit (Text.replicate w " ")
         in P.group (P.sep nl [indent' (num k) pad (go d) | (k, d) <- zip [n ..] ds])
      Section title ds ->
        let t = indent' (lit "# ") (lit "  ") (P.group (go (Bold title)))
            subs = [indent (lit "  ") (P.group (go d)) | d <- ds]
         in P.group (P.sep (nl <> nl) (t : subs))
      UntitledSection ds -> P.group (P.sep (nl <> nl) [P.group (go d) | d <- ds])
      Join ds -> foldMap go ds
      Column ds -> P.sep nl (map go ds)
      Group d -> P.group (go d)
      NamedLink name _ -> P.underline (go name)
      Image alt _ (Just caption) -> P.sep nl [go alt, go (Italic caption)]
      Image alt _ Nothing -> go alt
      Special form -> special form

    -- Emphasis marks go inside the first and last words of a paragraph, so they stay with them when the paragraph wraps.
    emphasized mark = \case
      Paragraph (l : rest@(_ : _)) ->
        let (mid, r) = (List.init rest, List.last rest)
         in P.group (lit mark <> go l) <> foldMap go mid <> P.group (go r <> lit mark)
      d -> P.group (lit mark <> go d <> lit mark)

    special :: RenderedSpecialForm -> Pretty
    special = \case
      Source srcs -> sources srcs
      FoldedSource srcs -> sources srcs
      Example code -> P.backticked (syntax code)
      ExampleBlock code -> P.indentN 4 (syntax code)
      Link code -> P.underline (syntax code)
      Signature sigs -> P.group (P.sep "\n\n" [P.indentN 4 (syntax s) | s <- sigs])
      SignatureInline sig -> P.backticked (syntax sig)
      Eval code result -> P.indentN 4 (P.lines [syntax code, "⧨", P.green (syntax result)])
      EvalInline _ result -> P.backticked (syntax result)
      Embed code -> P.indentN 2 (nl <> "{{ embed {{" <> syntax code <> "}} }}" <> nl)
      EmbedInline code -> "{{ embed {{" <> syntax code <> "}} }}"
      Video {} -> "(a video)"
      FrontMatter {} -> mempty
      LaTeXInline t -> P.backticked (lit t)
      Svg {} -> "(an image)"
      RenderError _ -> P.red "🆘  This part of the doc couldn't be rendered"

    sources srcs = P.group (P.indentN 4 (P.sep "\n\n" (map source srcs)))

    source ref = case ref of
      Term (_, obj) -> displayObject obj
      Type (_, obj) -> displayObject obj

    displayObject = \case
      BuiltinObject code -> syntax code
      MissingObject hash -> P.red ("😶  Missing source for: " <> lit (tShow hash))
      UserObject (Src _ full) -> syntax full

    lit :: Text -> Pretty
    lit = P.text

    nl :: Pretty
    nl = lit "\n"

    indent :: Pretty -> Pretty -> Pretty
    indent i = indent' i i

    indent' :: Pretty -> Pretty -> Pretty -> Pretty
    indent' initial afterNewline p = initial <> P.indentAfterNewline afterNewline p

-- | Code with its syntax highlighting, as lines (a literal can't have a newline in it).
syntax :: Server.SyntaxText -> Pretty
syntax (AnnotatedText segments) =
  P.lines (map (foldMap piece) (splitLines (toList segments)))
  where
    piece (text, color) = P.lit (AnnotatedText (Seq.singleton (Segment text color)))

    splitLines :: [Segment Server.Element] -> [[(Text, Maybe CT.Color)]]
    splitLines =
      reverse . map reverse . foldl' step [[]]
      where
        -- The lines so far, last first, each with its pieces last first.
        step :: [[(Text, Maybe CT.Color)]] -> Segment Server.Element -> [[(Text, Maybe CT.Color)]]
        step soFar (Segment text element) =
          let color = element >>= CT.defaultColors . elementForColor
           in case (soFar, Text.splitOn "\n" text) of
                (cur : done, first : rest) ->
                  foldl' (\acc line -> [(line, color)] : acc) (((first, color) : cur) : done) rest
                _ -> soFar

-- | The server's syntax elements say what they are, which is all the colors need (not what they refer to).
elementForColor :: Server.Element -> ST.Element Reference.Reference
elementForColor = \case
  Server.NumericLiteral -> ST.NumericLiteral
  Server.TextLiteral -> ST.TextLiteral
  Server.BytesLiteral -> ST.BytesLiteral
  Server.CharLiteral -> ST.CharLiteral
  Server.BooleanLiteral -> ST.BooleanLiteral
  Server.Blank -> ST.Blank
  Server.Var -> ST.Var
  Server.TypeReference _ _ -> ST.TypeReference Nothing dummy
  Server.DataConstructorReference _ _ -> ST.TermReference Nothing (Referent.Ref dummy)
  Server.AbilityConstructorReference _ _ -> ST.TermReference Nothing (Referent.Ref dummy)
  Server.TermReference _ _ -> ST.TermReference Nothing (Referent.Ref dummy)
  Server.Op op -> ST.Op op
  Server.AbilityBraces -> ST.AbilityBraces
  Server.ControlKeyword -> ST.ControlKeyword
  Server.TypeOperator -> ST.TypeOperator
  Server.BindingEquals -> ST.BindingEquals
  Server.TypeAscriptionColon -> ST.TypeAscriptionColon
  Server.DataTypeKeyword -> ST.DataTypeKeyword
  Server.DataTypeParams -> ST.DataTypeParams
  Server.Unit -> ST.Unit
  Server.DataTypeModifier -> ST.DataTypeModifier
  Server.UseKeyword -> ST.UseKeyword
  Server.UsePrefix -> ST.UsePrefix
  Server.UseSuffix -> ST.UseSuffix
  Server.HashQualifier _ -> ST.HashQualifier (HQ.NameOnly (Name.unsafeParseText "x"))
  Server.DelayForceChar -> ST.DelayForceChar
  Server.DelimiterChar -> ST.DelimiterChar
  Server.Parenthesis -> ST.Parenthesis
  Server.LinkKeyword -> ST.LinkKeyword
  Server.DocDelimiter -> ST.DocDelimiter
  Server.DocKeyword -> ST.DocKeyword
  Server.RecordFieldName name -> ST.RecordFieldName name
  Server.RecordFieldValueColon -> ST.RecordFieldValueColon
  Server.RecordExtraFields -> ST.RecordExtraFields

  where
    dummy = Reference.Builtin ""
