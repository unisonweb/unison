module Unison.Codebase.Editor.HandleInput.DebugFoldRanges (debugFoldRanges) where

import Control.Lens
import Control.Monad.Reader
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types qualified as LSP
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.FormatFile (TextReplacement (..))
import Unison.Codebase.Editor.HandleInput.FormatFile qualified as FormatFile
import Unison.Codebase.Editor.Output
import Unison.LSP.Conversions qualified as CV
import Unison.LSP.FoldingRange (foldingRangesForFile)
import Unison.Prelude
import Unison.Util.Range qualified as U

debugFoldRanges :: Cli ()
debugFoldRanges = do
  Cli.Env {loadSource} <- ask
  (filePath, _) <- Cli.expectLatestFile
  parsedFile <- Cli.expectLatestParsedFile
  let foldingRanges =
        foldingRangesForFile parsedFile
          & fmap
            ( \fr ->
                LSP.Range
                  (LSP.Position (fr ^. startLine) (fromMaybe 0 $ fr ^. startCharacter))
                  ( case (fr ^. endCharacter) of
                      Just c -> LSP.Position (fr ^. endLine) c
                      -- If there's no end char specified, go all the way to the beginning of the next line
                      Nothing -> LSP.Position ((fr ^. endLine) + 1) 0
                  )
            )
  sourceTxt <-
    liftIO (loadSource (Text.pack filePath)) >>= \case
      Cli.InvalidSourceNameError -> Cli.returnEarly $ InvalidSourceName filePath
      Cli.LoadError -> Cli.returnEarly $ SourceLoadFailed filePath
      Cli.LoadSuccess contents -> pure contents
  Cli.respond $ AnnotatedFoldRanges $ annotateRanges sourceTxt foldingRanges

-- | Annotate the bounds of a range within text using 《 and 》.
--
-- Useful for checking that computed ranges make sense against the source text.
--
-- >>> annotateRange "one\ntwo\nthree\nfour" [ LSP.Range (LSP.Position 1 0) (LSP.Position 2 3) ]
-- "one\n<two\nthr>ee\nfour"
annotateRanges :: Text -> [LSP.Range] -> Text
annotateRanges txt ranges =
  let replacements =
        ranges
          & foldMap
            ( \(LSP.Range start end) ->
                let startPos = CV.lspToUPos start
                    endPos = CV.lspToUPos end
                 in [ TextReplacement "《" (U.Range startPos startPos),
                      TextReplacement "》" (U.Range endPos endPos)
                    ]
            )
   in FormatFile.applyTextReplacements replacements txt
