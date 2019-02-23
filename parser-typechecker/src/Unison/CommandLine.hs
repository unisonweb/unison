{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}


module Unison.CommandLine where

import           Data.Maybe                     ( catMaybes )
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )
import           Control.Applicative            ((<|>))
import           Control.Concurrent             (forkIO, killThread)
import           Control.Concurrent.STM         (atomically)
import           Control.Monad                  (forever, join, when)
import           Data.Foldable                  (toList, traverse_)
import           Data.List                      (isSuffixOf, sort)
import           Data.ListLike                  (ListLike)
import           Data.List.Extra                (nubOrdOn)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.String                    (IsString, fromString)
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import           Data.Text                      (Text)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import qualified System.Console.ANSI            as Console
import qualified System.Console.Haskeline       as Line
import qualified System.Console.Terminal.Size   as Terminal
import           System.Directory               (canonicalizePath, doesFileExist)
import           Unison.Codebase                (Codebase)
import qualified Unison.Codebase                as Codebase
import           Unison.Codebase.Branch         (Branch)
import qualified Unison.Codebase.Branch         as Branch
import           Unison.Codebase.Editor         (BranchName, DisplayThing (..),
                                                 Event (..), Input (..),
                                                 Output (..))
import qualified Unison.Codebase.Editor         as E
import qualified Unison.Codebase.Runtime        as Runtime
import qualified Unison.Codebase.Watch          as Watch
import           Unison.CommandLine.InputPattern (InputPattern(parse))
import qualified Unison.HashQualified           as HQ
import           Unison.Name                    (Name)
import qualified Unison.Name                    as Name
import qualified Unison.Names                   as Names
import           Unison.NamePrinter             (prettyName,
                                                 prettyHashQualified,
                                                 styleHashQualified
                                                )
import           Unison.Parser                  (Ann)
import           Unison.Parser                  (startingLine)
import qualified Unison.PrettyPrintEnv          as PPE
import           Unison.PrintError              (prettyParseError,
                                                 prettyTypecheckedFile,
                                                 renderNoteAsANSI)
import qualified Unison.Result                  as Result
import qualified Unison.Referent                as Referent
import qualified Unison.Reference               as Reference
import qualified Unison.TypePrinter             as TypePrinter
import           Unison.Term                    (Term)
import qualified Unison.TermPrinter             as TermPrinter
import qualified Unison.Codebase.TypeEdit       as TypeEdit
import qualified Unison.Codebase.TermEdit       as TermEdit
import qualified Unison.UnisonFile              as UF
import qualified Unison.Util.ColorText          as CT
import           Unison.Util.Monoid             (intercalateMap)
import qualified Unison.Util.Pretty             as P
import qualified Unison.Util.Relation           as R
import           Unison.Util.TQueue             (TQueue)
import qualified Unison.Util.TQueue             as Q
import           Unison.Var                     (Var)
import qualified Unison.Var                     as Var

watchPrinter :: Var v => Text -> PPE.PrettyPrintEnv -> Ann
                      -> Term v
                      -> Runtime.IsCacheHit
                      -> P.Pretty P.ColorText
watchPrinter src ppe ann term isHit = P.bracket $ let
  lines = Text.lines src
  lineNum = fromMaybe 1 $ startingLine ann
  lineNumWidth = length (show lineNum)
  extra = "     " -- for the ` | > ` after the line number
  line = lines !! (lineNum - 1)
  in P.lines [
    fromString (show lineNum) <> " | " <> P.text line,
    fromString (replicate lineNumWidth ' ')
      <> fromString extra <> "⧩"
      <> (if isHit then P.bold " (using cache)" else ""),
    P.indentN (lineNumWidth + length extra)
      . P.green . P.map fromString $ TermPrinter.prettyTop ppe term
  ]

allow :: FilePath -> Bool
allow = (||) <$> (".u" `isSuffixOf`) <*> (".uu" `isSuffixOf`)

watchFileSystem :: TQueue Event -> FilePath -> IO (IO ())
watchFileSystem q dir = do
  (cancel, watcher) <- Watch.watchDirectory dir allow
  t <- forkIO . forever $ do
    (filePath, text) <- watcher
    atomically . Q.enqueue q $ UnisonFileChanged (Text.pack filePath) text
  pure (cancel >> killThread t)

watchBranchUpdates :: IO (Branch, BranchName) -> TQueue Event -> Codebase IO v a -> IO (IO ())
watchBranchUpdates currentBranch q codebase = do
  (cancelExternalBranchUpdates, externalBranchUpdates) <-
    Codebase.branchUpdates codebase
  thread <- forkIO . forever $ do
    updatedBranches <- externalBranchUpdates
    (b, bname) <- currentBranch
    b' <- Codebase.getBranch codebase bname
    -- We only issue the event if the branch is different than what's already
    -- in memory. This skips over file events triggered by saving to disk what's
    -- already in memory.
    when (b' /= Just b) $
      atomically . Q.enqueue q . UnisonBranchChanged $ updatedBranches
  pure (cancelExternalBranchUpdates >> killThread thread)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: IsString s => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

backtickEOS :: IsString s => P.Pretty s -> P.Pretty s
backtickEOS s = P.group ("`" <> s <> "`.")

tip :: P.Pretty CT.ColorText -> P.Pretty CT.ColorText
tip s = P.column2 [("Tip:", P.wrap s)]

warn :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
warn s = emojiNote "⚠️" s

problem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
problem = emojiNote "❗️"

bigproblem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
bigproblem = emojiNote "‼️"

emojiNote :: (ListLike s Char, IsString s) => String -> P.Pretty s -> P.Pretty s
emojiNote lead s = P.group (fromString lead) <> "\n" <> P.wrap s

nothingTodo :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
nothingTodo s = emojiNote "😶" s

completion :: String -> Line.Completion
completion s = Line.Completion s s True

autoComplete :: String -> [String] -> [Line.Completion]
autoComplete q ss = fixup $
  completion <$> (id $ Codebase.sortedApproximateMatches q ss)
  where
  -- workaround for https://github.com/judah/haskeline/issues/100
  -- if the common prefix of all the completions is smaller than
  -- the query, we make all the replacements equal to the query,
  -- which will preserve what the user has typed
  fixup [] = []
  fixup [c] = [c]
  fixup cs@(h:t) = let
    commonPrefix (h1:t1) (h2:t2) | h1 == h2 = h1 : commonPrefix t1 t2
    commonPrefix _ _             = ""
    overallCommonPrefix =
      foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
    in if length overallCommonPrefix < length q
       then [ c { Line.replacement = q } | c <- cs ]
       else cs

parseInput
  :: Map String InputPattern -> [String] -> Either (P.Pretty CT.ColorText) Input
parseInput patterns ss = case ss of
  []             -> Left ""
  command : args -> case Map.lookup command patterns of
    Just pat -> parse pat args
    Nothing ->
      Left
        .  warn
        .  P.wrap
        $  "I don't know how to "
        <> P.group (fromString command <> ".")
        <> "Type `help` or `?` to get help."

prompt :: String
prompt = "> "

putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.border 2 p

putPrettyLn' :: P.Pretty CT.ColorText -> IO ()
putPrettyLn' p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.indentN 2 p

getAvailableWidth :: IO Int
getAvailableWidth =
  fromMaybe 80 . fmap (\s -> 100 `min` Terminal.width s) <$> Terminal.size
