{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}


module Unison.CommandLine.InputPatterns where

-- import Debug.Trace
import           Data.List                       (intercalate)
import qualified Data.Set                        as Set
import           Data.String                     (fromString)
import qualified Data.Text                       as Text
import qualified Unison.Codebase                 as Codebase
import qualified Unison.Codebase.Branch          as Branch
import           Unison.Codebase.Editor          (Input (..))
import qualified Unison.Codebase.Editor          as E
import           Unison.CommandLine
import           Unison.CommandLine.InputPattern (ArgumentType (ArgumentType), InputPattern (InputPattern))
import qualified Unison.CommandLine.InputPattern as I
import qualified Unison.HashQualified as HQ
import qualified Unison.Names                    as Names
import qualified Unison.Util.ColorText           as CT
import           Unison.Util.Monoid              (intercalateMap)
import qualified Unison.Util.Pretty              as P

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ I.patternName i) <> fromString
    (if not . null $ I.aliases i
     then " (or " <> intercalate ", " (I.aliases i) <> ")"
     else ""),
  P.wrap $ I.help i ]

-- `example list ["foo", "bar"]` (haskell) becomes `list foo bar` (pretty)
makeExample :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExample p args =
  backtick (intercalateMap " " id (fromString (I.patternName p) : args))

makeExample' :: InputPattern -> P.Pretty CT.ColorText
makeExample' p = makeExample p []

makeExampleEOS ::
  InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExampleEOS p args = P.group $
  backtick (intercalateMap " " id (fromString (I.patternName p) : args)) <> "."

helpFor :: InputPattern -> Either (P.Pretty CT.ColorText) Input
helpFor p = I.parse help [I.patternName p]

updateBuiltins :: InputPattern
updateBuiltins = InputPattern "builtins.update" [] []
  "Adds all the builtins that are missing from this branch, and deprecate the ones that don't exist in this version of Unison."
  (const . pure $ UpdateBuiltinsI)

todo :: InputPattern
todo = InputPattern "todo" [] []
  "`todo` lists the work remaining in the current branch to complete an ongoing refactoring."
  (\ws -> if not $ null ws
             then Left $ warn "`todo` doesn't take any arguments."
             else pure $ TodoI)

add :: InputPattern
add = InputPattern "add" [] []
 "`add` adds to the codebase all the definitions from the most recently typechecked file."
 (\ws -> if not $ null ws
   then Left $ warn "`add` doesn't take any arguments."
   else pure $ SlurpFileI False)

view :: InputPattern
view = InputPattern "view" [] [(False, exactDefinitionQueryArg)]
      "`view foo` prints the definition of `foo`."
      (pure . ShowDefinitionI E.ConsoleLocation)

rename :: InputPattern
rename = InputPattern "rename" ["mv"]
    [(False, exactDefinitionQueryArg), (False, noCompletions)]
    "`rename foo bar` renames `foo` to `bar`."
    (\case
      [oldName, newName] -> Right $ RenameUnconflictedI
        allTargets
        (fromString oldName)
        (fromString newName)
      _ -> Left . P.warnCallout $ P.wrap
        "`rename` takes two arguments, like `rename oldname newname`.")

unname :: InputPattern
unname = InputPattern "unname" ["rm"]
    [(False, exactDefinitionQueryArg)]
    "`unname foo` removes the name `foo` from the namespace."
    (\case
      [] -> Left . P.warnCallout $ P.wrap
        "`unname` takes one or more arguments, like `unname name`."
      (Set.fromList . fmap HQ.fromString -> query) -> Right $ UnnameAllI query
    )

alias :: InputPattern
alias = InputPattern "alias" ["cp"]
    [(False, exactDefinitionQueryArg), (False, noCompletions)]
    "`alias foo bar` introduces `bar` with the same definition as `foo`."
    (\case
      [oldName, newName] -> Right $ AliasUnconflictedI
        allTargets
        (fromString oldName)
        (fromString newName)
      _ -> Left . warn $ P.wrap
        "`alias` takes two arguments, like `alias oldname newname`."
    )

update :: InputPattern
update = InputPattern "update" [] []
    "`update` works like `add`, except if a definition in the file has the same name as an existing definition, the name gets updated to point to the new definition. If the old definition has any dependents, `update` will add those dependents to a refactoring session."
    (\ws -> if not $ null ws
      then Left $ warn "`update` doesn't take any arguments."
      else pure $ SlurpFileI True
    )

branch :: InputPattern
branch = InputPattern "branch" [] [(True, branchArg)]
    (P.wrapColumn2
      [ ("`branch`",      "lists all branches in the codebase.")
      , ( "`branch foo`", "switches to the branch named 'foo', creating it first if it doesn't exist.")
      ]
    )
    (\case
      []  -> pure ListBranchesI
      [b] -> pure . SwitchBranchI $ Text.pack b
      _ -> Left . warn . P.wrap $  "Use `branch` to list all branches "
             <> "or `branch foo` to switch to or create the branch 'foo'."
    )

deleteBranch,replace,resolve :: InputPattern
deleteBranch = InputPattern "branch.delete" [] [(True, branchArg)]
  "`branch.delete <foo>` deletes the branch `foo`"
  (\(fmap Text.pack -> ws) -> case ws of
    [] -> helpFor deleteBranch
    ws -> pure $ DeleteBranchI ws)

replace = InputPattern "replace" []
          [ (False, exactDefinitionQueryArg)
          , (False, exactDefinitionQueryArg) ]
  (makeExample replace ["foo#abc", "foo#def"] <> "begins a refactor to replace"
    <> "uses of `foo#abc` with `foo#def`")
  (\_ -> Left . warn . P.wrap $ "This command hasn't been implemented. 😞")
resolve = InputPattern "resolve" [] [(False, exactDefinitionQueryArg)]
  (makeExample resolve ["foo#abc"] <> "sets `foo#abc` as the canonical `foo`,"
   <> "in cases of conflict, and begins a refactor to replace references to all"
   <> "other `foo`s to `foo#abc`.")
  (\_ -> Left . warn . P.wrap $ "This command hasn't been implemented. 😞")

help :: InputPattern
help = InputPattern
    "help" ["?"] [(True, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    (\case
      [] -> Left $ intercalateMap "\n\n" showPatternHelp validInputs
      [cmd] -> case lookup cmd (commandNames `zip` validInputs) of
        Nothing  -> Left . warn $ "I don't know of that command. Try `help`."
        Just pat -> Left $ I.help pat
      _ -> Left $ warn "Use `help <cmd>` or `help`.")

validInputs :: [InputPattern]
validInputs =
  [ help
  , add
  , branch
  , InputPattern "fork" [] [(False, branchArg)]
    (P.wrap
     "`fork foo` creates the branch 'foo' as a fork of the current branch.")
    (\case
      [b] -> pure . ForkBranchI $ Text.pack b
      _ -> Left . warn . P.wrap $ "Use `fork foo` to create the branch 'foo'"
                                <> "from the current branch."
    )
  , InputPattern "find" ["ls","list"] [(True, fuzzyDefinitionQueryArg)]
    (P.wrapColumn2
      [ ("`find`"
        , "lists all definitions in the current branch.")
      , ( "`find foo`"
        , "lists all definitions with a name similar to 'foo' in the current branch.")
      , ( "`find foo bar`"
        , "lists all definitions with a name similar to 'foo' or 'bar' in the current branch.")
      , ( "`find -l foo bar`"
        , "lists all definitions with a name similar to 'foo' or 'bar' in the current branch, along with their hashes and aliases.")
      ]
    )
    (pure . SearchByNameI)
  , InputPattern "merge" [] [(False, branchArg)]
    "`merge foo` merges the branch 'foo' into the current branch."
    (\case
      [b] -> pure . MergeBranchI $ Text.pack b
      _ -> Left . warn . P.wrap $
        "Use `merge foo` to merge the branch 'foo' into the current branch."
    )
  , view
  , InputPattern "edit" [] [(False, exactDefinitionQueryArg)]
      "`edit foo` prepends the definition of `foo` to the top of the most recently saved file."
      (pure . ShowDefinitionI E.LatestFileLocation)
  , rename
  , unname
  , alias
  , update
  , InputPattern "propagate" [] []
    "`propagate` rewrites any definitions that depend on definitions with type-preserving edits to use the updated versions of these dependencies."
    (const $ pure PropagateI)
  , todo
  , InputPattern "execute" [] [(True, noCompletions)]
    "`execute foo` evaluates the Unison expression `foo` of type `()` with access to the `IO` ability."
    (\ws -> if null ws
               then Left $ warn "`execute` needs a Unison language expression."
               else pure . ExecuteI $ intercalate " " ws)
  , InputPattern "quit" ["exit"] []
      "Exits the Unison command line interface."
      (\case
        [] -> pure QuitI
        _  -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."
      )
  , updateBuiltins
  , InputPattern "edit.list" [] []
      "Lists all the edits in the current branch."
      (const . pure $ ListEditsI)
  , deleteBranch
  ]

allTargets :: Set.Set Names.NameTarget
allTargets = Set.fromList [Names.TermName, Names.TypeName]

commandNames :: [String]
commandNames = I.patternName <$> validInputs

commandNameArg :: ArgumentType
commandNameArg =
  ArgumentType "command" $ \q _ _ -> pure (fuzzyComplete q commandNames)

branchArg :: ArgumentType
branchArg = ArgumentType "branch" $ \q codebase _b -> do
  branches <- Codebase.branches codebase
  let bs = Text.unpack <$> branches
  pure $ fuzzyComplete q bs

fuzzyDefinitionQueryArg :: ArgumentType
fuzzyDefinitionQueryArg =
  ArgumentType "fuzzy definition query" $ \q _ (Branch.head -> b) -> do
    pure $ fuzzyCompleteHashQualified b q

exactDefinitionQueryArg :: ArgumentType
exactDefinitionQueryArg =
  ArgumentType "definition query" $ \q _ (Branch.head -> b) -> do
    pure $ autoCompleteHashQualified b q

noCompletions :: ArgumentType
noCompletions = ArgumentType "a word" I.noSuggestions
