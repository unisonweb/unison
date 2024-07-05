# demonstrating our new input parsing errors

```unison
x = 55
```

`handleNameArg` parse error in `add`
```ucm
scratch/main> add .

1:2:
  |
1 | .
  |  ^
unexpected end of input
expecting '`' or operator (valid characters: !$%&*+-/:<=>\^|~)


scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> add 1

  

scratch/main> ls

  1. lib/ (469 terms, 74 types)
  2. x    (Nat)

scratch/main> add 2

  ⊡ Ignored previously added definitions: x

```
todo:
```haskell

  SA.Name name -> pure name
  SA.NameWithBranchPrefix (Left _) name -> pure name
  SA.NameWithBranchPrefix (Right prefix) name -> pure $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
  SA.HashQualified hqname -> maybe (Left "can’t find a name from the numbered arg") pure $ HQ.toName hqname
  SA.HashQualifiedWithBranchPrefix (Left _) hqname -> pure $ HQ'.toName hqname
  SA.HashQualifiedWithBranchPrefix (Right prefix) hqname ->
    pure . Path.prefixNameIfRel (Path.AbsolutePath' prefix) $ HQ'.toName hqname
  SA.ShallowListEntry prefix entry ->
    pure . HQ'.toName . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
  SA.SearchResult mpath result ->
    maybe (Left "can’t find a name from the numbered arg") pure . HQ.toName $ searchResultToHQ mpath result
  otherNumArg -> Left . I.Formatted $ wrongStructuredArgument "a name" otherNumArg

```

aliasMany: skipped -- similar to `add`

```ucm
scratch/main> update arg

Adds everything in the most recently typechecked file to the
namespace, replacing existing definitions having the same name,
and attempts to update all the existing dependents accordingly.
If the process can't be completed automatically, the dependents
will be added back to the scratch file for your review.

```
aliasTerm
```scratch
/main> alias.term ##Nat.+ Nat.+

```

aliasTermForce,
aliasType,


todo:
```alias
Many,
api,
authLogin,
back,
branchEmptyInputPattern,
branchInputPattern,
branchRenameInputPattern,
branchesInputPattern,
cd,
clear,
clone,
compileScheme,
createAuthor,
debugClearWatchCache,
debugDoctor,
debugDumpNamespace,
debugDumpNamespaceSimple,
debugTerm,
debugTermVerbose,
debugType,
debugLSPFoldRanges,
debugFileHashes,
debugNameDiff,
debugNumberedArgs,
debugTabCompletion,
debugFuzzyOptions,
debugFormat,
delete,
deleteBranch,
deleteProject,
deleteNamespace,
deleteNamespaceForce,
deleteTerm,
deleteTermVerbose,
deleteType,
deleteTypeVerbose,
deleteVerbose,
dependencies,
dependents,
diffNamespace,
display,
displayTo,
docToMarkdown,
docs,
docsToHtml,
edit,
editNamespace,
execute,
find,
findIn,
findAll,
findInAll,
findGlobal,
findShallow,
findVerbose,
findVerboseAll,
sfind,
sfindReplace,
forkLocal,
help,
helpTopics,
history,
ioTest,
ioTestAll,
libInstallInputPattern,
load,
makeStandalone,
mergeBuiltins,
mergeIOBuiltins,
mergeOldInputPattern,
mergeOldPreviewInputPattern,
mergeOldSquashInputPattern,
mergeInputPattern,
mergeCommitInputPattern,
names False, -- names
names True, -- names.global
namespaceDependencies,
previewAdd,
previewUpdate,
printVersion,
projectCreate,
projectCreateEmptyInputPattern,
projectRenameInputPattern,
projectSwitch,
projectsInputPattern,
pull,
pullWithoutHistory,
push,
pushCreate,
pushExhaustive,
pushForce,
quit,
releaseDraft,
renameBranch,
renameTerm,
renameType,
moveAll,
reset,
resetRoot,
runScheme,
saveExecuteResult,
test,
testAll,
todo,
ui,
undo,
up,
update,
updateBuiltins,
updateOld,
updateOldNoPatch,
upgrade,
upgradeCommitInputPattern,
view,
viewGlobal,
viewReflog

```

