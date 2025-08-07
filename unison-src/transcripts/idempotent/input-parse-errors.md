# demonstrating our new input parsing errors

The example we'd had here was demonstrating the improved parsing error messages for `add`, but that command has been superseded by `update`.

todo:

``` haskell
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

aliasTerm

``` 
> alias.term ##Nat.+ Nat.+
```

aliasTermForce,
aliasType,

todo:

``` 

aliasMany,
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
