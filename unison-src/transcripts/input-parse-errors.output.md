# demonstrating our new input parsing errors

```unison
x = 55
```

`handleNameArg` parse error in `add`
```ucm
scratch/main> add .

  Sorry, I'm not sure how to process your request. 1:2: | 1 | .
  | ^ unexpected end of input expecting '`' or operator (valid
  characters: !$%&*+-/:<=>\^|~)
  
  You can run `help add` for more information on using `add`
    `add` adds to the codebase all the definitions from the most recently typechecked file.

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
[ add,
      aliasMany,
      aliasTerm,
      aliasTermForce,
      aliasType,
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
    ]
