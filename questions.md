Task list:
- [x] error reporting for deserialization failures
- [x] error reporting for codebase consistency issues

| thing | v1↔v2 | v2↔v2s | desc. |
|-----|-----|-----|---|
|`Reference` | ✔ ✔ | . . |builtin or user-defined term or type|
|Recursive `Reference` | ✔ ✔ |  ||
|`Reference.Id` | ✔ ✔ | . . |user-defined term or type|
|Recursive `Reference.Id` | ✔ ✔ |  ||
|`ReferenceH` |  | . . | weak `Reference`|
|`Reference.IdH` |  | . . | weak `Reference.Id` |
|`Referent` |  | . . |builtin or user-defined function or constructor|
|Recursive `Referent` | ✔ ✔ |  ||
|`Referent.Id` | ✔← | . . | user-defined term or type |
|`ReferentH` |  | . . | weak `Referent` |
|`Referent.IdH` |  | . . | weak `Referent.Id` |
|`Hash` | ✔ ✔ | n/a |  |
|`ShortBranchHash` | →✔ |  | the base32 prefix of a branch/causal hash |
|`ConstructorType` | ✔ ✔ |  | data vs ability |
|`WatchKind` | ✔ ✔ |  | test vs regular |
|`Term.Term` | ✔ ✔ |  |  |
|`Decl.Decl` | ✔ ✔ |  |  |
|`Type` | ✔ ✔ |  |  |
|recursive `Type` | ✔ ✔ |  |  |
|`Symbol` | ✔ ✔ |  |  |
|`ShortHash` |  |  |  |
|`ShortHash.cycle` | →✔ | n/a | read pos and discard length |
|`ShortHash.cid` |  | n/a | haven't gotten to referents yet |
|`ABT.Term` | ✔ ✔ | n/a |  |
|`Causal`/`Branch` |  |  |  |
|`Branch0` |  |  |  |

## `Operations.hs`

### Exists Check

| Exists Check     | name                  |
| ---------------- | --------------------- |
| `HashId` by `Hash` | `objectExistsForHash` |

### Id Lookups
| ID lookups                   | Create if not found | Fail if not found             |
| ---------------------------- | ------------------- | ----------------------------- |
| `Text -> TextId`             |                     | `lookupTextId`                |
| `TextId -> Text`             |                     | `loadTextById`                |
| `Hash -> HashId`             |                     | `hashToHashId`                |
| `Hash -> ObjectId`           |                     | `hashToObjectId`              |
| `ObjectId -> Hash`           |                     | `loadHashByObjectId`          |
| `HashId -> Hash`             |                     | `loadHashByHashId`            |
| `CausalHashId -> CausalHash` |                     | `loadCausalHashById`          |
| `CausalHashId -> BranchHash` |                     | `loadValueHashByCausalHashId` |

### V2 ↔ Sqlite tranversals

| V2 ↔ Sqlite conversion traversals                            | name                                | notes             |
| ------------------------------------------------------------ | ----------------------------------- | ----------------- |
| `Reference' Text Hash` ↔ `Reference' TextId ObjectId`        | `c2sReference`, `s2cReference`      | normal references |
| `Reference.Id' Hash` ↔ `Reference.Id' ObjectId`              | `c2sReferenceId`, `s2cReferenceId`  | normal user references |
| `Reference' Text Hash` ↔ `Reference' TextId HashId`          | `c2hReference`, `h2cReference`      | weak references   |
| `Referent'' TextId ObjectId` → `Referent'' Text Hash` | `s2cReferent` | normal referent |
| `Referent'' TextId HashId` → `Referent'' Text Hash` | `h2cReferent` | weak referent |
| `Referent.Id' ObjectId` → `Referent.Id' Hash` | `s2cReferentId` | normal user referent |
| `TermEdit` | `s2cTermEdit` |  |
| `TermEdit.Typing` | `c2sTyping`, `s2cTyping` |  |
| `TypeEdit` | `s2cTypeEdit` |  |
| `Branch0` | `s2cBranch` |  |
| Branch diff |  | todo |
| `Patch` | `s2cPatch`, `c2lPatch` | `l` = local ids |
| `Patch` diff | `diffPatch` |  |
| User `Term` | `c2sTerm`, `s2cTermWithType`, `s2cTerm`, `s2cTypeOfTerm` |  |
| watch expressions | `c2wTerm`, `w2cTerm` |  |
| User `Decl` | `c2sDecl`, |  |

### Saving & loading?

| Saving & loading objects              | name                         | notes                                                       |
| ------------------------------------- | ---------------------------- | ----------------------------------------------------------- |
| `Patch` ↔ `PatchObjectId`             | `savePatch`, `loadPatchById` |                                                             |
| `H.Hash -> m (Maybe (Branch.Root m))` | `loadBranchByCausalHash`     | wip                                                         |
| `CausalHashId -> m (Maybe Branch)`    | `loadBranchByCausalHashId`   | equivalent to old `Branch0`, *not sure if actually useful?* |
| `BranchObjectId -> m (Branch m)`      | `loadBranchByObjectId`       | equivalent to old `Branch0`                                 |

### Deserialization helpers

|
|
|

| Deserialization helpers                                      |                                                              | notes |
| ------------------------------------------------------------ | ------------------------------------------------------------ | ----- |
| `decodeComponentLengthOnly`                                  | `ByteString -> m Word64`                                     |       |
| `decodeTermElementWithType`                                  | `C.Reference.Pos -> ByteString -> m (LocalIds, S.Term.Term, S.Term.Type)` |       |
| `decodeTermElementDiscardingTerm`, `decodeTermElementDiscardingType` | `-> m (LocalIds, S.Term.Term)`, `-> m (LocalIds, S.Term.Type)` |       |
| `decodeDeclElement`                                          | `Word64 -> ByteString -> m (LocalIds, S.Decl.Decl Symbol)`   |       |
| `deserializePatchObject`                                     | `PatchObjectId -> PatchFormat`                               |       |

### Reconstruct V1 data

| Reconstruct V1 data      |                                |
| ------------------------ | ------------------------------ |
| `getCycleLen`            | `Hash -> m Word64`             |
| `getDeclTypeByReference` | `Reference.Id -> DeclType`     |
| `componentByObjectId`    | `ObjectId -> m [Reference.Id]` |

### Codebase-y operations

|Codebase-y operations| type     | notes |
| ----------------------------- | ----------------------------- | ----- |
| `loadTermWithTypeByReference` | `Reference.Id -> MaybeT m (Term, Type)` |       |
| `loadTermByReference` | `Reference.Id -> MaybeT m Term` ||
| `loadTypeOfTermByTermReference` | `Reference.Id -> Maybe T m Type` ||
| `saveTermComponent` | `Hash -> [(Term, Type)] -> ObjectId` ||
| `loadDeclByReference` | `Reference.Id -> MaybeT m Decl` ||
| `saveDeclComponent` | `Hash -> [Decl] -> m ObjectId` ||
| `loadWatch` | `WatchKind -> Reference.Id -> MaybeT m Term` ||
| `saveWatch` | `WatchKind -> Reference.Id -> Term -> m ()` ||
| `termsHavingType` | `Reference -> m (Set Reference.Id)` ||
| `termsMentioningType` | " ||
| `termReferencesByPrefix` | `Text -> Maybe Word64 -> m [Reference.Id]` ||
| `declReferencesByPrefix` | `Text -> Maybe Word64 -> m [Reference.Id]` ||
| `saveRootBranch` | `Branch.Root m -> m (Db.BranchObjectId, Db.CausalHashId)` |wip|

### SqliteCodebase

| operation               | status | notes |
| ----------------------- | ------ | ----- |
| getTerm                 | ✔      |       |
| getTypeOfTerm           | ✔      |       |
| getTypeDeclaration      | ✔      |       |
| putTerm                 | ✔      |       |
| putTypeDeclaration      | ✔      |       |
| getRootBranch           | todo   |       |
| putRootBranch           | todo   |       |
| rootBranchUpdates       | todo   |       |
| getBranchForHash        | todo   |       |
| dependentsImpl          | ✔      |       |
| syncFromDirectory       | todo   |       |
| syncToDirectory         | todo   |       |
| watches                 | ✔      |       |
| getWatch                | ✔      |       |
| putWatch                | ✔      |       |
| getReflog               | ✔      |       |
| appendRefLog            | ✔      |       |
| termsOfTypeImpl         | todo   |       |
| termsMentioningTypeImpl | todo   |       |
| hashLength              | ✔      |       |
| termReferencesByPrefix  | ✔      |       |
| declReferencesByPrefix  | ✔      |       |
| referentsByPrefix       | ✔      |       |
| branchHashLength        | ✔      |       |
| branchHashesByPrefix    | ✔      |       |






Question: should the dependents / dependency index be a Relation Reference Reference (like now) a Relation Referent Reference?

Example, if you have `type Foo = Blah | Blah2 Nat`,



Advantages:

* If patches can replace constructors (not just types or terms), then having the index keyed by `Referent` lets you efficiently target the definitions that use those constructors.
* Also lets you find things that depend on `Blah2` (rather than depending on `Foo`).
