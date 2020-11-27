{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.Git where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import Data.String.Here (iTrim)
import qualified Data.Text as Text
import EasyTest
import Shellmet ()
import System.Directory (doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import Unison.Codebase (BuiltinAnnotation, Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.FileCodebase as FC
import Unison.Codebase.FileCodebase.Common (SyncToDir, formatAnn)
import Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex as SlimCopyRegenerateIndex
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Serialization.V1 as V1
import qualified Unison.Codebase.SyncMode as SyncMode
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Parser (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import qualified Unison.Util.Cache as Cache
import Unison.Var (Var)

test :: Test ()
test =
  scope "git" . tests $
    [ testPull,
      testPush,
      syncComplete,
      syncTestResults
    ]

traceTranscriptOutput :: Bool
traceTranscriptOutput = False

-- | make sure that a definition present in the target dir doesn't prevent
-- syncing of its dependencies
syncComplete :: Test ()
syncComplete = scope "syncComplete" $ do
  tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "syncComplete"

  targetDir <- io $ Temp.createTempDirectory tmp "target"
  let delete = io . traverse_ removeFile . fmap (targetDir </>)
      observe title expectation files = scope title . for_ files $ \path ->
        scope (makeTitle path) $ io (doesFileExist $ targetDir </> path) >>= expectation

  cache <- io Cache.nullCache
  codebase <- io $ snd <$> initCodebase cache tmp "codebase"

  runTranscript_
    tmp
    codebase
    cache
    [iTrim|
```ucm:hide
.builtin> alias.type ##Nat Nat
.builtin> alias.term ##Nat.+ Nat.+
```
```unison
pushComplete.a.x = 3
pushComplete.b.c.y = x + 1
```
```ucm
.> add
.> history pushComplete.b
```
|]

  -- sync pushComplete.b to targetDir
  -- observe that pushComplete.b.c and x exist
  b <-
    io (Codebase.getRootBranch codebase)
      >>= either
        (crash . show)
        (pure . Branch.getAt' (Path $ Seq.fromList ["pushComplete", "b"]))
  io $ Codebase.syncToDirectory codebase targetDir SyncMode.ShortCircuit b
  observe "initial" expect files

  -- delete pushComplete.b.c (#5lk9autjd5)
  -- delete x                (#msp7bv40rv)
  -- observe that pushComplete.b.c and x are now gone
  delete files
  observe "deleted" (expect . not) files

  -- sync again with ShortCircuit
  -- observe that pushComplete.b.c and x are still missing.
  -- `c` is short-circuited at `b`, and `x` is short-circuited
  -- at both `pushComplete` and `y`.
  io $ Codebase.syncToDirectory codebase targetDir SyncMode.ShortCircuit b
  observe "short-circuited" (expect . not) files

  -- sync again with Complete
  -- observe that pushComplete.b.c and x are back
  io $ Codebase.syncToDirectory codebase targetDir SyncMode.Complete b
  observe "complete" expect files

  -- if we haven't crashed, clean up!
  io $ removeDirectoryRecursive tmp
  where
    files =
      [ ".unison/v1/paths/5lk9autjd5911i8m52vsvf3si8ckino03gqrks1fokd9lf9kvc4id9gmuudjk4q06j3rkhi83o9g47mde5amchc1leqlskjs391m7fg.ub",
        ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/type.ub",
        ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/compiled.ub"
      ]

syncTestResults :: Test ()
syncTestResults = scope "syncTestResults" $ do
  -- put all our junk into here
  tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "syncTestResults"

  targetDir <- io $ Temp.createTempDirectory tmp "target"
  cache <- io Cache.nullCache
  codebase <- io $ snd <$> initCodebase cache tmp "codebase"

  runTranscript_
    tmp
    codebase
    cache
    [iTrim|
```ucm
.> builtins.merge
```
```unison
test> tests.x = [Ok "Great!"]
```
```ucm
.> add
```
|]

  {-
    .> history tests
      ⊙ #0bnfrk7cu4
    .> debug.file
      tests.x#2c2hpa2jm1
    .>
  -}

  b <-
    io (Codebase.getRootBranch codebase) >>= \case
      Left e -> crash $ show e
      Right b -> pure b

  io $
    Codebase.syncToDirectory
      codebase
      targetDir
      SyncMode.ShortCircuit
      (Branch.getAt' (Path $ pure "tests") b)

  scope "target-should-have" $
    for targetShouldHave $ \path ->
      scope (makeTitle path) $ io (doesFileExist $ targetDir </> path) >>= expect

  -- if we haven't crashed, clean up!
  io $ removeDirectoryRecursive tmp
  where
    targetShouldHave =
      [ ".unison/v1/paths/0bnfrk7cu44q0vvaj7a0osl90huv6nj01nkukplcsbgn3i09h6ggbthhrorm01gpqc088673nom2i491fh9rtbqcc6oud6iqq6oam88.ub",
        ".unison/v1/terms/#2c2hpa2jm1101sq10k4jqhpmv5cvvgtqm8sf9710kl8mlrum5b6i2d0rdtrrpg3k1ned5ljna1rvomjte7rcbpd9ouaqcsit1n1np3o/type.ub",
        ".unison/v1/terms/#2c2hpa2jm1101sq10k4jqhpmv5cvvgtqm8sf9710kl8mlrum5b6i2d0rdtrrpg3k1ned5ljna1rvomjte7rcbpd9ouaqcsit1n1np3o/compiled.ub",
        ".unison/v1/watches/test/#2c2hpa2jm1101sq10k4jqhpmv5cvvgtqm8sf9710kl8mlrum5b6i2d0rdtrrpg3k1ned5ljna1rvomjte7rcbpd9ouaqcsit1n1np3o.ub"
      ]

-- goal of this test is to make sure that pull doesn't grab a ton of unneeded
-- dependencies
testPull :: Test ()
testPull = scope "pull" $ do
  branchCache <- io $ Branch.boundedCache 4096
  -- let's push a broader set of stuff, pull a narrower one (to a fresh codebase)
  -- and verify that we have the definitions we expected and don't have some of
  -- the ones we didn't expect.

  -- put all our junk into here
  tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "git-pull"

  -- initialize author and user codebases
  authorCodebase <- io $ snd <$> initCodebase branchCache tmp "author"
  (userDir, userCodebase) <- io $ initCodebase branchCache tmp "user"

  -- initialize git repo
  let repo = tmp </> "repo.git"
  io $ "git" ["init", "--bare", Text.pack repo]

  -- run author/push transcript
  runTranscript_
    tmp
    authorCodebase
    branchCache
    [iTrim|
```ucm:hide
.builtin> alias.type ##Nat Nat
.builtin> alias.term ##Nat.+ Nat.+
```
```unison
unique type outside.A = A Nat
unique type outside.B = B Nat Nat
outside.c = 3
outside.d = 4

unique type inside.X = X outside.A
inside.y = c + c
```
```ucm
.myLib> debug.file
.myLib> add
.myLib> push ${repo}
```
|]

  -- check out the resulting repo so we can inspect it
  io $ "git" ["clone", Text.pack repo, Text.pack $ tmp </> "repo"]

  scope "git-should-have" $
    for gitShouldHave $ \path ->
      scope (makeTitle path) $ io (doesFileExist $ tmp </> "repo" </> path) >>= expect

  -- run user/pull transcript
  runTranscript_
    tmp
    userCodebase
    branchCache
    [iTrim|
```ucm:hide
.builtin> alias.type ##Nat Nat
.builtin> alias.term ##Nat.+ Nat.+
```
```ucm
.yourLib> pull ${repo}:.inside
```
  |]

  -- inspect user codebase
  scope "user-should-have" $
    for userShouldHave $ \path ->
      scope (makeTitle path) $ io (doesFileExist $ userDir </> path) >>= expect
  scope "user-should-not-have" $ -- this definitely won't pass with current implementation
    for userShouldNotHave $ \path ->
      scope (makeTitle path) $ io (doesFileExist $ userDir </> path) >>= expect . not

  -- if we haven't crashed, clean up!
  io $ removeDirectoryRecursive tmp
  where
    gitShouldHave =
      userShouldHave ++ userShouldNotHave
        ++ [ ".unison/v1/paths/p8ahoj90hkdjpvlcu60f6ks7q2is1uqbn1e74k5qn4jt1qmrhk0a62e9b2gamm6qmjdii478la2fha5pnnuvhit2b1mp439od7mrqmg.ub"
           ]
    userShouldHave =
      [ ".unison/v1/type-mentions-index/_builtin/Nat/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg",
        ".unison/v1/type-mentions-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo",
        ".unison/v1/type-mentions-index/_builtin/Nat/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0#d0",
        ".unison/v1/type-mentions-index/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0#d0",
        ".unison/v1/type-mentions-index/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0#d0",
        ".unison/v1/type-mentions-index/#2po5mnhi28fbs9fecf4ceq4q9htbfcgkl3ljnkhmhq30ec7m5h77fpl1ec96it21690ju6gnhkj8sqr2entn0cu1gfvl8rfddohk6ug/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0#d0",
        ".unison/v1/type-mentions-index/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0#d0",
        ".unison/v1/type-mentions-index/#k1lik85h1sgcpqura4riuipjq3mtkkuu5slida6q2lkg028fd7jn12kufrk2sqrtbftq3snteeh8l9o984mhnurmo3arr5j4d7hg5oo/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0#d0",
        ".unison/v1/types/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0/compiled.ub",
        ".unison/v1/types/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0/compiled.ub",
        ".unison/v1/dependents/_builtin/Nat.+/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg",
        ".unison/v1/dependents/_builtin/Nat/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg",
        ".unison/v1/dependents/_builtin/Nat/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0",
        ".unison/v1/dependents/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo",
        ".unison/v1/dependents/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0",
        ".unison/v1/dependents/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg",
        ".unison/v1/terms/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg/type.ub",
        ".unison/v1/terms/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg/compiled.ub",
        ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/type.ub",
        ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/compiled.ub",
        ".unison/v1/type-index/_builtin/Nat/#omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg",
        ".unison/v1/type-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo",
        ".unison/v1/type-index/#2po5mnhi28fbs9fecf4ceq4q9htbfcgkl3ljnkhmhq30ec7m5h77fpl1ec96it21690ju6gnhkj8sqr2entn0cu1gfvl8rfddohk6ug/#p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0#d0",
        ".unison/v1/type-index/#k1lik85h1sgcpqura4riuipjq3mtkkuu5slida6q2lkg028fd7jn12kufrk2sqrtbftq3snteeh8l9o984mhnurmo3arr5j4d7hg5oo/#19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0#d0",
        ".unison/v1/paths/esvotl1kr2aqo4tkq7p6lp2chkepmg7n3im1t6hqgd93slk97kops8idp7fj7i57pakvg6lhk0efsco6s2vvtql0jffomm8tvngogd0.ub",
        ".unison/v1/paths/ucnhqspklepn3ihu1o3ph2or9hsrhcpoav93v4gi1v97ttoc2vuup173mcophp8r90r0j3k5mg2knlqr85gdq1dseh8mt5t94c4am4o.ub"
      ]
    userShouldNotHave =
      [ ".unison/v1/type-mentions-index/_builtin/Nat/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58#d0",
        ".unison/v1/type-mentions-index/_builtin/Nat/#52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg",
        ".unison/v1/type-mentions-index/#ap7kd0rc80kp7vjosb0im9j365kgbqhqhj3fv4ufs7bv5b3ed0d4jleqqulu74lj60fuht1oqr117u17jnp1ql8te67vjit95p7k80o/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58#d0",
        ".unison/v1/type-mentions-index/#7krpfrn5gm7m3beiho9jmar3dojnj7mrksnjbmh8i0p9hbmekqv21kqrtsr5lq4rr4n0sako6e7lmt8k2a39senua9efjfo7214s3q8/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58#d0",
        ".unison/v1/type-mentions-index/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58#d0",
        ".unison/v1/types/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58/compiled.ub",
        ".unison/v1/dependents/_builtin/Nat/#52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg",
        ".unison/v1/dependents/_builtin/Nat/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58",
        ".unison/v1/terms/#52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg/type.ub",
        ".unison/v1/terms/#52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg/compiled.ub",
        ".unison/v1/type-index/_builtin/Nat/#52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg",
        ".unison/v1/type-index/#ap7kd0rc80kp7vjosb0im9j365kgbqhqhj3fv4ufs7bv5b3ed0d4jleqqulu74lj60fuht1oqr117u17jnp1ql8te67vjit95p7k80o/#aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58#d0",
        ".unison/v1/paths/000fqlrbs84nui3o3sp04s32vsbq39iv9foqvs4c38ajki3re86v72s0j5deqtcdqqml9r8e50lcmld2j8ncj7a1fqnqb4pvcaphcu0.ub",
        ".unison/v1/paths/d8ercjm1ol1htu82nmr37ejru1lt7lrl03d5j0u0dp0g2a98nl6n8abdjpf2jkvjuoq4u2qrhn99ps6fiqqn60b0tni7nkp7o593sr0.ub",
        ".unison/v1/paths/bih5ebeug86npp1n0mp51vi7a902ma6m1r3s1ehhfhpc0m71le2fdge8nftte5fuambfo2r753bjnguq5e3p6mip7incmghkho643pg.ub"
      ]

-- path "[inside]." esvotl1kr2aqo4tkq7p6lp2chkepmg7n3im1t6hqgd93slk97kops8idp7fj7i57pakvg6lhk0efsco6s2vvtql0jffomm8tvngogd0
-- path "[inside].X" ucnhqspklepn3ihu1o3ph2or9hsrhcpoav93v4gi1v97ttoc2vuup173mcophp8r90r0j3k5mg2knlqr85gdq1dseh8mt5t94c4am4o.ub
-- type outside.A  #19lkp9i61r793nmuup58b7g3ssmdip8e76ko3n1r0pjk4ld71euh2prdidhnllkt5lsk0tnpn8lv44t7h7q04eeaqvvh75dg4gi77h0
-- type outside.B  #aocoefu4taepnvd3gsbtgo5rc6a5oa109e0mfqjfg91m422he1m6nugnq1hb4nedvh32r244v6t0a7jq8k30nt92109466udv78cf58
-- outside.c       #msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo
-- outside.d       #52addbrohuv4kimiv8n6v00vsv46g3pig4imoor34lojgla9bo2tdcumh07pasuo4lmfnab53s1ulj9toai7963spt2jkk5h1qfdnlg
-- type inside.X   #p8f8gc2lehvr6ddq6ggittuo3t330q2pkou9gr1408r7o7r33is5cffstl5p916rbui2sa53iqnppsgsuskgodvd5003550roflmvn0
-- inside.y        #omqnfettvjqrjmpl2mn7s30g94gogjjoi6hd3ob6394r71mkidbg0kqtgtbkjkmhbqvipqed9ql4b0o7kp68c560e3onb0v3lbv6bjg
-- paths: esvot|ucnhq
-- want: A, c, X, y: 19lkp|msp7b|p8f8g|omqnf
-- no:  B, d: aocoe|52add|

-- initialize a fresh codebase
initCodebaseDir :: Branch.Cache IO -> FilePath -> String -> IO CodebasePath
initCodebaseDir branchCache tmpDir name = fst <$> initCodebase branchCache tmpDir name

initCodebase :: Branch.Cache IO -> FilePath -> String -> IO (CodebasePath, Codebase IO Symbol Ann)
initCodebase branchCache tmpDir name = do
  let codebaseDir = tmpDir </> name
  c <- FC.initCodebase branchCache codebaseDir
  pure (codebaseDir, c)

-- run a transcript on an existing codebase
runTranscript_ :: MonadIO m => FilePath -> Codebase IO Symbol Ann -> Branch.Cache IO -> String -> m ()
runTranscript_ tmpDir c branchCache transcript = do
  let configFile = tmpDir </> ".unisonConfig"
  -- transcript runner wants a "current directory" for I guess writing scratch files?
  let cwd = tmpDir </> "cwd"
  let err err = error $ "Parse error: \n" <> show err

  -- parse and run the transcript
  flip (either err) (TR.parse "transcript" (Text.pack transcript)) $ \stanzas ->
    void . liftIO $
      TR.run Nothing cwd configFile stanzas c branchCache
        >>= when traceTranscriptOutput . traceM . Text.unpack

-- goal of this test is to make sure that push works correctly:
-- the destination should contain the right definitions from the namespace,
-- unnamed transitive dependencies (terms and types),
-- dependents, type, and type mentions indices.
testPush :: Test ()
testPush = scope "push" $ do
  branchCache <- io $ Branch.boundedCache 4096
  tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "git-push"

  -- initialize a fresh codebase named "c"
  (codebasePath, c) <- io $ initCodebase branchCache tmp "c"

  -- Run the "setup transcript" to do the adds and updates; everything short of
  -- pushing.
  runTranscript_ tmp c branchCache setupTranscript

  -- now we'll try pushing multiple ways.
  for_ pushImplementations $ \(implName, impl) -> scope implName $ do
    -- initialize git repo
    let repoGit = tmp </> (implName ++ ".git")
    io $ "git" ["init", "--bare", Text.pack repoGit]

    -- push one way!
    codebase <- io $ FC.codebase1' impl branchCache V1.formatSymbol formatAnn codebasePath
    runTranscript_ tmp codebase branchCache (pushTranscript repoGit)

    -- check out the resulting repo so we can inspect it
    io $ "git" ["clone", Text.pack repoGit, Text.pack $ tmp </> implName]

    -- inspect it
    for_ groups $ \(group, list) -> scope group $
      for_ list $ \(title, path) ->
        scope title $
          io (doesFileExist $ tmp </> implName </> path) >>= expect

    for_ notGroups $ \(group, list) -> scope group $
      for_ list $ \(title, path) ->
        scope title $
          io (fmap not . doesFileExist $ tmp </> implName </> path) >>= expect

  -- if we haven't crashed, clean up!
  io $ removeDirectoryRecursive tmp
  where
    setupTranscript =
      [iTrim|
    ```ucm
    .> builtins.merge
    ```
    ```unison:hide
    --#0n4pbd0q9u
    type outside.A = A Nat outside.B

    --#muulibntaq
    type outside.B = B Int

    --#msp7bv40rv
    outside.c = 3

    --#6cdi7g1oi2
    outside.d = c < (p + 1)

    --#4idrjau939
    type inside.M = M outside.A

    --#fiupm7pl7o
    inside.p = c

    --#l5pndeifuh
    inside.q x = x + p * p

    inside.r = d
    ```
    ```ucm
    .foo> add
    ```
    ```unison:hide
    r = false
    ```
    ```ucm
    .foo.inside> update
    ```
  |]
    pushTranscript repo =
      [iTrim|
    ```ucm
    .foo.inside> push ${repo}
    ```
  |]

    pushImplementations ::
      (MonadIO m, Var v, BuiltinAnnotation a) =>
      [(String, SyncToDir m v a)]
    pushImplementations =
      [ ("SlimCopyRegenerateIndex", SlimCopyRegenerateIndex.syncToDirectory)
      ]

    groups =
      [ ("types", types),
        ("terms", terms),
        ("branches", branches),
        ("patches", patches),
        ("dependentsIndex", dependentsIndex),
        ("typeIndex", typeIndex),
        ("typeMentionsIndex", typeMentionsIndex)
      ]

    notGroups =
      [("notBranches", notBranches)]

    types =
      [ ("M", ".unison/v1/types/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg/compiled.ub"),
        ("A", ".unison/v1/types/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/compiled.ub"),
        ("B", ".unison/v1/types/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/compiled.ub")
      ]

    terms =
      [ ("p (type)", ".unison/v1/terms/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/type.ub"),
        ("p (compiled)", ".unison/v1/terms/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/compiled.ub"),
        ("c (type)", ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/type.ub"),
        ("c (compiled)", ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/compiled.ub"),
        ("d (type)", ".unison/v1/terms/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/type.ub"),
        ("d (compiled)", ".unison/v1/terms/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/compiled.ub"),
        ("q (type)", ".unison/v1/terms/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8/type.ub"),
        ("q (compiled)", ".unison/v1/terms/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8/compiled.ub"),
        ("r (type)", ".unison/v1/terms/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8/type.ub"),
        ("r (compiled)", ".unison/v1/terms/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8/compiled.ub"),
        ("r' (type)", ".unison/v1/terms/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo/type.ub"),
        ("r' (compiled)", ".unison/v1/terms/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo/compiled.ub")
      ]

    branches =
      [ ("_head", ".unison/v1/paths/_head/pciob2qnondela4h4u1dtk9pvbc9up7qed0j311lkomordjah2lliddis7tdl76h5mdbs5ja10tm8kh2o3sni1bu2kdsqtm4fkv5288"),
        (".foo.inside", ".unison/v1/paths/pciob2qnondela4h4u1dtk9pvbc9up7qed0j311lkomordjah2lliddis7tdl76h5mdbs5ja10tm8kh2o3sni1bu2kdsqtm4fkv5288.ub"),
        (".foo.inside'", ".unison/v1/paths/0ufjqqmabderbejfhrled8i4lirgpqgimejbkdnk1m9t90ibj25oi7g1h2adougdqhv72sv939eq67ur77n3qciajh0reiuqs68th00.ub"),
        (".foo.inside.M", ".unison/v1/paths/i2p08iv1l50fc934gh6kea181kvjnt3kdgiid5c4r5016kjuliesji43u4j4mjvsne3qvmq43puk9dkm61nuc542n7pchsvg6t0v55o.ub"),
        ("<empty>", ".unison/v1/paths/7asfbtqmoj56pq7b053v2jc1spgb8g5j4cg1tj97ausi3scveqa50ktv4b2ofoclnkqmnl18vnt5d83jrh85qd43nnrsh6qetbksb70.ub")
      ]

    notBranches =
      [ (".", ".unison/v1/paths/9r7l4k8ks1tog088fg96evunq1ednlsskf2lh0nacpe5n00khcrl8f1g5sevm7cqd3s64cj22ukvkh2fflm3rhhkn2hh2rj1n20mnm8.ub"),
        (".'", ".unison/v1/paths/llton7oiormlimkdmqjdr8tja12i6tebii7cmfd7545b7mt1sb02f9usjqnjd6iaisnn1ngpsl76hfg024l8dlult3s6stkt28j42sg.ub"),
        (".''", ".unison/v1/paths/givahf3f6fu8vv07kglsofdcoem7q5dm4rracr78a5didjc4pq2djh2rfdo5sn7nld2757oi02a4a07cv9rk4peafhh76nllcp8l1n8.ub"),
        (".foo", ".unison/v1/paths/a8dt4i16905fql2d4fbmtipmj35tj6qmkq176dlnsn6klh0josr255eobn0d3f0aku360h0em6oit9ftjpq3vhcdap8bgpqr79qne58.ub"),
        (".foo'", ".unison/v1/paths/l3r86dvdmbe2lsinh213tp9upm5qjtk17iep3n5mah7qg5bupj1e7ikpv1iqbgegp895r0krlo0u2c4nclvfvch3e6kspu766th6tqo.ub"),
        (".foo.outside", ".unison/v1/paths/s6iquav10f69pvrpj6rtm7vcp6fs6hgnnmjb1qs00n594ljugbf2qtls93oc4lvb3kjro8fpakoua05gqido4haj4m520rip2gu2hvo.ub"),
        (".foo.outside.A", ".unison/v1/paths/2i1lh7pntl3rqrtn4c10ajdg4m3al1rqm6u6ak5ak6urgsaf6nhqn2olt3rjqj5kcj042h8lqseguk3opp019hc7g8ncukds25t9r40.ub"),
        (".foo.outside.B", ".unison/v1/paths/jag86haq235jmifji4n8nff8dg1ithenefs2uk5ms6b4qgj9pfa9g40vs4kdn3uhm066ni0bvfb7ib9tqtdgqcn90eadl7282nqqbc0.ub")
      ]

    patches =
      [("patch", ".unison/v1/patches/96b419pm6l896ncmef9kqkpj29gq205amsl6prsl2num29thpn9fej8v8ndcmubadv5hehege4s43n3ljbifsnna92lpeuacq9fm3qo.up")]

    dependentsIndex =
      [ ("Nat <- A", ".unison/v1/dependents/_builtin/Nat/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8"),
        ("B <- A", ".unison/v1/dependents/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8"),
        ("Int <- B", ".unison/v1/dependents/_builtin/Int/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0"),
        ("Nat <- c", ".unison/v1/dependents/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo"),
        ("Boolean <- d", ".unison/v1/dependents/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("Nat <- d", ".unison/v1/dependents/_builtin/Nat/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("Nat.+ <- d", ".unison/v1/dependents/_builtin/Nat.+/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("Universal.< <- d", ".unison/v1/dependents/_builtin/Universal.$less-than$/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("c <- d", ".unison/v1/dependents/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("p <- d", ".unison/v1/dependents/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("A <- M", ".unison/v1/dependents/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg"),
        ("Nat <- p", ".unison/v1/dependents/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500"),
        ("c <- p", ".unison/v1/dependents/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500"),
        ("Nat <- q", ".unison/v1/dependents/_builtin/Nat/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Nat.* <- q", ".unison/v1/dependents/_builtin/Nat.$star$/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Nat.+ <- q", ".unison/v1/dependents/_builtin/Nat.+/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("p <- q", ".unison/v1/dependents/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Boolean <- r", ".unison/v1/dependents/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8"),
        ("d <- r", ".unison/v1/dependents/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8"),
        ("Boolean <- r'", ".unison/v1/dependents/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
      ]

    typeIndex =
      [ ("(Nat -> B -> A) <- A#0", ".unison/v1/type-index/#6n4ih159cqcvr52285qj3899ft380ao9l8is9louoen4ea6thgmq8hu38fmblo3tl6gjp0f6nrifplbh6d7770o96adr3d71i913aco/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("(Int -> B) <- B#0", ".unison/v1/type-index/#vjftvem4n0os6pnuko48ld67v7av3hq23r2gqvj7o536tfb1ctsci2fcgmmplj9b6slsege96onv4c2q8a0n8iadpe56mm4bc90muh8/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0"),
        ("Nat <- c", ".unison/v1/type-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo"),
        ("Boolean <- d", ".unison/v1/type-index/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("(A -> M) <- M#0", ".unison/v1/type-index/#735ugfihokh6o8ob9akhe1ei05ocsfncdrj76bdomeue5rb9td82q7m4a72e68bpgl3np562fehe9uio4vfcs07ib0mss1o5m08plk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0"),
        ("Nat <- p", ".unison/v1/type-index/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500"),
        -- note: typeForIndexing = Type.removeAllEffectVars typ
        ("(Nat -> Nat) <- q", ".unison/v1/type-index/#29pbek54phqkda8dp4erqn9u6etr8dm74h3sbg431kdvrt23l3c2a7eh01qpnc4kqq6i8fu1g0r5dsc08qqofnrlvfhpqs4cb6snls0/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Boolean <- r", ".unison/v1/type-index/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8"),
        ("Boolean <- r'", ".unison/v1/type-index/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
      ]

    typeMentionsIndex =
      [ ("(Nat -> B -> A) <- A#0", ".unison/v1/type-mentions-index/#6n4ih159cqcvr52285qj3899ft380ao9l8is9louoen4ea6thgmq8hu38fmblo3tl6gjp0f6nrifplbh6d7770o96adr3d71i913aco/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("(B -> A) <- A#0", ".unison/v1/type-mentions-index/#7u2a6hguqo74e3aq141fvopo9snclmfbg149k6e51j96hebi23q0tjq2dqjme76smull2r2lkap58ph0pcvpqn0dv1rk1ssfdt20cvo/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("Nat <- A#0", ".unison/v1/type-mentions-index/_builtin/Nat/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("B <- A#0", ".unison/v1/type-mentions-index/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("A <- A#0", ".unison/v1/type-mentions-index/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0"),
        ("(Int -> B) <- B#0", ".unison/v1/type-mentions-index/#vjftvem4n0os6pnuko48ld67v7av3hq23r2gqvj7o536tfb1ctsci2fcgmmplj9b6slsege96onv4c2q8a0n8iadpe56mm4bc90muh8/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0"),
        ("Int <- B#0", ".unison/v1/type-mentions-index/_builtin/Int/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0"),
        ("B <- B#0", ".unison/v1/type-mentions-index/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0"),
        ("Nat <- c", ".unison/v1/type-mentions-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo"),
        ("Boolean <- d", ".unison/v1/type-mentions-index/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do"),
        ("(A -> M) <- M#0", ".unison/v1/type-mentions-index/#735ugfihokh6o8ob9akhe1ei05ocsfncdrj76bdomeue5rb9td82q7m4a72e68bpgl3np562fehe9uio4vfcs07ib0mss1o5m08plk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0"),
        ("A <- M#0", ".unison/v1/type-mentions-index/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0"),
        ("M <- M#0", ".unison/v1/type-mentions-index/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0"),
        ("Nat <- p", ".unison/v1/type-mentions-index/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500"),
        ("(Nat -> Nat) <- q", ".unison/v1/type-mentions-index/#29pbek54phqkda8dp4erqn9u6etr8dm74h3sbg431kdvrt23l3c2a7eh01qpnc4kqq6i8fu1g0r5dsc08qqofnrlvfhpqs4cb6snls0/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Nat <- q", ".unison/v1/type-mentions-index/_builtin/Nat/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8"),
        ("Boolean <- r", ".unison/v1/type-mentions-index/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8"),
        ("Boolean <- r'", ".unison/v1/type-mentions-index/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
      ]

-- a helper to try turning these repo path names into test titles, by
-- limiting each path segment to 20 chars.  may produce duplicate names since
-- it ends up dropping reference cycles suffixes, constructor ids, etc.
makeTitle :: String -> String
makeTitle = intercalate "/" . map (take 20) . drop 2 . splitOn "/"
