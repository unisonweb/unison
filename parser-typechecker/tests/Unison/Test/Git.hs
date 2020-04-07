{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Unison.Test.Git where

import EasyTest
import Data.String.Here (iTrim)
import Unison.Prelude
import qualified Data.Text as Text
import qualified System.IO.Temp as Temp
import Shellmet ()
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory, removeDirectoryRecursive)

import Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Codebase.FileCodebase.Reserialize as Reserialize
import Unison.Codebase.FileCodebase.CopyFilterIndex as CopyFilterIndex
import Unison.Codebase.FileCodebase.CopyRegenerateIndex as CopyRegenerateIndex
import Unison.Codebase.FileCodebase.Common (SyncToDir, formatAnn)
import qualified Unison.Codebase.Serialization.V1 as V1
import Unison.Var (Var)
import Unison.Codebase (BuiltinAnnotation)

test :: Test ()
test = scope "git" . tests $ [testPush]

-- goal of this test is to make sure that push works correctly:
-- the destination should contain the right definitions from the namespace,
-- unnamed transitive dependencies (terms and types),
-- dependents, type, and type mentions indices.
testPush :: Test ()
testPush = scope "push" $ do

--  Temp.withSystemTempDirectory "git-push" $ \tmp -> do
    tmp <- io $ Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "git-push"

    let configFile = tmp </> ".unisonConfig"
    let codebaseDir = tmp </> "codebase"

    -- initialize a fresh codebase

    -- transcript runner wants a "current directory" for I guess writing scratch files?
    -- this should almost definitely not be this, if it were to be used.
    currentDir <- io getCurrentDirectory

    let err err = error $ "Parse error: \n" <> show err
    
    -- run the "setup transcript" to do the adds and updates, 
    -- everything short of pushing  
    c <- io $ FC.initCodebase codebaseDir
    flip (either err) (TR.parse "setupTranscript" setupTranscript) $ \stanzas -> do
      io . void $ TR.run currentDir configFile stanzas c
      
      -- now we'll try pushing three ways.
      for_ pushImplementations $ \(implName, impl) -> scope implName $ do
        -- initialize git repo
        let repoGit = tmp </> (implName ++ ".git")
        io $ "git" ["init", "--bare", Text.pack repoGit]

        -- push one way!
        flip (either err) (TR.parse "pushTranscript" (pushTranscript repoGit)) $ \stanzas -> do
          let experimentalCodebase = FC.codebase1' impl V1.formatSymbol formatAnn codebaseDir
          io . void $ TR.run currentDir configFile stanzas experimentalCodebase
          
          -- check out the resulting repo so we can inspect it 
          io $ "git" ["clone", Text.pack repoGit, Text.pack $ tmp </> implName ]
          
          -- inspect it
          for_ groups $ \(group, list) -> scope group $
            for_ list $ \(title, path) -> scope title $
              io (doesFileExist $ tmp </> implName </> path) >>= expect

    -- if we haven't crashed, clean up!
    io $ removeDirectoryRecursive tmp

  where
  setupTranscript = Text.pack $ [iTrim|
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
  pushTranscript repo = Text.pack $ [iTrim|
```ucm
.foo.inside> push ${repo}
```
|]

  pushImplementations :: (MonadIO m, Var v, BuiltinAnnotation a)
                      => [(String, SyncToDir m v a)]
  pushImplementations =
    [ ("Reserialize", Reserialize.syncToDirectory)
    , ("CopyFilterIndex", CopyFilterIndex.syncToDirectory)
    , ("CopyRegenerateIndex", CopyRegenerateIndex.syncToDirectory)
    ]

  groups =
    [ ("types", types)
    , ("terms", terms)
    , ("branches", branches)
    , ("patches", patches)
    , ("dependentsIndex", dependentsIndex)
    , ("typeIndex", typeIndex)
    , ("typeMentionsIndex", typeMentionsIndex) ]

  types =
    [ ("M", ".unison/v1/types/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg/compiled.ub")
    , ("A", ".unison/v1/types/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/compiled.ub")
    , ("B", ".unison/v1/types/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/compiled.ub")
    ]

  terms =
    [ ("p (type)",     ".unison/v1/terms/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/type.ub")
    , ("p (compiled)", ".unison/v1/terms/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/compiled.ub")
    , ("c (type)",     ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/type.ub")
    , ("c (compiled)", ".unison/v1/terms/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/compiled.ub")
    , ("d (type)",     ".unison/v1/terms/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/type.ub")
    , ("d (compiled)", ".unison/v1/terms/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/compiled.ub")
    , ("q (type)",     ".unison/v1/terms/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8/type.ub")
    , ("q (compiled)", ".unison/v1/terms/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8/compiled.ub")
    , ("r (type)",     ".unison/v1/terms/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8/type.ub")
    , ("r (compiled)", ".unison/v1/terms/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8/compiled.ub")
    , ("r' (type)",     ".unison/v1/terms/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo/type.ub")
    , ("r' (compiled)", ".unison/v1/terms/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo/compiled.ub")
    ]

  branches =
    [ ("_head",  ".unison/v1/paths/_head/pciob2qnondela4h4u1dtk9pvbc9up7qed0j311lkomordjah2lliddis7tdl76h5mdbs5ja10tm8kh2o3sni1bu2kdsqtm4fkv5288")
    , (".",      ".unison/v1/paths/pciob2qnondela4h4u1dtk9pvbc9up7qed0j311lkomordjah2lliddis7tdl76h5mdbs5ja10tm8kh2o3sni1bu2kdsqtm4fkv5288.ub")
    , (".'",     ".unison/v1/paths/0ufjqqmabderbejfhrled8i4lirgpqgimejbkdnk1m9t90ibj25oi7g1h2adougdqhv72sv939eq67ur77n3qciajh0reiuqs68th00.ub")
    , (".M",     ".unison/v1/paths/i2p08iv1l50fc934gh6kea181kvjnt3kdgiid5c4r5016kjuliesji43u4j4mjvsne3qvmq43puk9dkm61nuc542n7pchsvg6t0v55o.ub")
    , ("<empty>",".unison/v1/paths/7asfbtqmoj56pq7b053v2jc1spgb8g5j4cg1tj97ausi3scveqa50ktv4b2ofoclnkqmnl18vnt5d83jrh85qd43nnrsh6qetbksb70.ub")
    ]

  patches =
    [ ("patch", ".unison/v1/patches/96b419pm6l896ncmef9kqkpj29gq205amsl6prsl2num29thpn9fej8v8ndcmubadv5hehege4s43n3ljbifsnna92lpeuacq9fm3qo.up") ]

  dependentsIndex =
    [ ("Nat <- A",        ".unison/v1/dependents/_builtin/Nat/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8")
    , ("B <- A",          ".unison/v1/dependents/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8")
    , ("Int <- B",        ".unison/v1/dependents/_builtin/Int/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0")
    , ("Nat <- c",        ".unison/v1/dependents/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo")
    , ("Boolean <- d",    ".unison/v1/dependents/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("Nat <- d",        ".unison/v1/dependents/_builtin/Nat/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("Nat.+ <- d",      ".unison/v1/dependents/_builtin/Nat.+/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("Universal.< <- d",".unison/v1/dependents/_builtin/Universal.$less-than$/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("c <- d",          ".unison/v1/dependents/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("p <- d",          ".unison/v1/dependents/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("A <- M",          ".unison/v1/dependents/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg")
    , ("Nat <- p",        ".unison/v1/dependents/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500")
    , ("c <- p",          ".unison/v1/dependents/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500")
    , ("Nat <- q",        ".unison/v1/dependents/_builtin/Nat/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Nat.* <- q",      ".unison/v1/dependents/_builtin/Nat.$star$/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Nat.+ <- q",      ".unison/v1/dependents/_builtin/Nat.+/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("p <- q",          ".unison/v1/dependents/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Boolean <- r",    ".unison/v1/dependents/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8")
    , ("d <- r",          ".unison/v1/dependents/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8")
    , ("Boolean <- r'",    ".unison/v1/dependents/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
    ]

  typeIndex =
    [ ("(Nat -> B -> A) <- A#0",".unison/v1/type-index/#6n4ih159cqcvr52285qj3899ft380ao9l8is9louoen4ea6thgmq8hu38fmblo3tl6gjp0f6nrifplbh6d7770o96adr3d71i913aco/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("(Int -> B) <- B#0",     ".unison/v1/type-index/#vjftvem4n0os6pnuko48ld67v7av3hq23r2gqvj7o536tfb1ctsci2fcgmmplj9b6slsege96onv4c2q8a0n8iadpe56mm4bc90muh8/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0")
    , ("Nat <- c",              ".unison/v1/type-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo")
    , ("Boolean <- d",          ".unison/v1/type-index/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("(A -> M) <- M#0",       ".unison/v1/type-index/#735ugfihokh6o8ob9akhe1ei05ocsfncdrj76bdomeue5rb9td82q7m4a72e68bpgl3np562fehe9uio4vfcs07ib0mss1o5m08plk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0")
    , ("Nat <- p",              ".unison/v1/type-index/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500")
    -- note: typeForIndexing = Type.removeAllEffectVars typ
    , ("(Nat -> Nat) <- q",     ".unison/v1/type-index/#29pbek54phqkda8dp4erqn9u6etr8dm74h3sbg431kdvrt23l3c2a7eh01qpnc4kqq6i8fu1g0r5dsc08qqofnrlvfhpqs4cb6snls0/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Boolean <- r",          ".unison/v1/type-index/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8")
    , ("Boolean <- r'",          ".unison/v1/type-index/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
    ]

  typeMentionsIndex =
    [ ("(Nat -> B -> A) <- A#0",".unison/v1/type-mentions-index/#6n4ih159cqcvr52285qj3899ft380ao9l8is9louoen4ea6thgmq8hu38fmblo3tl6gjp0f6nrifplbh6d7770o96adr3d71i913aco/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("(B -> A) <- A#0",       ".unison/v1/type-mentions-index/#7u2a6hguqo74e3aq141fvopo9snclmfbg149k6e51j96hebi23q0tjq2dqjme76smull2r2lkap58ph0pcvpqn0dv1rk1ssfdt20cvo/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("Nat <- A#0",            ".unison/v1/type-mentions-index/_builtin/Nat/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("B <- A#0",              ".unison/v1/type-mentions-index/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("A <- A#0",              ".unison/v1/type-mentions-index/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8#d0")
    , ("(Int -> B) <- B#0",     ".unison/v1/type-mentions-index/#vjftvem4n0os6pnuko48ld67v7av3hq23r2gqvj7o536tfb1ctsci2fcgmmplj9b6slsege96onv4c2q8a0n8iadpe56mm4bc90muh8/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0")
    , ("Int <- B#0",            ".unison/v1/type-mentions-index/_builtin/Int/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0")
    , ("B <- B#0",              ".unison/v1/type-mentions-index/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0/#muulibntaqdk8hn0qjdnf9jn2qjgsh9bbtsrp626dianupo25llnecke6lhgv01vdenra45hor9u855kiiitu3ua60dg1bk4teb4ba0#d0")
    , ("Nat <- c",              ".unison/v1/type-mentions-index/_builtin/Nat/#msp7bv40rvjd2o8022ti44497ft2hohrg347pu0pfn75vt1s0qh2v8n9ttmmpv23s90fo2v2qpr8o5nl2jelt0cev6pi1sls79kgdoo")
    , ("Boolean <- d",          ".unison/v1/type-mentions-index/_builtin/Boolean/#6cdi7g1oi2lro3d6n9qg8v8fe3l2clc194qnb507oi72d5ap08gs0v9m80qbe0nc1keui9r03jnb48is0lttbsk336ehetlc2cs37do")
    , ("(A -> M) <- M#0",       ".unison/v1/type-mentions-index/#735ugfihokh6o8ob9akhe1ei05ocsfncdrj76bdomeue5rb9td82q7m4a72e68bpgl3np562fehe9uio4vfcs07ib0mss1o5m08plk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0")
    , ("A <- M#0",              ".unison/v1/type-mentions-index/#0n4pbd0q9uh78eurgn28gkqk44gdtgttv9uuvusvm1fg6dvapdn76ui86lsn761lop466vo8m80m4is9n5qukg80vr4k8fibpo58rk8/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0")
    , ("M <- M#0",              ".unison/v1/type-mentions-index/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg/#4idrjau9395kb8lsvielcjkli6dd7kkgalsfsgq4hq1k62n3vgpd2uejfuldmnutn1uch2292cj6ebr4ebvgqopucrp2j6pmv0s5uhg#d0")
    , ("Nat <- p",              ".unison/v1/type-mentions-index/_builtin/Nat/#fiupm7pl7o6ffitqatr174po7rdoh8ajqtcj7nirbeb9nqm4qd5qg9uvf1hic7lsm7b9qs38ka9lqv1iksmd6mothe816di0vcs0500")
    , ("(Nat -> Nat) <- q",     ".unison/v1/type-mentions-index/#29pbek54phqkda8dp4erqn9u6etr8dm74h3sbg431kdvrt23l3c2a7eh01qpnc4kqq6i8fu1g0r5dsc08qqofnrlvfhpqs4cb6snls0/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Nat <- q",              ".unison/v1/type-mentions-index/_builtin/Nat/#l5pndeifuhmue9a204v77h8kgff6lt8i5rnujkv3u74bjqukokol9vj45t291i7grneso95i3jctnr8a1nes523m1gb8jqir3o1k6h8")
    , ("Boolean <- r",          ".unison/v1/type-mentions-index/_builtin/Boolean/#im2kiu2hmnfdvv5fbfc5lhaakebbs69074hjrb3ptkjnrh6dpkcp1rnnq99mhson2gr6g8uduppvpelpq4jvq1rg5p3f9jpiplpk9u8")
    , ("Boolean <- r'",          ".unison/v1/type-mentions-index/_builtin/Boolean/#gi015he0n17ji9sl5hgh1q8tjas74341p48h719kkgajj75d6qapakq993gu2duvit32b7qhqac1odk6jhvad0ku8ajcj7sup6t6mbo")
    ]
