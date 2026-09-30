module Unison.Test.AnnotationSites (test) where

import EasyTest
import Unison.Lexer.Pos (Pos (..))
import Unison.Parser.Ann (Ann (..), SiteId (..), atSite, contains, encompasses, isFileAnn, siteId, startingLine)

test :: Test ()
test =
  let a = Ann (Pos 1 2) (Pos 3 4)
      b = Ann (Pos 2 2) (Pos 2 4)
      tagged = atSite (SiteId 7) a
      locations = [Intrinsic, External, GeneratedFrom a, a, b]
   in scope "annotation-sites" $
        tests
          [ scope "distinct-identities-same-location" $ do
              expectEqual a tagged
              expectEqual (siteId tagged) (Just (SiteId 7))
              expectEqual (siteId (atSite (SiteId 8) tagged)) (Just (SiteId 8))
              expectEqual (compare tagged (atSite (SiteId 8) a)) EQ,
            scope "source-operations-preserved" $ do
              expect (isFileAnn tagged)
              expectEqual (startingLine tagged) (startingLine a)
              expectEqual (contains tagged (Pos 2 3)) (contains a (Pos 2 3))
              expectEqual (encompasses tagged b) (encompasses a b)
              expectEqual (start tagged, end tagged) (start a, end a),
            scope "constructors-and-diagnostics" $
              mapM_
                ( \x -> do
                    expectEqual (show x) (show (atSite (SiteId 7) x))
                    expectEqual (atSite (SiteId 7) x) x
                    expectEqual (compare x a) (compare (atSite (SiteId 7) x) tagged)
                )
                locations,
            scope "combining-ranges-does-not-copy-identity" $ do
              expectEqual (siteId (tagged <> b)) Nothing
              expectEqual (siteId (b <> tagged)) Nothing
              expectEqual (siteId (mempty <> tagged)) (Just (SiteId 7))
              expectEqual (siteId (tagged <> mempty)) (Just (SiteId 7)),
            scope "combination-and-association-preserve-locations" $
              mapM_
                ( \(x, y, z) -> do
                    let x' = atSite (SiteId 1) x; y' = atSite (SiteId 2) y; z' = atSite (SiteId 3) z
                    expectEqual (x' <> y') (x <> y)
                    expectEqual ((x' <> y') <> z') (x' <> (y' <> z'))
                )
                [(x, y, z) | x <- locations, y <- locations, z <- locations],
            scope "generated-source-is-still-visible" $ do
              let g = atSite (SiteId 9) (GeneratedFrom tagged)
              expectEqual (startingLine g) (startingLine a)
              expectEqual (encompasses g b) (encompasses a b)
              expectEqual (siteId g) (Just (SiteId 9))
              case g of
                GeneratedFrom source -> expectEqual (siteId source) (Just (SiteId 7))
                _ -> crash "lost generated annotation"
          ]
