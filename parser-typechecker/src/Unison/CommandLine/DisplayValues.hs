{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}

module Unison.CommandLine.DisplayValues where

import qualified Unison.Term as Term
import Unison.Term (AnnotatedTerm)
import qualified Unison.Util.Pretty as P
import qualified Unison.Runtime.IOSource as B
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.NamePrinter as NP
import Unison.Util.SyntaxText (SyntaxText)

type Pretty = P.Pretty P.ColorText

{-
displayDoc :: PPE.PrettyPrintEnv -> AnnotatedTerm v a -> Pretty  
displayDoc ppe t = case t of
  Term.App' (Term.Constructor' r cid) segs | r == B.docReference -> case segs of
    Term.Apps' (Term.Constructor' r cid) args | r == B.docSegmentReference -> case (cid, args) of
      (blob, [Term.Text' txt]) | blob == B.segmentBlobId 
        -> P.text txt
      (link, [Term.App' (Term.Constructor' r lid) a]) | link == B.segmentLinkId
                                                      | r == B.linkReference 
        -> case lid of
          _ | lid == B.linkTermId -> P.text txt
    _ -> error "todo"
  _ -> error "todo"
-}

{-
segmentBlobId = constructorNamed docSegmentReference "Doc.Segment.Blob"
segmentLinkId = constructorNamed docSegmentReference "Doc.Segment.Link"
segmentSignatureId = constructorNamed docSegmentReference "Doc.Segment.Signature"
segmentTranscludeId = constructorNamed docSegmentReference "Doc.Segment.Transclude"
segmentSourceId = constructorNamed docSegmentReference "Doc.Segment.Source"
segmentEvaluateId = constructorNamed docSegmentReference "Doc.Segment.Evaluate"
linkTermId = constructorNamed linkReference "Link.Term"
linkTypeId = constructorNamed linkReference "Link.Type"
-}

