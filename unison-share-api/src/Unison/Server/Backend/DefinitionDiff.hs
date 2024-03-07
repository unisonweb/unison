module Unison.Server.Backend.DefinitionDiff (diffDisplayObjects) where

import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types (DiffedSyntaxText (DiffedSyntaxText), DisplayObjectDiff (..))
import Unison.Util.AnnotatedText (AnnotatedText (..))

diffDisplayObjects :: DisplayObject SyntaxText SyntaxText -> DisplayObject SyntaxText SyntaxText -> DisplayObjectDiff
diffDisplayObjects from to = case (from, to) of
  (BuiltinObject fromST, BuiltinObject toST) -> DisplayObjectDiff (BuiltinObject (diffSyntaxText fromST toST))
  (MissingObject fromSH, MissingObject toSH)
    | fromSH == toSH -> DisplayObjectDiff (MissingObject fromSH)
    | otherwise -> MismatchedDisplayObjects (MissingObject fromSH) (MissingObject toSH)
  (UserObject fromST, UserObject toST) -> DisplayObjectDiff (UserObject (diffSyntaxText fromST toST))
  (l, r) -> MismatchedDisplayObjects l r

diffSyntaxText :: SyntaxText -> SyntaxText -> DiffedSyntaxText
diffSyntaxText (AnnotatedText fromST) (AnnotatedText toST) = DiffedSyntaxText $ Seq.fromList $ Diff.getDiff (Foldable.toList @Seq fromST) (Foldable.toList @Seq toST)
