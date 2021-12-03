{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Settings where

debugNoteLoc,debugNoteSummary,debugRevealForalls :: Bool
debugNoteLoc = False
debugNoteSummary = False
debugRevealForalls = False

renderTermMaxLength :: Int
renderTermMaxLength = 20

demoHideVarNumber :: Bool
demoHideVarNumber = False

removePureEffects :: Bool
removePureEffects = True

cleanupTypes :: Bool
cleanupTypes = True
