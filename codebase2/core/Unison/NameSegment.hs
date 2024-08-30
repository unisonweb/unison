module Unison.NameSegment
  ( NameSegment,
    toUnescapedText,

    -- * Sentinel name segments
    defaultPatchSegment,
    docSegment,
    libSegment,
    pattern LibSegment,
    publicLooseCodeSegment,
    baseSegment,
    snocSegment,
    consSegment,
    concatSegment,
    watchSegment,
    setSegment,
    modifySegment,
    licenseSegment,
    metadataSegment,
    authorsSegment,
    copyrightHoldersSegment,
    guidSegment,
    builtinSegment,
  )
where

import Unison.NameSegment.Internal (NameSegment (NameSegment, toUnescapedText))

------------------------------------------------------------------------------------------------------------------------
-- special segment names

defaultPatchSegment :: NameSegment
defaultPatchSegment = NameSegment "patch"

docSegment :: NameSegment
docSegment = NameSegment "doc"

libSegment :: NameSegment
libSegment = NameSegment "lib"

pattern LibSegment :: NameSegment
pattern LibSegment = NameSegment "lib"

publicLooseCodeSegment :: NameSegment
publicLooseCodeSegment = NameSegment "public"

baseSegment :: NameSegment
baseSegment = NameSegment "base"

snocSegment :: NameSegment
snocSegment = NameSegment ":+"

consSegment :: NameSegment
consSegment = NameSegment "+:"

concatSegment :: NameSegment
concatSegment = NameSegment "++"

watchSegment :: NameSegment
watchSegment = NameSegment ">"

setSegment :: NameSegment
setSegment = NameSegment "set"

modifySegment :: NameSegment
modifySegment = NameSegment "modify"

licenseSegment :: NameSegment
licenseSegment = NameSegment "License"

metadataSegment :: NameSegment
metadataSegment = NameSegment "metadata"

authorsSegment :: NameSegment
authorsSegment = NameSegment "authors"

copyrightHoldersSegment :: NameSegment
copyrightHoldersSegment = NameSegment "copyrightHolders"

guidSegment :: NameSegment
guidSegment = NameSegment "guid"

builtinSegment :: NameSegment
builtinSegment = NameSegment "builtin"
