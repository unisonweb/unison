{-# LANGUAGE TemplateHaskell #-}

module Unison.MCP.StaticResources
  ( staticResources,
    unisonGuideText,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Network.MCP.Types
import Unison.Util.FileEmbed (embedProjectStringFile)

staticResources :: Map Text (Resource, ResourceContent)
staticResources =
  Map.fromList
    [ (resourceUri . fst $ unisonGuideResource, unisonGuideResource)
    ]

unisonGuideText :: Text
unisonGuideText = $(embedProjectStringFile "src/Unison/MCP/StaticResources/unison-guide.md")

unisonGuideResource :: (Resource, ResourceContent)
unisonGuideResource =
  ( Resource
      { resourceUri = "file://unison-guide",
        resourceName = "Unison Programming Guide",
        resourceDescription = Just "A complete guide on how to program in Unison in Markdown format",
        resourceMimeType = Just "text/markdown",
        resourceTemplate = Nothing
      },
    ResourceContent
      { resourceContentUri = "file://unison-guide",
        resourceContentMimeType = Just "text/markdown",
        resourceContentText = Just unisonGuideText,
        resourceContentBlob = Nothing
      }
  )
