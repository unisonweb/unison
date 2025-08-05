module Unison.MCP.Prompts (prompts) where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.MCP.Types qualified as MCP
import Unison.MCP.StaticResources (unisonGuideText)
import Unison.MCP.Types
import Unison.MCP.Wrapper
import Unison.MCP.Wrapper qualified as MCPWrapper

prompts :: [MCPWrapper.Prompt MCP]
prompts =
  [ writeUnisonCodePrompt
  ]

writeUnisonCodePrompt :: Prompt MCP
writeUnisonCodePrompt =
  Prompt
    { promptName = "unison-programming-assistant",
      promptDescription = "Unison Programming Assistant",
      promptArgs =
        Map.fromList
          [ ( "project-and-branch",
              PromptArgument
                { promptArgumentDescription = "[Optional] The Unison project and branch this code should be implemented in. E.g. scratch/main",
                  promptArgumentRequired = False
                }
            ),
            ( "preferred-libraries",
              PromptArgument
                { promptArgumentDescription = "[Optional] Specific libraries you'd like to be used. E.g. `@ceedubs/json and @unison/base`",
                  promptArgumentRequired = False
                }
            )
          ],
      promptHandler = \args -> do
        pure $
          MCP.GetPromptResult
            { getPromptDescription = Just "Ask the agent to write some Unison code for you.",
              getPromptMessages =
                [ MCP.PromptMessage
                    { promptMessageRole = "assistant",
                      promptMessageContent =
                        MCP.PromptContent
                          { promptContentType = TextPromptContent,
                            promptContentText =
                              Text.unlines
                                [ "Your role is to be a helpful Unison programming assistant. You will be given a description of a program to write in Unison, and you should write the code to implement it.",
                                  case Map.lookup "preferred-libraries" args of
                                    Just preferredLibs -> "You should use the following unison libraries to accomplish the task if they are applicable: " <> preferredLibs <> " if they are not already installed, you may search share for the libraries and then install them."
                                    Nothing -> "",
                                  "After implementing the code, ensure you typecheck it, and add watch expressions to test any pure functions.",
                                  "You can use the tools available to search Unison Share and the local project and its dependencies for definitions and documentation to help you accomplish your task.",
                                  "",
                                  "You should use the following guidelines when writing Unison code:",
                                  "",
                                  unisonGuideText
                                ]
                          }
                    }
                ]
            }
    }
