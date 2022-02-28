{-# Language OverloadedStrings #-}

module Main where

import Data.String (fromString)
import Unison.Util.Pretty as PP
import Data.Text (Text)

main :: IO ()
main = do
  -- putStrLn . PP.toANSI 60 $ ex1
  -- print $ examples
  putStrLn . PP.toANSI 25 $ examples
  where
  -- ex1 = PP.linesSpaced [PP.red "hi", PP.blue "blue"]
  examples = PP.linesSpaced [
    PP.bold "Creating `Pretty`s",

    "Use `OverloadedStrings`, `lit`, and `text` to get values into `Pretty`",
    "Here's an overloaded string",
    PP.lit "Here's a call to `lit`", -- works for any `IsString`
    PP.text ("No need to Text.unpack, just `PP.text` directly" :: Text),

    PP.bold "Use the `Monoid` and/or `Semigroup` to combine strings",
    "Hello, " <> PP.red "world!",

    PP.yellow "`wrap` does automatic line wrapping",
    PP.wrap $ loremIpsum,
    PP.wrapString "Can also call `wrapString` directly if you have a String value.",

    PP.bold "Indentation: can indent by n spaces, or by another `Pretty`",
    PP.indentN 2 (PP.wrap loremIpsum),
    PP.indent (PP.red ">> ") (PP.wrap loremIpsum),

    PP.bold "Other handy functions",

    PP.bulleted [
      PP.sep ", " (replicate 10 "a"),
      PP.lines ["Alice", PP.hiBlue "Bob", "Carol"],
      PP.blue "foo bar baz"
    ],

    PP.indentN 4 $ PP.bulleted ["Alice", "Bob", "Carol"],
    PP.dashed ["Alice", PP.red "Bob", "Carol"],
    PP.column2 [
      (PP.bold "Name", PP.bold "Favorite color"),
      ("Alice"       , PP.red "Red"),
      ("Bob"         , PP.blue "Blue"),
      ("Carolina"    , PP.green "Green"),
      ("Dave"        , PP.black "Black")
    ],
    PP.numbered (fromString . show) [
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"],
    -- Feel free to start the numbering wherever you like
    PP.numbered (fromString . show . (10 +)) ["uno", "dos", "tres"],

    PP.bold "Grouping and breaking",
    PP.wrap "The orElse function chooses between two `Pretty`, preferring the first if it fits, and using the second otherwise.",

    PP.wrap "The `group` function introduces a level of breaking. The renderer will try to avoid breaking up a `group` unless it's needed. Groups are broken \"outside in\".",

    -- question - I think this group shouldn't be needed
    PP.group (PP.orElse "This fits." "So this won't be used."),
    "This is a very long string which won't fit."
      `PP.orElse` "This is a very...(truncated)"
    ]
  loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
  -- loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas sem nisi, venenatis viverra ex eu, tristique dapibus justo. Ut lobortis mattis rutrum. Vivamus mattis eros diam, a egestas mi venenatis vel. Nunc felis dui, consectetur ac volutpat vitae, molestie in augue. Cras nec aliquet ex. In et sem vel sapien auctor euismod. Pellentesque eu aliquam dolor. Cras porttitor mi velit, dapibus vulputate odio pharetra non. Etiam iaculis nulla eu nisl euismod ultricies."
