module Unison.Util.Menu where

import Unison.Util.ColorText (StyledText)
-- utility - command line menus

type Caption = String
type Stylized = StyledText -- todo upgrade this to a doc
type Keyword = String
type MetaChoice = String
type Console = IO String

{-
   <caption>

   1 ping
     pong
   2 foo
   3 bar

   [cancel]
   [help]

   >> ping

 -}
menu1 :: Console
      -> Caption
      -> (a -> Stylized)
      -> (MetaChoice -> Stylized)
      -> [[(Keyword, a)]]
      -> [([Keyword], MetaChoice)]
      -> Maybe Keyword
      -> IO (Either MetaChoice [a])
menu1 _console _caption _render _renderMeta _groups _metas _initial = pure $ Right []

{-
   <caption>

   1 ping
     pong
   2 foo
   3 bar

   [all]
   [cancel]
   [help]

   >> 1 3
   >> *

 -}
menuN :: Console
      -> Caption
      -> (a -> Stylized)
      -> (MetaChoice -> Stylized)
      -> [([Keyword], [a])]
      -> [([Keyword], MetaChoice)]
      -> Maybe [Keyword]
      -> IO (Either MetaChoice [[a]])
menuN _console _caption _render _renderMeta _groups _metas _initials = pure (Right [])
