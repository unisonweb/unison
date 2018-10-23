module Unison.Util.Menu where

-- utility - command line menus

type Caption = String
type Stylized = String -- todo upgrade this to a doc
type Keyword = String
type MetaChoice = String
type Console = IO Char

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
      -> IO (Either MetaChoice [a])
menu1 _console _caption _render _renderMeta _groups _metas = pure (Right [])

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
      -> [[(Keyword, a)]]
      -> [([Keyword], MetaChoice)]
      -> IO (Either MetaChoice [[a]])
menuN _console _caption _render _renderMeta _groups _metas = pure (Right [])
