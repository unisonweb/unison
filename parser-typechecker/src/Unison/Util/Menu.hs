{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Unison.Util.Menu (menu1, menuN) where

import           Data.List             (find, isPrefixOf)
import           Data.String           (IsString, fromString)
import           Data.Strings          (strPadLeft)
import           Safe                  (atMay)
import qualified Text.Read             as Read
import           Unison.Util.ColorText (StyledText, renderText)
import           Unison.Util.Monoid    (intercalateMap)
-- utility - command line menus

type Caption = StyledText
type Stylized = StyledText
type Keyword = String
type Console = IO String

renderChoices :: forall a mc
              .  (a -> Stylized)
              -> (mc -> Stylized)
              -> [([Keyword], [a])]
              -> [([Keyword], mc)]
              -> (Keyword -> Bool)
              -> Stylized
renderChoices render renderMeta groups metas isSelected =
  intercalateMap "\n" format numberedGroups <> "\n\n" <>
  intercalateMap " " (("["<>) . (<>"]") . renderMeta . snd) metas
  where
    numberedGroups :: [(([Keyword], [a]), Int)]
    numberedGroups = zip groups [1..]
    numberWidth = ceiling @Double . logBase 10 . fromIntegral $ length groups
    format :: (([Keyword], [a]), Int) -> Stylized
    format ((keywords, as), number) =
      intercalateMap
        "\n"
        (format1 number (length as) (any isSelected keywords))
        (zip as [1..])
    format1 :: Int -> Int -> Bool -> (a, Int) -> Stylized
    format1 groupNumber groupSize isSelected (a, index) =
      header <> bracket <> render a
      where
        header :: (Semigroup s, IsString s) => s
        header =
          (if representativeRow
            then (if isSelected then "*" else " ")
                  <> fromString (strPadLeft ' ' numberWidth (show groupNumber))
            else fromString $ replicate (numberWidth + 1) ' ') <> ". "
        representativeRow :: Bool
        representativeRow = index == 0 -- alternatively: index == groupSize - 1 `div` 2
        bracket :: IsString s => s
        bracket =
          if groupSize == 1         then "·"
          else if index == 1        then "┌"
          else if index < groupSize then "│"
          else                           "└"

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

menu1 :: forall a mc
      . Console
      -> Caption
      -> (a -> Stylized)
      -> (mc -> Stylized)
      -> [(Keyword, a)]
      -> [(Keyword, mc)]
      -> Maybe Keyword
      -> IO (Maybe (Either mc a))
menu1 console caption render renderMeta groups metas initial = do
  let groups' = [ ([k], [a]) | (k, a) <- groups ]
      metas' = [ ([k], mc) | (k, mc) <- metas ]
  groupMenu1 console caption render renderMeta groups' metas' initial >>= \case
    Just (Right [a]) -> pure (Just (Right a))
    Just (Left mc) -> pure (Just (Left mc))
    Nothing -> pure Nothing
    _ -> error "unpossible; by construction we should only get singleton lists back"

_repeatMenu1 :: forall a mc
      . Console
     -> Caption
     -> (a -> Stylized)
     -> (mc -> Stylized)
     -> [([Keyword], [a])]
     -> [([Keyword], mc)]
     -> Maybe Keyword
     -> IO (Either mc [a])
_repeatMenu1 console caption render renderMeta groups metas initial =
  groupMenu1 console caption render renderMeta groups metas initial >>= \case
    Just x -> pure x
    Nothing -> _repeatMenu1 console caption render renderMeta groups metas initial

groupMenu1 :: forall a mc
       . Console
      -> Caption
      -> (a -> Stylized)
      -> (mc -> Stylized)
      -> [([Keyword], [a])]
      -> [([Keyword], mc)]
      -> Maybe Keyword
      -> IO (Maybe (Either mc [a]))
groupMenu1 console caption render renderMeta groups metas initial = do
  let restart = groupMenu1 console caption render renderMeta groups metas initial
      -- restart with an updated caption
      restart' caption groups metas initial =
                  groupMenu1 console caption render renderMeta groups metas initial
      resume = do
        putStr $ if null initial then "Choose by number or prefix: "
          else "Choose by number or prefix, or press Enter to use the current selection (*): "
        input <- console
        case words input of
          [] -> useExistingSelections groups initial
          input : _ -> case Read.readMaybe input of
            Just i  -> pickGroupByNumber i
            Nothing -> pickGroupByPrefix input
        where
          pickGroupByNumber :: Int -> IO (Maybe (Either mc [a]))
          pickGroupByNumber i = case atMay groups (i-1) of
            Nothing -> do
              putStrLn $ "Please pick a number from 1 to " ++
                          show (length groups) ++ "."
              restart
            Just (_keywords, as) -> pure (Just (Right as))
          pickGroupByPrefix :: String -> IO (Maybe (Either mc [a]))
          pickGroupByPrefix s = case matchingItems groups metas s of
            ([],[]) -> do
              putStrLn $ "Sorry, '" ++ s ++ "' didn't match anything."
              resume
            ([(_, as)],[]) -> pure (Just (Right as))
            ([], [(_, mc)]) -> pure (Just (Left mc))
            (groups, metas) ->
              restart' "Please clarify your selection:" groups metas Nothing >>= \case
                Nothing -> resume
                x -> pure x
          matchingItems ::
            forall a mc. [([Keyword], [a])] -> [([Keyword], mc)] -> String
                   -> ([([Keyword], [a])], [([Keyword], mc)])
          matchingItems groups metas s =
            (filter (any (s `isPrefixOf`) . fst) groups
            ,filter (any (s `isPrefixOf`) . fst) metas)
          useExistingSelections ::
            [([Keyword], [a])] -> Maybe Keyword -> IO (Maybe (Either mc [a]))
          useExistingSelections groups initial = case initial of
            Nothing -> pure Nothing
            Just initial ->
              case findMatchingGroup [initial] groups of
                Just group -> pure (Just (Right group))
                Nothing -> error $
                  "Default selection \"" ++ show initial ++ "\"" ++
                  " not found in choice groups:\n" ++ show (fst <$> groups)
          findMatchingGroup :: forall a. [Keyword] -> [([Keyword], [a])] -> Maybe [a]
          findMatchingGroup initials groups =
            snd <$> find (\(keywords, _as) -> any (`elem` keywords) initials) groups
  print . renderText $ caption
  putStrLn ""
  print . renderText $ renderChoices render renderMeta groups metas (`elem` initial)
  resume

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
      -> (mc -> Stylized)
      -> [([Keyword], [a])]
      -> [([Keyword], mc)]
      -> [Keyword]
      -> IO (Either mc [[a]])
menuN _console _caption _render _renderMeta _groups _metas _initials = pure (Right [])
