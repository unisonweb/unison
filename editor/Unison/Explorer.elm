module Unison.Explorer where

import Elmz.Layout (Layout,Region)
import Elmz.Layout as Layout
import Elmz.Maybe
import Elmz.Movement as Movement
import Elmz.Signal as Signals
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Input.Field as Field
import Keyboard
import List
import List ((::))
import Maybe
import Mouse
import Signal
import String
import Unison.Styles as Styles
import Window

{-|

While CLOSED, on click/enter, if scope is currently defined, enter state OPEN
  * generates a popup immediately below the currently selected scope
  * popup has an input box and a list of search results
While OPEN
  * if on tablet/mobile and valid completions is small, avoid showing input box
    unless user clicks
  * arrow keys do not manipulate scope, they navigate around the explorer
    * similar behavior to scope manipulation, mouse movement cancels
  * clicks outside explorer popup cancel the edit
  * clicks inside input box handled by input box
  * hovers inside explorer pop up previews
  * click/enter of search result exits to CLOSED with a pending edit
  * display goal type
  * display current type
-}

todo : a
todo = todo

type alias S v =
  { isKeyboardOpen : Bool
  , prompt : String
  , goal : Element
  , current : Element
  , input : Field.Content
  , searchbox : Signal.Channel Field.Content
  , focus : Region
  , width : Int
  , completions : List (Element,v)
  , invalidCompletions : List Element }

listSelection : Signal (Int,Int) -> Signal Movement.D1 -> Signal (List v) -> Signal (Layout (Maybe Int)) -> Signal (Maybe Int)
listSelection mouse upDown values l =
  let reset = mouse
      base vprev v l (x,y) = case Layout.atPoint l { x = x, y = y } of
        h :: _ -> (vprev, v, l, h)
        _ -> (vprev, v, l, Just 0)
      indexOk ctx i = Layout.exists ((==) i) ctx
      modify f (vprevs, vs, ctx, i) =
        let i' = case i of
              Nothing -> Nothing
              Just i ->
                if vs /= vprevs
                then index i vprevs `Maybe.andThen` \v -> indexOf ((==) v) vs
                else Just i
            m i = case i of
              Nothing -> Nothing
              Just i -> if indexOk ctx (Just (f i)) then Just (f i) else Just i
        in (vprevs, vs, ctx, m i')
      changes = Signal.map2 (/=) (Signals.delay [] values) values |> Signals.ups
      upDown' = Signal.map2 (\b d -> if b then d else Movement.D1 Movement.Zero) changes upDown
      mover = { increment = modify (\i -> i + 1), decrement = modify (\i -> i - 1) }
  in Movement.moveD1 mover reset (Signal.map4 base (Signals.delay [] values) values l mouse) upDown'
     |> Signal.map (Maybe.map (\(_,_,_,i) -> i))
     |> Signals.flattenMaybe

highlightSelection : Signal (Layout (Maybe a)) -> Signal (Maybe a) -> Signal Element
highlightSelection l i =
  let layer l i = case i of
    Nothing -> E.empty
    Just i -> case Layout.region (\_ _ -> True) identity l (Just i) of
      (_, region) :: _ -> Styles.selection l region
      _ -> E.empty
  in Signal.map2 layer l i

autocomplete : S v -> Layout (Maybe Int)
autocomplete s =
  let ok = not (List.isEmpty s.completions)
      statusColor = Styles.statusColor ok
      fld = Field.field (Styles.autocomplete ok)
                        (Signal.send s.searchbox)
                        s.prompt
                        s.input
      insertion = Styles.carotUp 7 statusColor
      status = Layout.above Nothing (Layout.embed Nothing s.goal)
                                    (Layout.embed Nothing s.current)
      renderCompletion i (e,v) = Layout.embed (Just i) e
      invalids = List.map (Layout.embed Nothing) s.invalidCompletions
      fldLayout = Layout.embed Nothing fld
      [top,bot] = Layout.column
        [ fldLayout
        , Styles.verticalCells Nothing E.empty (status :: List.indexedMap renderCompletion s.completions
          `List.append` invalids) ]
      box = Layout.above Nothing
        (Layout.embed Nothing (E.beside (E.spacer 14 1) insertion))
        (Layout.above Nothing top bot)
      boxTopLeft = { x = s.focus.topLeft.x, y = s.focus.topLeft.y + s.focus.height }
      h = boxTopLeft.y + Layout.heightOf box
  in Layout.container Nothing s.width h boxTopLeft box

explorer : Signal (Int,Int) -> Signal Movement.D1 -> Signal (Maybe (S v)) -> Signal (Maybe (Element, Maybe v))
explorer mouse upDown s =
  let base : Signal (Layout (Maybe Int))
      base = Signals.fromMaybe (Signal.constant (Layout.empty (Just 0)))
                               (Signals.justs (Signal.map (Maybe.map autocomplete) s))
      values =
        let f s = case s of
          Nothing -> []
          Just s -> List.map snd s.completions
        in Signal.map f s
      selectedIndex : Signal (Maybe Int)
      selectedIndex = listSelection mouse upDown values base

      selectedValue =
        let f s i = Elmz.Maybe.map2 (\s i -> Maybe.map snd <| index i s.completions) s i
                 |> Elmz.Maybe.join
        in Signal.map2 f s selectedIndex

      highlight : Signal Element
      highlight = highlightSelection base selectedIndex

      selection' =
        let f ex s v hl = Maybe.map (\_ -> (E.layers [Layout.element ex, hl], v)) s
        in Signal.map4 f base s selectedValue highlight
  in selection'

index : Int -> List a -> Maybe a
index i l = case List.drop i l of
  h :: _ -> Just h
  _ -> Nothing

indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf f l = List.indexedMap (\i a -> (i, f a)) l
           |> List.filterMap (\(i,b) -> if b then Just i else Nothing)
           |> index 0

searchbox : (List v -> String -> List v) -> Signal (List v) -> Signal String -> Signal (List v)
searchbox match vs s = Signal.map2 match vs s

main =
  let names = ["Alice", "Allison", "Bob", "Burt", "Carol", "Chris", "Dave", "Donna", "Eve", "Frank"]
      search = Signal.channel Field.noContent
      searchStrings = Signal.map .string (Signal.subscribe search)
      values = searchbox (\vs s -> List.filter (String.startsWith s) vs) (Signal.constant names) searchStrings
      s vs c w =
        Just
          { isKeyboardOpen = True
          , prompt = "Enter a name"
          , goal = Styles.codeText "Goal: a valid name"
          , current = Styles.codeText "status"
          , input = c
          , searchbox = search
          , focus = { topLeft = { x = 50, y = 50 }, width = 50, height = 50 }
          , width = w
          , completions = List.map (\s -> (Styles.codeText s, s)) vs
          , invalidCompletions = [] }
      is = Signal.map3 s values (Signal.subscribe search) Window.width
      ex = explorer Mouse.position Movement.upDown is
      scene e = case e of
        Nothing -> Styles.codeText "empty"
        Just (e, v) -> E.flow E.down
          [ e
          , E.spacer 20 10
          , "current selection: " ++ toString v |> Styles.codeText ]
   in Signal.map scene ex
