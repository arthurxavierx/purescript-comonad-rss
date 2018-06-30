module RSS.App.Pages where

import Prelude

import Control.Comonad ((=>>))
import Control.Comonad.Store (Store, peeks, store)
import Control.Monad.State.Class (modify)
import Data.Array (length, (!!), (:), (..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, launchAff_)
import React.DOM as D
import React.DOM.Props as P
import UI.React (ReactComponent, ReactUI)

type Space = Store Int
type Comm item = (->)
  { items :: Array item
  , onSelectItem :: item -> Aff Unit
  }

component
  :: forall item
   . (item -> ReactUI Space (Comm item))
  -> ReactUI Space (Comm item)
  -> Int
  -> ReactComponent Space (Comm item)
component renderItem renderEmpty pageSize =
  store render 0
  =>>
    \component' send props@{ items } ->
      peeks (clampPages pageSize items) component' send props
  where
    render :: Int -> ReactUI Space (Comm item)
    render pos send props@{ items } =
      let
        pages = chunks pageSize items
        currentItems = fromMaybe [] (pages!!pos)
        header =
          D.div [ P.className "PagesHeader row" ] $
            if Array.length items < pageSize
            then []
            else [ previousButton ] <> pageButtons <> [ nextButton ]

        previousButton =
          D.button
            [ P.className "icon"
            , P.onClick \_ -> launchAff_ $ send $ void $ modify (_ - 1)
            , P.style ({ visibility: if pos <= 0 then "hidden" else "visible" })
            ]
            [ D.text "«" ]

        nextButton =
          D.button
            [ P.className "icon"
            , P.onClick \_ -> launchAff_ $ send $ void $ modify (_ + 1)
            , P.style ({ visibility: if pos >= length pages - 1 then "hidden" else "visible" })
            ]
            [ D.text "»" ]

        pageButtons =
          (0..(length pages - 1)) # foldMap \i ->
            [ D.button
                [ P.className (if pos == i then "icon PageButton selected" else "icon PageButton")
                , P.onClick \_ -> launchAff_ $ send $ void $ modify (const i)
                ]
                [ D.text $ show (i + 1) ]
            ]
      in
        if length items <= 0
          then renderEmpty send props
          else
            D.div
              [ P.className "Pages column flex" ]
              [ header
              , D.div
                  [ P.className "PagesItems column flex" ]
                  (foldMap (\item -> [renderItem item send props]) currentItems)
              ]

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks n as
  | Array.length as <= 0 = []
  | otherwise = (Array.take n as):(chunks n (Array.drop n as))

clampPages :: forall item. Int -> Array item -> Int -> Int
clampPages pageSize items = clamp 0 ((Array.length items - 1) `div` pageSize)
