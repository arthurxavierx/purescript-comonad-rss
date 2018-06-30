module RSS.App.Sidenav where

import Prelude

import Data.Array (deleteAt, zip, length, (..))
import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import RSS (Feed(..), Feeds)
import React.DOM as D
import React.DOM.Props as P
import UI.React (ReactUI)

type Props =
  { feeds :: Feeds String
  , onNewFeed :: Aff Unit
  , onRemoveFeed :: Feeds String -> Aff Unit
  , onSelectFeed :: Feed String -> Aff Unit
  }

sidenav :: forall space. ReactUI space ((->) Props)
sidenav _ { feeds, onNewFeed, onRemoveFeed, onSelectFeed } =
  let
    enumeratedFeeds = zip (0..(length feeds - 1)) feeds
  in
    D.aside
      [ P.className "column" ]
      [ D.nav
          [ P.className "column flex" ]
          $ enumeratedFeeds # foldMap \(Tuple i feed@(Feed { title })) ->
            [ D.div
                [ P.className "Feed row" ]
                [ D.span
                    [ P.role "link"
                    , P.onClick \_ -> launchAff_ $ onSelectFeed feed
                    ]
                    [ D.text title ]
                , D.button
                    [ P.className "icon"
                    , P.onClick \_ -> launchAff_ $
                        onRemoveFeed (fromMaybe feeds (deleteAt i feeds))
                    ]
                    [ D.text "×" ]
                ]
            ]
      , D.button
          [ P.className "primary"
          , P.onClick \_ -> launchAff_ onNewFeed
          ]
          [ D.text "New feed" ]
      ]
