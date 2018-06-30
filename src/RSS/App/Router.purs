module RSS.App.Router where

import Prelude

import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Trans.Class (lower)
import Control.Monad.State.Class (modify, put)
import Control.Monad.Transition.Trans (TransitionT, hoistTransitionT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (launchAff_)
import RSS (Feed(..), Feeds, Item)
import RSS.App.ViewFeed as ViewFeed
import RSS.App.ViewItem as ViewItem
import React.DOM as D
import React.DOM.Props as P
import Type.Proxy (Proxy2)
import UI (liftUIT)
import UI.Combination (Combination)
import UI.Combination as Combination
import UI.React (ReactComponent, ReactUI)

type Routes =
  ( viewFeed :: Proxy2 ViewFeed.Space
  , viewItem :: Proxy2 ViewItem.Space
  )

data State
  = NoSelection
  | SelectedFeed (Feed String)
  | SelectedItem (Feed String) (Item String)

type Space = StoreT State (Combination Routes)
type Comm = (->) (Feeds String)

component :: ReactComponent Space Comm
component =
  StoreT
  $ flip Tuple NoSelection
  $ Combination.combine render
      { viewFeed:
          ViewFeed.component
          <#> Combination.liftUI (SProxy :: SProxy "viewFeed")
          <#> liftUIT
      , viewItem:
          ViewItem.component
          <#> Combination.liftUI (SProxy :: SProxy "viewItem")
          <#> liftUIT
      }
  where
    render :: _ -> State -> ReactUI Space Comm
    render { viewFeed, viewItem } state send feeds =
      D.div
        [ P.className "Content column flex" ]
        [ D.button
            [ P.className "icon"
            , P.onClick \_ -> launchAff_ $ send goUp
            ]
            [ D.text "←" ]
        , case state of
            NoSelection ->
              noSelection

            SelectedFeed feed@(Feed { items }) ->
              viewFeed send
                { items
                , onSelectItem: \item -> send do
                    put (SelectedItem feed item)
                    hoistTransitionT lower $
                      Combination.lift (SProxy :: SProxy "viewItem") $
                        put (Just item)
                }

            SelectedItem _ item ->
              viewItem send {}
        ]

    noSelection =
      D.div
        [ P.className "NoSelection column flex" ]
        [ D.text "No feed selected" ]

    goUp = void $ modify
      case _ of
        NoSelection -> NoSelection
        SelectedFeed _ -> NoSelection
        SelectedItem feed _ -> SelectedFeed feed

selectFeed :: forall m. Feed String -> TransitionT Space m Unit
selectFeed feed = do
  put (SelectedFeed feed)
  hoistTransitionT lower $
    Combination.lift (SProxy :: SProxy "viewFeed") $
      put 0
