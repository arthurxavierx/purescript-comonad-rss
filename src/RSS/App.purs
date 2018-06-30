module RSS.App where

import Prelude

import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Trans.Class (lower)
import Control.Monad.State.Class (modify, put)
import Control.Monad.Transition.Trans (TransitionT, hoistTransitionT)
import Data.Array (cons)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import RSS (Feed, Feeds)
import RSS.App.NewFeedModal as NewFeedModal
import RSS.App.Router as Router
import RSS.App.Sidenav (sidenav)
import React.DOM as D
import React.DOM.Props as P
import Type.Proxy (Proxy2)
import UI (liftUIT)
import UI.Combination (Combination)
import UI.Combination as Combination
import UI.React (ReactComponent, ReactUI)

type ChildrenProxies =
  ( content :: Proxy2 Router.Space
  , modal :: Proxy2 NewFeedModal.Space
  )

type Space = StoreT (Feeds String) (Combination ChildrenProxies)
type Comm = (->) (Feeds String)

component :: forall comm. Applicative comm => Feeds String -> ReactComponent Space comm
component init = StoreT $ Tuple (stateToProps <$> combination) init
  where
    stateToProps :: ReactUI Space Comm -> (Feeds String -> ReactUI Space comm)
    stateToProps ui state send = pure $ ui send state

    combination :: Combination ChildrenProxies (ReactUI Space Comm)
    combination =
      Combination.combine render
        { content:
            Router.component
            <#> Combination.liftUI (SProxy :: SProxy "content")
            <#> liftUIT
        , modal:
            NewFeedModal.component
            <#> Combination.liftUI (SProxy :: SProxy "modal")
            <#> liftUIT
        }

    render :: _ -> ReactUI Space Comm
    render { content, modal } send feeds =
      D.section
        [ P.className "AppContainer"
        ]
        [ sidenav send
            { feeds
            , onNewFeed: send openModal
            , onRemoveFeed: send <<< put
            , onSelectFeed: send <<< selectFeed
            }
        , content send feeds
        , modal send { onSubmit: \feed -> send (addFeed feed *> closeModal) }
        ]

addFeed :: forall m. Feed String -> TransitionT Space m Unit
addFeed feed = do
  _ <- modify (cons feed)
  selectFeed feed

selectFeed :: forall m. Feed String -> TransitionT Space m Unit
selectFeed =
  hoistTransitionT lower <<<
    Combination.lift (SProxy :: SProxy "content") <<<
      Router.selectFeed

openModal :: forall m. TransitionT Space m Unit
openModal =
  hoistTransitionT lower $
    Combination.lift (SProxy :: SProxy "modal") $
      NewFeedModal.show

closeModal :: forall m. TransitionT Space m Unit
closeModal =
  hoistTransitionT lower $
    Combination.lift (SProxy :: SProxy "modal") $
      NewFeedModal.hide
