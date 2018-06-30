module RSS.App.ViewItem where

import Control.Comonad.Store (Store, store)
import Data.Maybe (Maybe(..))
import RSS (Item(..))
import React.DOM as D
import React.DOM.Props as P
import UI.React (ReactUI, ReactComponent)

type Space = Store (Maybe (Item String))
type Comm = (->) {}

component :: ReactComponent Space Comm
component = store render Nothing
  where
    render :: Maybe (Item String) -> ReactUI Space Comm
    render Nothing _ _ = D.div' []
    render (Just (Item item)) send _ =
      D.article
        [ P.className "column flex"
        ]
        [ D.h1' [ D.text item.title ]
        , D.div
            [ P.className "column flex"
            , P.dangerouslySetInnerHTML { __html: item.content }
            ] []
        ]
