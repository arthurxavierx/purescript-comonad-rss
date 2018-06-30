module RSS.App.ViewFeed where

import Effect.Aff (launchAff_)
import RSS (Item(..))
import RSS.App.Pages as Pages
import React.DOM as D
import React.DOM.Props as P
import UI.React (ReactUI, ReactComponent)

type Space = Pages.Space
type Comm = Pages.Comm (Item String)

component :: ReactComponent Space Comm
component = Pages.component renderItem renderEmpty 8
  where
    renderEmpty :: ReactUI Space Comm
    renderEmpty _ _ = D.div' [ D.text "No new items." ]

    renderItem :: Item String -> ReactUI Space Comm
    renderItem item@(Item { title, snippet }) _ { onSelectItem } =
      D.div
        [ P.className "Item ItemPreview"
        , P.role "link"
        , P.onClick \_ -> launchAff_ (onSelectItem item)
        ]
        [ D.h4' [ D.text title ]
        , D.p' [ D.text snippet ]
        ]
