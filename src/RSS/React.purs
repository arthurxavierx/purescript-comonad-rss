module RSS.React where

import RSS (Item(..))
import React (ReactElement)
import React.DOM (div) as D
import React.DOM.Props (dangerouslySetInnerHTML) as P

{-- itemCard :: forall a. Item a -> ReactElement --}
{-- itemCard = ?itemCard --}

itemContent :: Item String -> ReactElement
itemContent (Item { content }) =
  D.div [ P.dangerouslySetInnerHTML { __html: content } ] []
