module Main where

import Prelude

import Data.Array (null)
import Data.Filterable (filterMap)
import Data.Lazy (force)
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import RSS (FeedData, Feeds)
import RSS.App as App
import RSS.Parser (parseURL)
import RSS.Persistence (load) as Persistence
import ReactDOM (render)
import UI.React (toReact)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

main :: Effect Unit
main = launchAff_ (liftEffect <<< renderApp =<< loadFeeds)
  where
    loadFeeds :: Aff (Feeds String)
    loadFeeds = do
      feedData <- liftEffect getSavedFeedData
      feedMs <- sequential $ traverse parallel (parseURL <<< _.url <$> feedData)
      pure (filterMap identity feedMs)

    getSavedFeedData :: Effect (Array (FeedData ()))
    getSavedFeedData = do
      feedData <- Persistence.load
      if null feedData
        then pure initialFeedData
        else pure feedData

renderApp :: Feeds String -> Effect Unit
renderApp feeds = do
  document <- DOM.window >>= DOM.document
  appDiv <- DOM.getElementById "app" (DOM.toNonElementParentNode document)
  for_ appDiv $ render (toReact force (App.component feeds))

initialFeedData :: Array (FeedData ())
initialFeedData =
  [ { url: "https://www.nasa.gov/rss/dyn/breaking_news.rss"
    , title: "NASA Breaking News"
    , description: "A RSS news feed containing the latest NASA news articles and press releases."
    }
  , { url: "https://www.reddit.com/.rss"
    , title: "reddit: the front page of the internet"
    , description: ""
    }
  ]
