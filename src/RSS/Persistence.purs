module RSS.Persistence where

import Prelude

import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import RSS (FeedData, Feed(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype PersistedFeeds = PersistedFeeds (Array (FeedData ()))
derive instance newtypePersistedFeeds :: Newtype PersistedFeeds _
derive newtype instance readForeignPersistedFeeds :: ReadForeign PersistedFeeds
derive newtype instance writeForeignPersistedFeeds :: WriteForeign PersistedFeeds

save :: forall a. Array (Feed a) -> Effect Unit
save feeds = do
  let model = PersistedFeeds (getMetadata <$> feeds)
  setItem feedsKey (writeJSON model) =<< localStorage =<< window
  where
    getMetadata (Feed {url, title, description}) = {url, title, description}

load :: Effect (Array (FeedData ()))
load = do
  modelM <- getItem feedsKey =<< localStorage =<< window
  pure $ fromMaybe [] do
    PersistedFeeds feeds <- hush <<< readJSON =<< modelM
    pure feeds

feedsKey :: String
feedsKey = "CoRSS_feeds"
