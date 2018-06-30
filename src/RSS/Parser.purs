module RSS.Parser
  ( parseURL
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.String (take) as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import RSS (Feed(..), Item(..))

type FeedFFI =
  { title :: String
  , url :: String
  , description :: String
  , items :: Array ItemFFI
  }

type ItemFFI =
  { title :: String
  , author :: String
  , url :: String
  , date :: String
  , content :: String
  , snippet :: String
  , comments :: Void
  }

foreign import _parseURL :: String -> EffectFnAff FeedFFI

_CORS_PROXY :: String
_CORS_PROXY = "https://cors-anywhere.herokuapp.com/"

parseURL :: String -> Aff (Maybe (Feed String))
parseURL url =
  Just <$> (liftEffect <<< toFeed =<< fromEffectFnAff (_parseURL (_CORS_PROXY <> url)))
  <|>
  pure Nothing
  where
    toFeed feed = do
      items <- traverse toItem feed.items
      pure $ Feed (feed { items = items })

    toItem item = do
      date <- JSDate.toDateTime <$> JSDate.parse item.date
      pure $ Item $ item
        { date = date
        , snippet = String.take 140 item.snippet <> "…"
        , comments = mempty
        }
