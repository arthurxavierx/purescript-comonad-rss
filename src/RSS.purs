module RSS where

import Prelude

import Data.DateTime (DateTime)
import Data.Eq (class Eq1)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

type FeedData metadata =
  { url :: String
  , title :: String
  , description :: String
  | metadata
  }

--
newtype Feed a = Feed (FeedData (items :: Array (Item a)))
type Feeds a = Array (Feed a)

derive instance newtypeFeed :: Newtype (Feed a) _
derive instance functorFeed :: Functor Feed
instance showFeed :: Show a => Show (Feed a) where
  show (Feed _data) = "Feed { "
    <> "url: " <> show _data.url <> ", "
    <> "title: " <> show _data.title <> ", "
    <> "description: " <> show _data.description <> ", "
    <> "items: " <> show _data.items
    <> " }"

--
newtype Item a = Item
  { title :: String
  , author :: String
  , url :: String
  , date :: Maybe DateTime
  , content :: a
  , snippet :: String
  , comments :: Comments a
  }

type Comments a = Map Int a

derive instance newtypeItem :: Newtype (Item a) _
derive instance functorItem :: Functor Item
derive instance eqItem :: Eq a => Eq (Item a)
instance eq1Item :: Eq1 Item where
  eq1 = eq

instance showItem :: Show a => Show (Item a) where
  show (Item item) = "Item { "
    <> "title: " <> show item.title <> ", "
    <> "author: " <> show item.author <> ", "
    <> "url: " <> show item.url <> ", "
    <> "date: " <> show item.date <> ", "
    <> "snippet: " <> show item.snippet <> ", "
    <> "comments: " <> show item.comments
    <> " }"
