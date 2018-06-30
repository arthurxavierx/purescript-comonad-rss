module Control.Comonad.Day where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Functor.Day (Day, runDay)

lowerDay1 :: forall w1 w2. Functor w1 => Comonad w2 => Day w1 w2 ~> w1
lowerDay1 = runDay (\f w s -> map (_ `f` extract s) w)

lowerDay2 :: forall w1 w2. Comonad w1 => Functor w2 => Day w1 w2 ~> w2
lowerDay2 = runDay (\f w s -> map (f (extract w)) s)
