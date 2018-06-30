module UI.Day
  ( module D
  , combine
  , liftLeft
  , liftRight
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Monad.Transition.Trans (TransitionT(..), runTransitionT)
import Data.Functor.Day (Day, day, runDay) as D
import UI (Component, UI, liftUI)

combine
  :: forall w1 w2 m f a
   . Functor m
  => Comonad w1
  => Comonad w2
  => Component m w1 f a
  -> Component m w2 f a
  -> (UI m (D.Day w1 w2) (f a) -> UI m (D.Day w1 w2) (f a) -> UI m (D.Day w1 w2) (f a))
  -> Component m (D.Day w1 w2) f a
combine c1 c2 append = D.day build c1 c2
  where
    build ui1 ui2 = append (liftUI lowerLeft ui1) (liftUI lowerRight ui2)

liftLeft :: forall w1 w2 m. Functor w1 => Comonad w2 => TransitionT w1 m ~> TransitionT (D.Day w1 w2) m
liftLeft t = TransitionT (runTransitionT t <<< lowerLeft)

liftRight :: forall w1 w2 m. Functor w2 => Comonad w1 => TransitionT w2 m ~> TransitionT (D.Day w1 w2) m
liftRight t = TransitionT (runTransitionT t <<< lowerRight)

lowerLeft :: forall f g. Functor f => Comonad g => D.Day f g ~> f
lowerLeft = D.runDay (\get f g -> map (_ `get` extract g) f)

lowerRight :: forall f g. Functor g => Comonad f => D.Day f g ~> g
lowerRight = D.runDay (\get f g -> map (get (extract f)) g)
