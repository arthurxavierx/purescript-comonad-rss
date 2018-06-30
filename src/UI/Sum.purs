module UI.Sum
  ( module S
  , combine
  , liftLeft
  , liftRight
  , moveLeft
  , moveRight
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Sum as S
import Control.Monad.Transition.Trans (TransitionT(..), runTransitionT)
import UI (Component, liftUI)

combine
  :: forall w1 w2 m f a
   . Functor m
  => Comonad w1
  => Comonad w2
  => Component m w1 f a
  -> Component m w2 f a
  -> Component m (S.Sum w1 w2) f a
combine c1 c2 = S.sum (c1 <#> liftUI S.lowerLeft) (c2 <#> liftUI S.lowerRight)

liftLeft :: forall f g m. TransitionT f m ~> TransitionT (S.Sum f g) m
liftLeft t = TransitionT (runTransitionT t <<< S.lowerLeft)

liftRight :: forall f g m. TransitionT g m ~> TransitionT (S.Sum f g) m
liftRight t = TransitionT (runTransitionT t <<< S.lowerRight)

moveLeft :: forall f g m. Comonad f => TransitionT (S.Sum f g) m Unit
moveLeft = TransitionT \s -> extract (S.lowerLeft s) unit

moveRight :: forall f g m. Comonad g => TransitionT (S.Sum f g) m Unit
moveRight = TransitionT \s -> extract (S.lowerRight s) unit
