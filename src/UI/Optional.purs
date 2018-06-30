module UI.Optional
  ( module S
  , Optional
  , optional
  , optional_
  , lift
  , hide
  , show
  , toggle
  ) where

import Prelude hiding (show)

import Control.Comonad (class Comonad)
import Control.Monad.Transition.Trans (TransitionT(..))
import Data.Identity (Identity(..))
import UI (Component, UI)
import UI.Sum as S

type Optional = S.Sum Identity

optional
  :: forall m w f a
   . Functor m
  => Comonad w
  => UI m Identity (f a)
  -> Component m w f a
  -> Component m (Optional w) f a
optional = S.combine <<< Identity

optional_
  :: forall m w f a
   . Functor m
  => Comonad w
  => f a
  -> Component m w f a
  -> Component m (Optional w) f a
optional_ = S.combine <<< Identity <<< const

lift :: forall w m. TransitionT w m ~> TransitionT (Optional w) m
lift = S.liftRight

hide :: forall w m. TransitionT (Optional w) m Unit
hide = S.moveLeft

show :: forall w m. Comonad w => TransitionT (Optional w) m Unit
show = S.moveRight

toggle :: forall w m. Comonad w => TransitionT (Optional w) m Unit
toggle = TransitionT \s -> S.extractOther s unit
