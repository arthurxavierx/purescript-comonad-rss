module UI.Zipper
  ( module Z
  , moveLeft
  , moveRight
  , moveToFirst
  , moveToLast
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Zipper (Zipper(..), goLeft, goRight) as Z
import Control.Monad.Transition.Trans (TransitionT(..))
import Data.List (List(..))

moveLeft :: forall m. TransitionT Z.Zipper m Unit
moveLeft = TransitionT \z -> extract (Z.goLeft z) unit

moveRight :: forall m. TransitionT Z.Zipper m Unit
moveRight = TransitionT \z -> extract (Z.goRight z) unit

moveToFirst :: forall m. TransitionT Z.Zipper m Unit
moveToFirst = TransitionT \z -> extract (goToFirst z) unit
  where
    goToFirst :: Z.Zipper ~> Z.Zipper
    goToFirst z@(Z.Zipper Nil _ _) = z
    goToFirst z = goToFirst (Z.goLeft z)

moveToLast :: forall m. TransitionT Z.Zipper m Unit
moveToLast = TransitionT \z -> extract (goToLast z) unit
  where
    goToLast :: Z.Zipper ~> Z.Zipper
    goToLast z@(Z.Zipper Nil _ _) = z
    goToLast z = goToLast (Z.goRight z)
