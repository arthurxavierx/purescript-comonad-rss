module Control.Comonad.Sum
  ( Sum
  , sum
  , lower
  , extractOther
  , lowerLeft
  , lowerRight
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (=>>))
import Data.Functor.Coproduct (Coproduct, left, right)

data LR = L | R

data Sum f g a = Sum LR (f a) (g a)

sum :: forall f g a. f a -> g a -> Sum f g a
sum fa ga = Sum L fa ga

derive instance functorSum :: (Functor f, Functor g) => Functor (Sum f g)

instance extendSum :: (Extend f, Extend g) => Extend (Sum f g) where
  extend f (Sum s fa ga) =
    Sum s
      (fa =>> f <<< flip (Sum L) ga)
      (ga =>> f <<< Sum R fa)

instance comonadSum :: (Comonad f, Comonad g) => Comonad (Sum f g) where
  extract (Sum L fa _) = extract fa
  extract (Sum R _ ga) = extract ga

lower :: forall f g a. Sum f g a -> Coproduct f g a
lower (Sum L fa _) = left fa
lower (Sum R _ ga) = right ga

extractOther :: forall f g a. Comonad f => Comonad g => Sum f g a -> a
extractOther (Sum L _ ga) = extract ga
extractOther (Sum R fa _) = extract fa

lowerLeft :: forall f g a. Sum f g a -> f a
lowerLeft (Sum _ fa _) = fa

lowerRight :: forall f g a. Sum f g a -> g a
lowerRight (Sum _ _ ga) = ga
