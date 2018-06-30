module Control.Comonad.Zipper where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List(Nil), mapWithIndex, reverse, (:))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Unfoldable (replicate)

data Zipper a = Zipper (List a) a (List a)

derive instance functorZipper :: Functor Zipper

goLeft :: Zipper ~> Zipper
goLeft z@(Zipper Nil _ _) = z
goLeft (Zipper (b:bs) a after) = Zipper bs b (a:after)

goRight :: Zipper ~> Zipper
goRight z@(Zipper _ _ Nil) = z
goRight (Zipper before a (b:bs)) = Zipper (a:before) b bs

instance extendZipper :: Extend Zipper where
  extend f = map f <<< dup
    where
      dup :: forall a. Zipper a -> Zipper (Zipper a)
      dup z@(Zipper before a after) =
        Zipper
          (mapWithIndex (\i _ -> applyRepeated (i + 1) goLeft z) before)
          z
          (mapWithIndex (\i _ -> applyRepeated (i + 1) goRight z) after)

      applyRepeated :: forall a. Int -> (a -> a) -> (a -> a)
      applyRepeated i g = unwrap $ foldMap Endo (replicate i g :: List (a -> a))

instance comonadZipper :: Comonad Zipper where
  extract (Zipper _ a _) = a

instance foldableZipper :: Foldable Zipper where
  foldr f y (Zipper before a after) = foldr f y (reverse before <> pure a <> after)
  foldl f y (Zipper before a after) = foldl f y (reverse before <> pure a <> after)
  foldMap f (Zipper before a after) = foldMap f (reverse before <> pure a <> after)
