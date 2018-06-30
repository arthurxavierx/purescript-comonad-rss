module UI.React where

import Prelude

import Control.Comonad (class Comonad)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React as R
import UI (Component, UI)
import UI as UI

-- | A `ReactUI` is a `UI` whose action handlers run in the `Eff` monad, producing
-- | interfaces described by `ReactElement`s.
type ReactUI w f = UI Aff w (f R.ReactElement)

-- | A `ReactComponent` is a comonad `w` describing a space of all future possible
-- | `ReactUI`s.
type ReactComponent w f = Component Aff w f R.ReactElement

type Algebra f = forall a. f a -> a

-- | Explore a `ReactComponent` whose comonad and monad do form a `Pairing` by producing
-- | a `ReactClass` which can be rendered using React.
explore :: forall w f. Comonad w => Algebra f -> ReactComponent w f -> R.ReactClass {}
explore alg initialSpace =
  R.pureComponent "ReactComponent" \this ->
    pure
      { state: { space: initialSpace }
      , render: do
          { space } <- R.getState this
          pure $ alg $
            UI.explore'
              (_.space <$> liftEffect (R.getState this))
              (liftEffect <<< R.setState this <<< { space: _ })
              space
      }

-- | Instantiate a `ReactComponent` as a `ReactElement`
toReact :: forall w f. Comonad w => Algebra f -> ReactComponent w f -> R.ReactElement
toReact alg component = flip R.unsafeCreateLeafElement {} (explore alg component)
