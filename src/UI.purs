module UI where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans, lower)
import Control.Extend (extend)
import Control.Monad.Transition.Trans (TransitionT, hoistTransitionT, pairTransitionT)

-- | A `Handler` is an event handler running in a `m` monad for actions of type `action`.
type Handler m w = TransitionT w m Unit -> m Unit

-- | A `UI` is a function which outputs a description of type `a` of an interface given
-- | a way to handle the `action`s dispatched by this interface through a `Handler`.
type UI m w a = Handler m w -> a

-- | A component is a comonad `w` representing the space of all possible future `UI`s.
-- | Through the use of `Pairing`s, one can `pair` this comonad `w` with a monad `m` for
-- | moving around in this space and, thus, modifying the state of the component.
-- |
-- | In this way, a `Component` is a comonad `w` full of future `UI`s whose `Handler`s
-- | handle (in a `m` monad) actions of type `m Unit` dispatched by an interface of
-- | type `a`.
type Component m w f a = w (UI m w (f a))

-- | Given a way of writing/updating the component's state within a `m` monad and
-- | a current or initial `Component` (a space of interfaces), to `explore` a component
-- | means to get the current interface out of the component's comonad by wiring up the
-- | action handler with the `write` function supplied.
-- |
-- | This is a generic function for exploring the space of states defined by a comonad.
-- | Specific UI renderers must define their own specific derivations to effectively
-- | create the stateful components.
explore
  :: forall m w f a
   . Comonad w
  => Monad m
  => (Component m w f a -> m Unit)
  -> Component m w f a
  -> f a
explore write space = extract space send
  where
    send :: Handler m w
    send transition =
      pairTransitionT (const identity) transition (extend pure space)
      >>= write

explore'
  :: forall m w f a
   . Comonad w
  => Monad m
  => (m (Component m w f a))
  -> (Component m w f a -> m Unit)
  -> Component m w f a
  -> f a
explore' read write space = extract space send
  where
    send :: Handler m w
    send transition =
      read
      >>= pairTransitionT (const identity) transition <<< extend pure
      >>= write

-- | TODO: write documentation
liftUI
  :: forall m w wp
   . Comonad w
  => Functor m
  => wp ~> w
  -> UI m w ~> UI m wp
liftUI lower ui = \send -> ui (send <<< hoistTransitionT lower)

-- | TODO: write documentation
liftUIT
  :: forall m w wp
   . Comonad w
  => Functor m
  => ComonadTrans wp
  => UI m w ~> UI m (wp w)
liftUIT = liftUI lower

-- | TODO: write documentation
liftComponent
  :: forall m w wp f a
   . Comonad w
  => Functor m
  => wp ~> w
  -> Component m w f a -> UI m wp (f a)
liftComponent lower = liftUI lower <<< extract

-- | TODO: write documentation
liftComponentT
  :: forall m w wp f a
   . Comonad w
  => Functor m
  => ComonadTrans wp
  => Component m w f a -> UI m (wp w) (f a)
liftComponentT = liftComponent lower

{-- -- | Allow for the execution of monadic actions in the `m` monad (with access to the --}
{-- -- | current state of the component) in response to every user action within a --}
{-- -- | `Component`. --}
{-- -- | --}
{-- -- | This function is a comonadic combinator. --}
{-- effect --}
{--   :: forall m w a --}
{--    . Monad m --}
{--   => Comonad w --}
{--   => (w Unit -> m Unit) --}
{--   -> Component m w a --}
{--   -> UI m w a --}
{-- effect eff component send = extract component \transition -> do --}
{--   w <- send transition --}
{--   eff w --}
{--   pure w --}
