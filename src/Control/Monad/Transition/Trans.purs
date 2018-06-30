-- | Monads from comonads,
-- | based on <https://github.com/paf31/purescript-pairing>.

module Control.Monad.Transition.Trans
  ( TransitionT(..)
  , runTransitionT
  , liftTransitionT
  , lowerTransitionT
  , hoistTransitionT
  , pairTransitionT
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Class (class ComonadAsk, class ComonadEnv, ask, local)
import Control.Comonad.Store.Class (class ComonadStore, peek, pos)
import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Tuple (Tuple(..))

newtype TransitionT w m a = TransitionT (forall r. w (a -> m r) -> m r)

runTransitionT :: forall w m a r. TransitionT w m a -> w (a -> m r) -> m r
runTransitionT (TransitionT t) = t

liftTransitionT :: forall w m s. Comonad w => (forall a. w a -> s) -> TransitionT w m s
liftTransitionT f = TransitionT (extract <*> f)

lowerTransitionT :: forall w m a s. Functor w => Applicative m => TransitionT w m s -> w a -> m s
lowerTransitionT t = runTransitionT t <<< (pure <$ _)

hoistTransitionT :: forall w v m. v ~> w -> TransitionT w m ~> TransitionT v m
hoistTransitionT nt (TransitionT t) = TransitionT (t <<< nt)

pairTransitionT :: forall w m a b c. Functor w => (a -> b -> m c) -> TransitionT w m a -> w b -> m c
pairTransitionT f t w = runTransitionT t (map (flip f) w)

instance cmttFT :: Functor w => Functor (TransitionT w m) where
  map f (TransitionT t) = TransitionT \w -> t (map (_ <<< f) w)

instance cmttAT :: Extend w => Apply (TransitionT w m) where
  apply (TransitionT f) (TransitionT a) = TransitionT \w -> f (w =>> \wf g -> a (map (_ <<< g) wf))

instance cmttA2T :: Comonad w => Applicative (TransitionT w m) where
  pure a = TransitionT \w -> extract w a

instance cmttBT :: Extend w => Bind (TransitionT w m) where
  bind (TransitionT k) f = TransitionT \w -> k (w =>> \wa a -> runTransitionT (f a) wa)

instance cmttMT :: Comonad w => Monad (TransitionT w m)

instance cmttMTransT :: Comonad w => MonadTrans (TransitionT w) where
  lift :: forall m a. Monad m => m a -> TransitionT w m a
  lift ma = TransitionT \w -> extract (map (ma >>= _) w)

instance cmttMAT :: ComonadAsk e w => MonadAsk e (TransitionT w m) where
  ask = liftTransitionT (ask :: forall a. w a -> e)

instance cmttMRT :: ComonadEnv e w => MonadReader e (TransitionT w m) where
  local f (TransitionT x) = TransitionT (x <<< local f)

instance cmttMST :: ComonadStore s w => MonadState s (TransitionT w m) where
  state f = do
    s <- liftTransitionT pos
    case f s of
      Tuple a s1 -> TransitionT \w -> peek s1 w a

instance cmttMTT :: ComonadTraced t w => MonadTell t (TransitionT w m) where
  tell t = TransitionT \w -> track t w unit
