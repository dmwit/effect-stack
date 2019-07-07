{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.State where

import Control.Monad.State
import Control.Monad.Stack.Internal
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.CPS as RC
import Control.Monad.Trans.RWS.Lazy as RL
import Control.Monad.Trans.RWS.Strict as RS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import Control.Monad.Trans.State.Lazy as SL
import Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Writer.CPS as WC
import Control.Monad.Trans.Writer.Lazy as WL
import Control.Monad.Trans.Writer.Strict as WS

class Monad m => StateStack m where
	type PopState m :: * -> *
	liftState :: PopState m a -> m a

type instance Pop SL.StateT m = PopState m
type StateDepth n m = IteratePop n SL.StateT m
type StateConstraints n m = (KnownNat n, StackConstraints n SL.StateT StateStack m)
type MonadStateDepth n m s = (StateConstraints n m, MonadState s (StateDepth n m))

depthState :: forall n m a. StateConstraints n m => StateDepth n m a -> m a
depthState = depth @n @SL.StateT @StateStack liftState

instance (StateStack m, Monoid w) => StateStack (AccumT w m) where
	type PopState (AccumT w m) = PopState m
	liftState = lift . liftState

instance StateStack m => StateStack (ContT r m) where
	type PopState (ContT r m) = PopState m
	liftState = lift . liftState

instance StateStack m => StateStack (ExceptT e m) where
	type PopState (ExceptT e m) = PopState m
	liftState = lift . liftState

instance StateStack m => StateStack (IdentityT m) where
	type PopState (IdentityT m) = PopState m
	liftState = lift . liftState

instance StateStack m => StateStack (MaybeT m) where
	type PopState (MaybeT m) = PopState m
	liftState = lift . liftState

instance (Monad m, Monoid w) => StateStack (RC.RWST r w s m) where
	type PopState (RC.RWST r w s m) = m
	liftState = lift

instance (Monad m, Monoid w) => StateStack (RL.RWST r w s m) where
	type PopState (RL.RWST r w s m) = m
	liftState = lift

instance (Monad m, Monoid w) => StateStack (RS.RWST r w s m) where
	type PopState (RS.RWST r w s m) = m
	liftState = lift

instance StateStack m => StateStack (ReaderT r m) where
	type PopState (ReaderT r m) = PopState m
	liftState = lift . liftState

instance StateStack m => StateStack (SelectT r m) where
	type PopState (SelectT r m) = PopState m
	liftState = lift . liftState

instance Monad m => StateStack (SL.StateT s m) where
	type PopState (SL.StateT s m) = m
	liftState = lift

instance Monad m => StateStack (SS.StateT s m) where
	type PopState (SS.StateT s m) = m
	liftState = lift

instance (StateStack m, Monoid w) => StateStack (WC.WriterT w m) where
	type PopState (WC.WriterT w m) = PopState m
	liftState = lift . liftState

instance (StateStack m, Monoid w) => StateStack (WL.WriterT w m) where
	type PopState (WL.WriterT w m) = PopState m
	liftState = lift . liftState

instance (StateStack m, Monoid w) => StateStack (WS.WriterT w m) where
	type PopState (WS.WriterT w m) = PopState m
	liftState = lift . liftState
