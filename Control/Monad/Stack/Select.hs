{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Select where

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

import Data.Kind

class Monad m => SelectStack m where
	type PopSelect m :: Type -> Type
	liftSelect :: PopSelect m a -> m a

type instance Pop SelectT m = PopSelect m
type SelectDepth n m = IteratePop n SelectT m
type SelectConstraints n m = (KnownNat n, StackConstraints n SelectT SelectStack m)

depthSelect :: forall n m a. SelectConstraints n m => SelectDepth n m a -> m a
depthSelect = depth @n @SelectT @SelectStack liftSelect

instance (SelectStack m, Monoid w) => SelectStack (AccumT w m) where
	type PopSelect (AccumT w m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (ContT r m) where
	type PopSelect (ContT r m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (ExceptT e m) where
	type PopSelect (ExceptT e m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (IdentityT m) where
	type PopSelect (IdentityT m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (MaybeT m) where
	type PopSelect (MaybeT m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (RC.RWST r w s m) where
	type PopSelect (RC.RWST r w s m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (RL.RWST r w s m) where
	type PopSelect (RL.RWST r w s m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (RS.RWST r w s m) where
	type PopSelect (RS.RWST r w s m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (ReaderT r m) where
	type PopSelect (ReaderT r m) = PopSelect m
	liftSelect = lift . liftSelect

instance Monad m => SelectStack (SelectT r m) where
	type PopSelect (SelectT r m) = m
	liftSelect = lift

instance SelectStack m => SelectStack (SL.StateT s m) where
	type PopSelect (SL.StateT s m) = PopSelect m
	liftSelect = lift . liftSelect

instance SelectStack m => SelectStack (SS.StateT s m) where
	type PopSelect (SS.StateT s m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (WC.WriterT w m) where
	type PopSelect (WC.WriterT w m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (WL.WriterT w m) where
	type PopSelect (WL.WriterT w m) = PopSelect m
	liftSelect = lift . liftSelect

instance (SelectStack m, Monoid w) => SelectStack (WS.WriterT w m) where
	type PopSelect (WS.WriterT w m) = PopSelect m
	liftSelect = lift . liftSelect
