{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Error where

import Control.Monad.Except
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

class Monad m => ErrorStack m where
	type PopError m :: * -> *
	liftError :: PopError m a -> m a

type instance Pop ExceptT m = PopError m
type ErrorDepth n m = IteratePop n ExceptT m
type ErrorConstraints n m = (KnownNat n, StackConstraints n ExceptT ErrorStack m)
type MonadErrorDepth n m e = (ErrorConstraints n m, MonadError e (ErrorDepth n m))

depthError :: forall n m a. ErrorConstraints n m => ErrorDepth n m a -> m a
depthError = depth @n @ExceptT @ErrorStack liftError

instance (ErrorStack m, Monoid w) => ErrorStack (AccumT w m) where
	type PopError (AccumT w m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (ContT r m) where
	type PopError (ContT r m) = PopError m
	liftError = lift . liftError

instance Monad m => ErrorStack (ExceptT e m) where
	type PopError (ExceptT e m) = m
	liftError = lift

instance ErrorStack m => ErrorStack (IdentityT m) where
	type PopError (IdentityT m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (MaybeT m) where
	type PopError (MaybeT m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (RC.RWST r w s m) where
	type PopError (RC.RWST r w s m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (RL.RWST r w s m) where
	type PopError (RL.RWST r w s m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (RS.RWST r w s m) where
	type PopError (RS.RWST r w s m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (ReaderT r m) where
	type PopError (ReaderT r m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (SelectT r m) where
	type PopError (SelectT r m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (SL.StateT s m) where
	type PopError (SL.StateT s m) = PopError m
	liftError = lift . liftError

instance ErrorStack m => ErrorStack (SS.StateT s m) where
	type PopError (SS.StateT s m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (WC.WriterT w m) where
	type PopError (WC.WriterT w m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (WL.WriterT w m) where
	type PopError (WL.WriterT w m) = PopError m
	liftError = lift . liftError

instance (ErrorStack m, Monoid w) => ErrorStack (WS.WriterT w m) where
	type PopError (WS.WriterT w m) = PopError m
	liftError = lift . liftError
