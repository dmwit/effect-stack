{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Writer where

import Control.Monad.Writer
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

class Monad m => WriterStack m where
	type PopWriter m :: * -> *
	liftWriter :: PopWriter m a -> m a

type instance Pop WL.WriterT m = PopWriter m
type WriterDepth n m = IteratePop n WL.WriterT m
type WriterConstraints n m = (KnownNat n, StackConstraints n WL.WriterT WriterStack m)
type MonadWriterDepth n m w = (WriterConstraints n m, MonadWriter w (WriterDepth n m))

depthWriter :: forall n m a. WriterConstraints n m => WriterDepth n m a -> m a
depthWriter = depth @n @WL.WriterT @WriterStack liftWriter

instance (WriterStack m, Monoid w) => WriterStack (AccumT w m) where
	type PopWriter (AccumT w m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (ContT r m) where
	type PopWriter (ContT r m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (ExceptT e m) where
	type PopWriter (ExceptT e m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (IdentityT m) where
	type PopWriter (IdentityT m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (MaybeT m) where
	type PopWriter (MaybeT m) = PopWriter m
	liftWriter = lift . liftWriter

instance (Monad m, Monoid w) => WriterStack (RC.RWST r w s m) where
	type PopWriter (RC.RWST r w s m) = m
	liftWriter = lift

instance (Monad m, Monoid w) => WriterStack (RL.RWST r w s m) where
	type PopWriter (RL.RWST r w s m) = m
	liftWriter = lift

instance (Monad m, Monoid w) => WriterStack (RS.RWST r w s m) where
	type PopWriter (RS.RWST r w s m) = m
	liftWriter = lift

instance WriterStack m => WriterStack (ReaderT r m) where
	type PopWriter (ReaderT r m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (SelectT r m) where
	type PopWriter (SelectT r m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (SL.StateT s m) where
	type PopWriter (SL.StateT s m) = PopWriter m
	liftWriter = lift . liftWriter

instance WriterStack m => WriterStack (SS.StateT s m) where
	type PopWriter (SS.StateT s m) = PopWriter m
	liftWriter = lift . liftWriter

instance (Monad m, Monoid w) => WriterStack (WC.WriterT w m) where
	type PopWriter (WC.WriterT w m) = m
	liftWriter = lift

instance (Monad m, Monoid w) => WriterStack (WL.WriterT w m) where
	type PopWriter (WL.WriterT w m) = m
	liftWriter = lift

instance (WriterStack m, Monoid w) => WriterStack (WS.WriterT w m) where
	type PopWriter (WS.WriterT w m) = PopWriter m
	liftWriter = lift . liftWriter
