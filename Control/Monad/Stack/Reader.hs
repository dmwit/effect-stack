{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Reader where

import Control.Monad.Reader
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

class Monad m => ReaderStack m where
	type PopReader m :: * -> *
	liftReader :: PopReader m a -> m a

type instance Pop ReaderT m = PopReader m
type ReaderDepth n m = IteratePop n ReaderT m
type ReaderConstraints n m = (KnownNat n, StackConstraints n ReaderT ReaderStack m)
type MonadReaderDepth n m r = (ReaderConstraints n m, MonadReader r (ReaderDepth n m))

depthReader :: forall n m a. ReaderConstraints n m => ReaderDepth n m a -> m a
depthReader = depth @n @ReaderT @ReaderStack liftReader

instance (ReaderStack m, Monoid w) => ReaderStack (AccumT w m) where
	type PopReader (AccumT w m) = PopReader m
	liftReader = lift . liftReader

instance ReaderStack m => ReaderStack (ContT r m) where
	type PopReader (ContT r m) = PopReader m
	liftReader = lift . liftReader

instance Monad m => ReaderStack (ExceptT e m) where
	type PopReader (ExceptT e m) = m
	liftReader = lift

instance ReaderStack m => ReaderStack (IdentityT m) where
	type PopReader (IdentityT m) = PopReader m
	liftReader = lift . liftReader

instance ReaderStack m => ReaderStack (MaybeT m) where
	type PopReader (MaybeT m) = PopReader m
	liftReader = lift . liftReader

instance (Monad m, Monoid w) => ReaderStack (RC.RWST r w s m) where
	type PopReader (RC.RWST r w s m) = m
	liftReader = lift

instance (Monad m, Monoid w) => ReaderStack (RL.RWST r w s m) where
	type PopReader (RL.RWST r w s m) = m
	liftReader = lift

instance (Monad m, Monoid w) => ReaderStack (RS.RWST r w s m) where
	type PopReader (RS.RWST r w s m) = m
	liftReader = lift

instance Monad m => ReaderStack (ReaderT r m) where
	type PopReader (ReaderT r m) = m
	liftReader = lift

instance ReaderStack m => ReaderStack (SelectT r m) where
	type PopReader (SelectT r m) = PopReader m
	liftReader = lift . liftReader

instance ReaderStack m => ReaderStack (SL.StateT s m) where
	type PopReader (SL.StateT s m) = PopReader m
	liftReader = lift . liftReader

instance ReaderStack m => ReaderStack (SS.StateT s m) where
	type PopReader (SS.StateT s m) = PopReader m
	liftReader = lift . liftReader

instance (ReaderStack m, Monoid w) => ReaderStack (WC.WriterT w m) where
	type PopReader (WC.WriterT w m) = PopReader m
	liftReader = lift . liftReader

instance (ReaderStack m, Monoid w) => ReaderStack (WL.WriterT w m) where
	type PopReader (WL.WriterT w m) = PopReader m
	liftReader = lift . liftReader

instance (ReaderStack m, Monoid w) => ReaderStack (WS.WriterT w m) where
	type PopReader (WS.WriterT w m) = PopReader m
	liftReader = lift . liftReader
