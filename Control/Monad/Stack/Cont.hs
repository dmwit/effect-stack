{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Cont where

import Control.Monad.Cont
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

class Monad m => ContStack m where
	type PopCont m :: * -> *
	liftCont :: PopCont m a -> m a

type instance Pop ContTag m = PopCont m
type ContDepth n m = IteratePop n ContTag m
type ContConstraints n m = (KnownNat n, StackConstraints n ContTag ContStack m)
type MonadContDepth n m = (ContConstraints n m, MonadCont (ContDepth n m))

depthCont :: forall n m a. ContConstraints n m => ContDepth n m a -> m a
depthCont = depth @n @ContTag @ContStack liftCont

instance (ContStack m, Monoid w) => ContStack (AccumT w m) where
	type PopCont (AccumT w m) = PopCont m
	liftCont = lift . liftCont

instance Monad m => ContStack (ContT r m) where
	type PopCont (ContT r m) = m
	liftCont = lift

instance ContStack m => ContStack (ExceptT e m) where
	type PopCont (ExceptT e m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (IdentityT m) where
	type PopCont (IdentityT m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (MaybeT m) where
	type PopCont (MaybeT m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (RC.RWST r w s m) where
	type PopCont (RC.RWST r w s m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (RL.RWST r w s m) where
	type PopCont (RL.RWST r w s m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (RS.RWST r w s m) where
	type PopCont (RS.RWST r w s m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (ReaderT r m) where
	type PopCont (ReaderT r m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (SelectT r m) where
	type PopCont (SelectT r m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (SL.StateT s m) where
	type PopCont (SL.StateT s m) = PopCont m
	liftCont = lift . liftCont

instance ContStack m => ContStack (SS.StateT s m) where
	type PopCont (SS.StateT s m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (WC.WriterT w m) where
	type PopCont (WC.WriterT w m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (WL.WriterT w m) where
	type PopCont (WL.WriterT w m) = PopCont m
	liftCont = lift . liftCont

instance (ContStack m, Monoid w) => ContStack (WS.WriterT w m) where
	type PopCont (WS.WriterT w m) = PopCont m
	liftCont = lift . liftCont
