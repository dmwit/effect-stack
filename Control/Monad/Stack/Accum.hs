{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Stack.Accum where

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

class Monad m => AccumStack m where
	type PopAccum m :: Type -> Type
	liftAccum :: PopAccum m a -> m a

type instance Pop AccumT m = PopAccum m
type AccumDepth n m = IteratePop n AccumT m
type AccumConstraints n m = (KnownNat n, StackConstraints n AccumT AccumStack m)

depthAccum :: forall n m a. AccumConstraints n m => AccumDepth n m a -> m a
depthAccum = depth @n @AccumT @AccumStack liftAccum

instance (Monad m, Monoid w) => AccumStack (AccumT w m) where
	type PopAccum (AccumT w m) = m
	liftAccum = lift

instance AccumStack m => AccumStack (ContT r m) where
	type PopAccum (ContT r m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (ExceptT e m) where
	type PopAccum (ExceptT e m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (IdentityT m) where
	type PopAccum (IdentityT m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (MaybeT m) where
	type PopAccum (MaybeT m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (RC.RWST r w s m) where
	type PopAccum (RC.RWST r w s m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (RL.RWST r w s m) where
	type PopAccum (RL.RWST r w s m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (RS.RWST r w s m) where
	type PopAccum (RS.RWST r w s m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (ReaderT r m) where
	type PopAccum (ReaderT r m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (SelectT r m) where
	type PopAccum (SelectT r m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (SL.StateT s m) where
	type PopAccum (SL.StateT s m) = PopAccum m
	liftAccum = lift . liftAccum

instance AccumStack m => AccumStack (SS.StateT s m) where
	type PopAccum (SS.StateT s m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (WC.WriterT w m) where
	type PopAccum (WC.WriterT w m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (WL.WriterT w m) where
	type PopAccum (WL.WriterT w m) = PopAccum m
	liftAccum = lift . liftAccum

instance (AccumStack m, Monoid w) => AccumStack (WS.WriterT w m) where
	type PopAccum (WS.WriterT w m) = PopAccum m
	liftAccum = lift . liftAccum
