{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Stack.Fail where

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

class Monad m => FailStack m where
	type PopFail m :: * -> *
	liftFail :: PopFail m a -> m a

instance (FailStack m, Monoid w) => FailStack (AccumT w m) where
	type PopFail (AccumT w m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (ContT r m) where
	type PopFail (ContT r m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (ExceptT e m) where
	type PopFail (ExceptT e m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (IdentityT m) where
	type PopFail (IdentityT m) = PopFail m
	liftFail = lift . liftFail

instance Monad m => FailStack (MaybeT m) where
	type PopFail (MaybeT m) = m
	liftFail = lift

instance (FailStack m, Monoid w) => FailStack (RC.RWST r w s m) where
	type PopFail (RC.RWST r w s m) = PopFail m
	liftFail = lift . liftFail

instance (FailStack m, Monoid w) => FailStack (RL.RWST r w s m) where
	type PopFail (RL.RWST r w s m) = PopFail m
	liftFail = lift . liftFail

instance (FailStack m, Monoid w) => FailStack (RS.RWST r w s m) where
	type PopFail (RS.RWST r w s m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (ReaderT r m) where
	type PopFail (ReaderT r m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (SelectT r m) where
	type PopFail (SelectT r m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (SL.StateT s m) where
	type PopFail (SL.StateT s m) = PopFail m
	liftFail = lift . liftFail

instance FailStack m => FailStack (SS.StateT s m) where
	type PopFail (SS.StateT s m) = PopFail m
	liftFail = lift . liftFail

instance (FailStack m, Monoid w) => FailStack (WC.WriterT w m) where
	type PopFail (WC.WriterT w m) = PopFail m
	liftFail = lift . liftFail

instance (FailStack m, Monoid w) => FailStack (WL.WriterT w m) where
	type PopFail (WL.WriterT w m) = PopFail m
	liftFail = lift . liftFail

instance (FailStack m, Monoid w) => FailStack (WS.WriterT w m) where
	type PopFail (WS.WriterT w m) = PopFail m
	liftFail = lift . liftFail
