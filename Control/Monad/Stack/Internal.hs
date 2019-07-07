{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Stack.Internal
	( module Control.Monad.Stack.Internal
	, module GHC.TypeNats
	) where

import Data.Constraint
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import GHC.TypeNats
import Unsafe.Coerce

type family Pop (c :: k) (m :: k') :: k'

type family IteratePop (n :: Nat) (c :: k) (m :: Type -> Type) :: Type -> Type where
	IteratePop 0 c m = m
	IteratePop n c m = IteratePop (n-1) c (Pop c m)

type family StackConstraints (n :: Nat) (c :: k) (cSucc :: k' -> Constraint) (m :: k') :: Constraint where
	StackConstraints 0 c cSucc m = ()
	StackConstraints n c cSucc m = (cSucc m, StackConstraints (n-1) c cSucc (Pop c m))

predNat :: forall n. KnownNat n => Either (n :~: 0) (Dict (KnownNat (n-1)))
predNat = case sameNat (Proxy @0) (Proxy @n) of
	Just Refl -> Left Refl
	Nothing -> case someNatVal (natVal @n Proxy - 1) of
		SomeNat p -> Right (unsafeCoerce (wrap p))
	where
	wrap :: KnownNat n' => Proxy n' -> Dict (KnownNat n')
	wrap _ = Dict

nonZeroNat :: forall n c cSucc m a.
	KnownNat (n-1) =>
	( IteratePop n c m a :~: IteratePop (n-1) c (Pop c m) a
	, StackConstraints n c cSucc m :~: (cSucc m, StackConstraints (n-1) c cSucc (Pop c m))
	)
nonZeroNat = unsafeCoerce (Refl, Refl)

depth :: forall n c cSucc m a.
	(KnownNat n, StackConstraints n c cSucc m) =>
	(forall m. cSucc m => Pop c m a -> m a) ->
	IteratePop n c m a ->
	m a
depth lift act = case predNat @n of
	Left Refl -> act
	Right Dict -> case nonZeroNat @n @c @cSucc @m of
		(Refl, Refl) -> lift (depth @(n-1) @c @cSucc lift act)

-- | ContT is polykinded, which leads to issues checking that ContT ~ ContT!
data ContTag
