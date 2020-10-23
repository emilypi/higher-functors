{-# language EmptyCase #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
-- |
-- Module       : Data.Functor.Higher
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module consists of the class definitions for 'HFunctor' and
-- 'SemiHFunctor' along with useful combinators for them.
--
module Data.Functor.Higher
( -- * Higher functors
  HFunctor(..)
, hlower
  -- * Type-indexed functors
, SemiHFunctor(..)
, semilower
) where


import Control.Applicative.Lift (Lift(..))
import Control.Monad.Trans.Accum (runAccumT, AccumT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))

import Data.Coerce (coerce)
import Data.Function.Higher (NT(..), type (~>))
import Data.Functor.Higher.Applied (Applied(..))
import Data.Functor.Higher.Identity (HIdentity(..))
import Data.Functor.Higher.Compose (HCompose(..))
import Data.Functor.Higher.Const (HConst(..), SemiHConst(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Kind (Type)
import Data.Proxy (Proxy)

import GHC.Generics

-- -------------------------------------------------------------------- --
-- Higher functors

class HFunctor (t :: (i -> Type) -> (j -> Type)) where
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap #-}

instance (HFunctor t, HFunctor u) => HFunctor (HCompose t u) where
  hmap f = HCompose . hmap (hmap f) . getHCompose
  {-# inline hmap #-}

instance HFunctor (HConst f) where
  hmap _ = coerce
  {-# inline hmap #-}

instance HFunctor HIdentity where
  hmap f = HIdentity . f . runHIdentity
  {-# inline hmap #-}

instance HFunctor (ReaderT r) where
  hmap f = ReaderT . fmap f . runReaderT
  {-# inline hmap #-}

instance HFunctor (StateT s) where
  hmap f = StateT . fmap f . runStateT
  {-# inline hmap #-}

instance HFunctor (WriterT w) where
  hmap f = WriterT . f . runWriterT
  {-# inline hmap #-}

instance HFunctor MaybeT where
  hmap f = MaybeT . f . runMaybeT
  {-# inline hmap #-}

instance HFunctor IdentityT where
  hmap f = IdentityT . f . runIdentityT
  {-# inline hmap #-}

instance HFunctor (ExceptT e) where
  hmap f = ExceptT . f . runExceptT
  {-# inline hmap #-}

instance HFunctor (RWST r w s) where
  hmap f = RWST . (fmap (fmap f)) . runRWST
  {-# inline hmap #-}

instance HFunctor (AccumT w) where
  hmap f = AccumT . fmap f . runAccumT
  {-# inline hmap #-}

instance HFunctor Lift where
  hmap _ (Pure a) = Pure a
  hmap f (Other t) = Other (f t)
  {-# inline hmap #-}

instance HFunctor (Product f) where
  hmap f (Pair t u) = Pair t (f u)
  {-# inline hmap #-}

instance HFunctor (NT f) where
  hmap h (NT fg) = NT $ h . fg


hlower
  :: HFunctor t
  => Functor f
  => (forall i. f i -> i)
  -> t f a
  -> t Identity a
hlower f t = hmap (Identity . f) t
{-# inline hlower #-}

-- -------------------------------------------------------------------- --
-- Type-indexed functors

-- | 'SemiHFunctor's are type-indexed "1 and a 1/2" functors from the 2-category of
-- Haskell functors to the 1-category of Haskell types. For a true functor on functors, see
-- 'Data.Functor.Higher.HFunctor'.
--
class SemiHFunctor (t :: (i -> Type) -> Type) where
  semimap :: (f ~> g) -> t f -> t g
  default semimap :: (Generic1 t, SemiHFunctor (Rep1 t)) => (f ~> g) -> t f -> t g
  semimap f = to1 . semimap f . from1
  {-# inline semimap #-}
  {-# minimal semimap #-}

instance SemiHFunctor (Applied a) where
  semimap f = Applied . f . runApplied
  {-# inline semimap #-}

instance SemiHFunctor Proxy where
  semimap _ = coerce
  {-# inline semimap #-}

instance SemiHFunctor (Const a) where
  semimap _ = coerce
  {-# inline semimap #-}

instance SemiHFunctor (SemiHConst a) where
  semimap _ = coerce
  {-# inline semimap #-}

instance SemiHFunctor V1 where
  semimap _ = \case
  {-# inline semimap #-}

instance SemiHFunctor U1 where
  semimap _ = coerce
  {-# inline semimap #-}

instance SemiHFunctor f => SemiHFunctor (Rec1 f) where
  semimap f (Rec1 a) = Rec1 (semimap f a)
  {-# inline semimap #-}

instance SemiHFunctor (K1 i c) where
  semimap _ = coerce
  {-# inline semimap #-}

instance SemiHFunctor f => SemiHFunctor (M1 i c f) where
  semimap f (M1 a) = M1 (semimap f a)
  {-# inline semimap #-}

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor (f :+: g) where
  semimap f (L1 a) = L1 (semimap f a)
  semimap f (R1 a) = R1 (semimap f a)
  {-# inline semimap #-}

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor (f :*: g) where
  semimap f (a :*: b) = semimap f a :*: semimap f b
  {-# inline semimap #-}

instance (Functor f, SemiHFunctor g) => SemiHFunctor (f :.: g) where
  semimap f (Comp1 a) = Comp1 (fmap (semimap f) a)
  {-# inline semimap #-}

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor (Sum f g) where
  semimap f (InL a) = InL (semimap f a)
  semimap f (InR a) = InR (semimap f a)
  {-# inline semimap #-}

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor (Product f g) where
  semimap f (Pair a b) = Pair (semimap f a) (semimap f b)
  {-# inline semimap #-}

-- Compose f g a = f (g a), f must be (regular) Functor
instance (Functor f, SemiHFunctor g) => SemiHFunctor (Compose f g) where
  semimap f (Compose a) = Compose (fmap (semimap f) a)
  {-# inline semimap #-}

semilower
  :: SemiHFunctor t
  => Functor f
  => (forall x. f x -> x)
  -> t f
  -> t Identity
semilower f = semimap (Identity . f)
{-# inline semilower #-}
