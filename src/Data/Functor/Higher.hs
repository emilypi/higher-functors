{-# language EmptyCase #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Functor.Higher
( -- * Higher functors
  HFunctor(..)
  -- * Type-indexed functors
, SemiHFunctor(..)
, Applied(..)
, semimapIdentity
) where


import Control.Applicative.Lift
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Function.Higher
import Data.Functor.Higher.Applied
import Data.Functor.Higher.Identity
import Data.Functor.Higher.Compose
import Data.Functor.Higher.Const
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Kind (Type)
import Data.Proxy

import GHC.Generics
import Data.Coerce (coerce)


-- -------------------------------------------------------------------- --
-- Higher functors

class HFunctor (t :: (i -> Type) -> (j -> Type)) where
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap #-}

instance (HFunctor t, HFunctor u) => HFunctor (HCompose t u) where
  hmap f = HCompose . hmap (hmap f) . getHCompose
  {-# inline hmap #-}

instance HFunctor (HConst a) where
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

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor ((:+:) f g) where
  semimap f (L1 a) = L1 (semimap f a)
  semimap f (R1 a) = R1 (semimap f a)
  {-# inline semimap #-}

instance (SemiHFunctor f, SemiHFunctor g) => SemiHFunctor ((:*:) f g) where
  semimap f (a :*: b) = semimap f a :*: semimap f b
  {-# inline semimap #-}

-- (f :.: g) a = f (g a), f must be (regular) Functor
instance (Functor f, SemiHFunctor g) => SemiHFunctor ((:.:) f g) where
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


semimapIdentity
  :: SemiHFunctor t
  => Functor f
  => (forall x. f x -> x)
  -> t f
  -> t Identity
semimapIdentity f = semimap (Identity . f)
{-# inline semimapIdentity #-}
