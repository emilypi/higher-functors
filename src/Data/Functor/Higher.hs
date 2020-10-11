{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Functor.Higher
( -- * Type-indexed functors
  SemiHFunctor(..)
, Applied(..)
, semimapIdentity
, HFunctor(..)
) where


import Data.Function.Higher
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Proxy

import GHC.Generics


-- -------------------------------------------------------------------- --
-- Higher functors

class HFunctor (t :: (i -> Type) -> (j -> Type) -> Type) where
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap #-}

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
  {-# minimal semimap #-}

-- | 'Applied' represents a type-constructor applied to a haskell type.
-- The container can be changed by 'semimap'ping constructors.
--
newtype Applied a f = Applied { runApplied :: f a }

instance SemiHFunctor (Applied a) where
  semimap f = Applied . f . runApplied

instance SemiHFunctor Proxy where
  semimap _ Proxy = Proxy

instance SemiHFunctor (Const a) where
  semimap _ = Const . getConst

semimapIdentity
  :: SemiHFunctor t
  => Functor f
  => (forall x. f x -> x)
  -> t f
  -> t Identity
semimapIdentity f = semimap (Identity . f)
