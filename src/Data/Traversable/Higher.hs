{-# language EmptyCase #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}
{-# language RankNTypes #-}
module Data.Traversable.Higher
( HTraversable(..)
, SemiHTraversable(..)
) where


import Control.Applicative (liftA2)

import Data.Applicative.Higher
import Data.Coerce (coerce)
import Data.Function.Higher (type (~>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Higher
import Data.Functor.Higher.Applied (Applied(..))
import Data.Functor.Higher.Compose
import Data.Functor.Higher.Const
import Data.Functor.Higher.Identity
import Data.Functor.Product
import Data.Functor.Sum (Sum(..))
import Data.Proxy

import GHC.Generics

-- -------------------------------------------------------------------- --
-- Higher traversable functors

class HFunctor t => HTraversable t where
  htraverse :: HApplicative u => (forall i. f i -> u g i) -> t f ~> u (t g)
  htraverse f = hsequence . hmap f
  {-# inline htraverse #-}

  hsequence :: HApplicative u => t (u f) ~> u (t f)
  hsequence = htraverse id
  {-# inline hsequence #-}
  {-# minimal htraverse | hsequence #-}

instance HTraversable HIdentity where
  htraverse f (HIdentity a) = hmap HIdentity (f a)
  {-# inline htraverse #-}

instance Functor f => HTraversable (HConst f) where
  htraverse _ = hpure . coerce
  {-# inline htraverse #-}

instance (HTraversable t, HTraversable u) => HTraversable (HCompose t u) where
  htraverse f (HCompose t) = hmap HCompose $ htraverse (htraverse f) t
  {-# inline htraverse #-}

instance Functor f => HTraversable (Product f) where
  htraverse f (Pair a b) = hlift2 $ Pair (hpure a) (f b)
  {-# inline htraverse #-}

-- -------------------------------------------------------------------- --
-- Type-indexed traversable functors

class SemiHFunctor t => SemiHTraversable t where
  semitraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f -> h (t g)
  {-# minimal semitraverse #-}

instance SemiHTraversable (Applied a) where
  semitraverse f (Applied a) = Applied <$> f a
  {-# inline semitraverse #-}

instance SemiHTraversable Proxy where
  semitraverse _ = pure . coerce
  {-# inline semitraverse #-}

instance SemiHTraversable (Const a) where
  semitraverse _ = pure . coerce
  {-# inline semitraverse #-}

instance SemiHTraversable (SemiHConst a) where
  semitraverse _ = pure . coerce
  {-# inline semitraverse #-}

instance SemiHTraversable V1 where
  semitraverse _ = pure . \case
  {-# inline semitraverse #-}

instance SemiHTraversable U1 where
  semitraverse _ = pure . coerce
  {-# inline semitraverse #-}

instance SemiHTraversable f => SemiHTraversable (Rec1 f) where
  semitraverse f (Rec1 a) = Rec1 <$> semitraverse f a
  {-# inline semitraverse #-}

instance SemiHTraversable (K1 i c) where
  semitraverse _ = pure . coerce
  {-# inline semitraverse #-}

instance SemiHTraversable f => SemiHTraversable (M1 i c f) where
  semitraverse f (M1 a) = M1 <$> semitraverse f a
  {-# inline semitraverse #-}

instance (SemiHTraversable f, SemiHTraversable g) => SemiHTraversable (f :+: g) where
  semitraverse f (L1 a) = L1 <$> semitraverse f a
  semitraverse f (R1 a) = R1 <$> semitraverse f a
  {-# inline semitraverse #-}

instance (SemiHTraversable f, SemiHTraversable g) => SemiHTraversable (f :*: g) where
  semitraverse f (a :*: b) = liftA2 (:*:) (semitraverse f a) (semitraverse f b)
  {-# inline semitraverse #-}

-- (f :.: g) a = f (g a), f must be (regular) Functor
instance (Traversable f, SemiHTraversable g) => SemiHTraversable (f :.: g) where
  semitraverse f (Comp1 a) = Comp1 <$> traverse (semitraverse f) a
  {-# inline semitraverse #-}

instance (SemiHTraversable f, SemiHTraversable g) => SemiHTraversable (Sum f g) where
  semitraverse f (InL a) = InL <$> semitraverse f a
  semitraverse f (InR a) = InR <$> semitraverse f a
  {-# inline semitraverse #-}

instance (SemiHTraversable f, SemiHTraversable g) => SemiHTraversable (Product f g) where
  semitraverse f (Pair a b) = liftA2 Pair (semitraverse f a) (semitraverse f b)
  {-# inline semitraverse #-}

-- Compose f g a = f (g a), f must be (regular) Functor
instance (Traversable f, SemiHTraversable g) => SemiHTraversable (Compose f g) where
  semitraverse f (Compose a) = Compose <$> traverse (semitraverse f) a
  {-# inline semitraverse #-}
