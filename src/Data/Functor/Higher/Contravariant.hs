{-# language EmptyCase #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Functor.Higher.Contravariant
( HContravariant(..)
, HDivisible(..)
, HDecidable(..)
, SemiHContravariant(..)
, SemiHDivisible(..)
, SemiHDecidable(..)
) where


import Control.Applicative (Alternative(..))

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Function.Higher
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Contravariant.Divisible
import Data.Functor.Higher.Const (SemiHConst(..), HConst(..))
import Data.Functor.Sum
import Data.Functor.Product
import Data.Proxy (Proxy(..))

import Data.Void (Void, absurd)

import GHC.Generics

-- -------------------------------------------------------------------- --
-- Higher contravariant functors

class HContravariant (t :: (i -> Type) -> j -> Type) where
  --fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f <~ g) -> (t f ~> t g)
  {-# minimal hcontramap #-}

instance HContravariant (Nop f) where
  hcontramap f (Nop g) = Nop $ g . f

instance HContravariant (HConst f) where
  hcontramap _ = coerce

class HContravariant t => HDivisible t where
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a
  hconquer :: t f a
  {-# minimal hdivide, hconquer #-}


instance Alternative f => HDivisible (Nop f) where
  hdivide f (Nop g) (Nop h) = Nop $ \t -> case f t of
    (a,b) -> g a <|> h b
  {-# inline hdivide #-}

  hconquer = Nop $ const empty
  {-# inline hconquer #-}

instance Alternative f => HDivisible (HConst f) where
  hdivide _ _ = coerce
  {-# inline hdivide #-}

  hconquer = HConst empty
  {-# noinline hconquer #-}


class HDivisible t => HDecidable t where
  -- adecide :: Decidable f => (a -> Either b c) -> t f b -> t f c -> t f a
  hchoose :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  hlose :: (forall x. f x -> Void) -> t f a
  {-# minimal hchoose, hlose #-}

instance Alternative f => HDecidable (Nop f) where
  hchoose f (Nop g) (Nop h) = Nop $ \t -> case f t of
    Left a -> g a
    Right b -> h b
  {-# inline hchoose #-}

  hlose v = Nop $ absurd . v
  {-# inline hlose #-}

instance (Decidable f, Alternative f) => HDecidable (HConst f) where
  hchoose _ _ = coerce
  hlose _ = HConst empty

-- -------------------------------------------------------------------- --
-- Type-indexed contravariant functors

class SemiHContravariant (t :: (i -> Type) -> Type) where
  semicontramap :: (f <~ g) -> t f -> t g
  default semicontramap
    :: ( Generic1 t
       , SemiHContravariant (Rep1 t)
       )
    => (f <~ g)
    -> t f
    -> t g
  semicontramap f = to1 . semicontramap f . from1


instance SemiHContravariant Proxy where
  semicontramap _ = coerce
  {-# inline semicontramap #-}

instance SemiHContravariant (Const a) where
  semicontramap _ = coerce
  {-# inline semicontramap #-}

instance SemiHContravariant (SemiHConst a) where
  semicontramap _ = coerce
  {-# inline semicontramap #-}

instance SemiHContravariant V1 where
  semicontramap _ = \case
  {-# inline semicontramap #-}

instance SemiHContravariant U1 where
  semicontramap _ = coerce
  {-# inline semicontramap #-}

instance SemiHContravariant f => SemiHContravariant (Rec1 f) where
  semicontramap f (Rec1 a) = Rec1 (semicontramap f a)
  {-# inline semicontramap #-}

instance SemiHContravariant (K1 i c) where
  semicontramap _ = coerce
  {-# inline semicontramap #-}

instance SemiHContravariant f => SemiHContravariant (M1 i c f) where
  semicontramap f (M1 a) = M1 (semicontramap f a)
  {-# inline semicontramap #-}

instance (SemiHContravariant f, SemiHContravariant g) => SemiHContravariant (f :+: g) where
  semicontramap f (L1 a) = L1 (semicontramap f a)
  semicontramap f (R1 a) = R1 (semicontramap f a)
  {-# inline semicontramap #-}

instance (SemiHContravariant f, SemiHContravariant g) => SemiHContravariant (f :*: g) where
  semicontramap f (a :*: b) = semicontramap f a :*: semicontramap f b
  {-# inline semicontramap #-}

instance (Functor f, SemiHContravariant g) => SemiHContravariant (f :.: g) where
  semicontramap f (Comp1 a) = Comp1 (fmap (semicontramap f) a)
  {-# inline semicontramap #-}

instance (SemiHContravariant f, SemiHContravariant g) => SemiHContravariant (Sum f g) where
  semicontramap f (InL a) = InL (semicontramap f a)
  semicontramap f (InR a) = InR (semicontramap f a)
  {-# inline semicontramap #-}

instance (SemiHContravariant f, SemiHContravariant g) => SemiHContravariant (Product f g) where
  semicontramap f (Pair a b) = Pair (semicontramap f a) (semicontramap f b)
  {-# inline semicontramap #-}

instance (Functor f, SemiHContravariant g) => SemiHContravariant (Compose f g) where
  semicontramap f (Compose a) = Compose (fmap (semicontramap f) a)
  {-# inline semicontramap #-}


class SemiHContravariant t => SemiHDivisible t where
  semidivide :: Divisible f => (forall a b c. f a -> (g b, h c)) -> t f -> t g -> t h
  semilose :: (a -> Void) -> t f

instance SemiHDivisible Proxy where
  semidivide _ _ _ = Proxy
  semilose _ = Proxy
  {-# inline semidivide #-}

instance Monoid a => SemiHDivisible (Const a) where
  semidivide _ (Const a) (Const b) = Const (a <> b)
  semilose _ = Const mempty
  {-# inline semidivide #-}

instance Monoid a => SemiHDivisible (SemiHConst a) where
  semidivide _ (SemiHConst a) (SemiHConst b) = SemiHConst $ a <> b
  semilose _ = SemiHConst mempty
  {-# inline semidivide #-}

class SemiHDivisible t => SemiHDecidable t where
