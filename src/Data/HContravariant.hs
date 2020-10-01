{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.HContravariant where

import Data.Functor.Contravariant (Contravariant)
import Data.NaturalTransformation
import Data.Functor.Contravariant.Divisible (Decidable, Divisible)
import Data.Void (Void)


class HContravariant t where
  fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f ~> g) -> (t g ~> t f)
  {-# minimal hcontramap, fcontramap #-}

class HContravariant t => HDivisible t where
  adivide :: Divisible f => (a -> (b,c)) -> t f b -> t f c -> t f a
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a

  hconquer :: t f a
  {-# minimal hdivide, adivide, hconquer #-}

class HDivisible t => HDecideable t where
  adecide :: Decidable f => (a -> Either b c) -> t f b -> t f c -> t f a
  hdecide :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  hlose :: (f a -> f Void) -> t f a
  {-# minimal hdecide, adecide, hlose #-}
