{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Contravariant where


import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible (Decidable, Divisible)
import Data.Kind (Type)
import Data.Function.Higher
import Data.Void (Void)



class HContravariant (t :: (i -> Type) -> j -> Type) where
  --fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f ~> g) -> (t g ~> t f)
  {-# minimal hcontramap #-}

class HContravariant t => HDivisible t where
  -- adivide :: Divisible f => (a -> (b,c)) -> t f b -> t f c -> t f a
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a

  hconquer :: t f a
  {-# minimal hdivide, hconquer #-}

class HDivisible t => HDecideable t where
  -- adecide :: Decidable f => (a -> Either b c) -> t f b -> t f c -> t f a
  hdecide :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  hlose :: (f a -> f Void) -> t f a
  {-# minimal hdecide, hlose #-}
