{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Contravariant
( HContravariant(..)
, HDivisible(..)
, HDecidable(..)
) where


import Control.Applicative (Alternative(..))
import Data.Kind (Type)
import Data.Function.Higher
import Data.Void (Void, absurd)


class HContravariant (t :: (i -> Type) -> j -> Type) where
  --fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f <~ g) -> (t f ~> t g)
  {-# minimal hcontramap #-}

instance HContravariant (Nop f) where
  hcontramap f (Nop g) = Nop $ g . f

class HContravariant t => HDivisible t where
  -- adivide :: Divisible f => (a -> (b,c)) -> t f b -> t f c -> t f a
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a

  hconquer :: t f a
  {-# minimal hdivide, hconquer #-}

instance Alternative f => HDivisible (Nop f) where
  hdivide f (Nop g) (Nop h) = Nop $ \t -> case f t of
    (a,b) -> g a <|> h b
  {-# inline hdivide #-}

  hconquer = Nop $ const empty
  {-# inline hconquer #-}

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
