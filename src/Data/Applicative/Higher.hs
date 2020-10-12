{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Applicative.Higher where


import Data.Function.Higher (type (~>))
import Data.Functor.Higher (SemiHFunctor, HFunctor)
import Data.Functor.Higher.Tensor (HTensor(..))


class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  hlift2 :: HTensor (t f) (t g) ~> t (HTensor f g)
  {-# minimal hpure, hlift2 #-}

class SemiHFunctor t => SemiHApplicative t where
  semipure :: Applicative f => f a -> t f
  semilift2 :: Applicative f => (a -> b -> c) -> t f -> t g -> t h
  {-# minimal semilift2, semipure #-}
