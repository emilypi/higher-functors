{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.HDistributive where


import Data.HFunctor
import Data.NaturalTransformation


class HFunctor t => HDistributive t where
  acotraverse :: Functor h => (forall i. h (g i) -> f i) -> h (t g b) -> t f a

  hcotraverse :: HFunctor u => (forall i. u g i -> f i) -> u (t g) ~> t f
  hcotraverse f = hmap f . hdistribute

  hdistribute :: HFunctor u => u (t f) ~> t (u f)
  hdistribute = hcotraverse id
  {-# minimal acotraverse, (hdistribute | hdistribute) #-}
