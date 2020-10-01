{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.HTraversable where


import Data.HApplicative
import Data.HFunctor
import Data.NaturalTransformation


class HFunctor t => HTraversable t where
  atraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f a -> h (t g b)

  htraverse :: HApplicative u => (forall i. f i -> u g i) -> t f ~> u (t g)
  htraverse f = hsequence . hmap f

  hsequence :: HApplicative u => t (u f) ~> u (t f)
  hsequence = htraverse id
  {-# minimal atraverse, (htraverse | hsequence) #-}
