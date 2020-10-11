{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.Traversable.Higher where


import Data.Applicative.Higher
import Data.Function.Higher (type (~>))
import Data.Functor.Higher



class HFunctor t => HTraversable t where
  htraverse :: HApplicative u => (forall i. f i -> u g i) -> t f ~> u (t g)
  htraverse f = hsequence . hmap f

  hsequence :: HApplicative u => t (u f) ~> u (t f)
  hsequence = htraverse id
  {-# minimal htraverse | hsequence #-}

class SemiHFunctor t => SemiHTraversable t where
  atraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f -> h (t g)
  {-# minimal atraverse #-}
