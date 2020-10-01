{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.HApplicative where


import Data.HFunctor
import Data.NaturalTransformation

class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  happly :: (forall x. f x -> g x -> u f g x) -> t f a -> t g b -> t (u f g) c
  alift2 :: Applicative f => (a -> b -> c) -> t f a -> t f b -> t f c
  {-# minimal alift2, hpure, happly #-}
