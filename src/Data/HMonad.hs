{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Data.HMonad where


import Data.HFunctor
import Data.HApplicative
import Data.NaturalTransformation

class HApplicative t => HMonad t where
  mbind :: Monad m => t m a -> (a -> m b) -> t m b

  hbind :: t f a -> (f ~> t g) -> t g a
  hbind t f = hjoin (hmap f t)

  hjoin :: t (t f) ~> t f
  hjoin t = hbind t id
  {-# minimal mbind, (hjoin | hbind) #-}
