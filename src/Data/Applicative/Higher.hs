{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Applicative.Higher where


import Data.Function.Higher
import Data.Functor.Higher


class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  happly :: (forall x. f x -> g x -> u f g x) -> t f a -> t g b -> t (u f g) c
  {-# minimal hpure, happly #-}

class SemiHFunctor t => SemiHApplicative t where
  alift2 :: Applicative f => (a -> b -> c) -> t f -> t g -> t h
  {-# minimal alift2 #-}
