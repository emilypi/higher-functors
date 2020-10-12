{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Comonad.Higher where


import Control.Comonad (Comonad)

import Data.Function.Higher (type (~>))
import Data.Functor.Higher (SemiHFunctor, HFunctor(..))




class SemiHFunctor t => SemiHComonad t where
  semicounit :: t w -> w a
  semiextract :: Comonad w => t w -> (t w -> x a) -> t x
  {-# minimal semicounit, semiextract #-}


class HFunctor t => HComonad t where
  hcopure :: t g ~> g

  hcobind :: t g a -> (t g ~> f) -> t f a
  hcobind t f = hmap f (hcojoin t)

  hcojoin :: t f ~> t (t f)
  hcojoin t = hcobind t id
  {-# minimal hcopure, (hcobind | hcojoin) #-}
