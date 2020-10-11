{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# language MultiParamTypeClasses #-}
module Data.Adjunction.Higher where

import Data.Function.Higher
import Data.Functor.Higher


class (HFunctor l, HFunctor r) => HAdjunction l r where
  hunit :: a ~> r (l a)
  hcounit :: l (r a) ~> a

  hleftAdjunct :: (l a ~> b) -> a ~> r b
  hleftAdjunct f a = hmap f (hunit a)

  hrightAdjunct :: (a ~> r b) -> l a ~> b
  hrightAdjunct f a = hcounit (hmap f a)
  {-# minimal hunit, hcounit #-}
