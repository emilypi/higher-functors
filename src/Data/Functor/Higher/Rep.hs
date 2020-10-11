{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Rep where


import Data.Function.Higher
import Data.Functor.Higher
import Data.Kind


class HFunctor t => HRepresentable t where
  type HRep t :: (k -> Type) -> Type

  htabulate :: HRep t ~> t f
  hindex :: t f ~> HRep t
  {-# minimal (hindex, htabulate) #-}
