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
  htabulate = case hrepresent of NIso _ g -> g

  hindex :: t f ~> HRep t
  hindex = case hrepresent of NIso f _ -> f

  hrepresent :: t f <~> HRep t
  hrepresent = NIso hindex htabulate
  {-# minimal (hindex, htabulate) | hrepresent  #-}
