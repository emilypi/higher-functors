module Data.HRepresentable where



class HFunctor t => HRepresentable t where
  type HRep t :: * -> *

  htabulate :: HRep t ~> t f
  htabulate = case hrepresent of NIso _ g -> g

  hindex :: t f ~> HRep t
  hindex = case hrepresent of NIso f _ -> f

  hrepresent :: t f <~> HRep t
  hrepresent = NIso hindex htabulate
  {-# minimal (hindex, htabulate) | hrepresent  #-}
