{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.HTraversable.Semi where


import Data.HFunctor.Semi

class SemiHFunctor t => SemiHTraversable t where
  atraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f -> h (t g)
  {-# minimal atraverse #-}
