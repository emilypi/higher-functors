{-# language TypeOperators #-}
{-# language RankNTypes #-}
module Data.Traversable.Higher where


import Data.Applicative.Higher
import Data.Coerce (coerce)
import Data.Function.Higher (type (~>))
import Data.Functor.Higher
import Data.Functor.Higher.Compose
import Data.Functor.Higher.Const
import Data.Functor.Higher.Identity
import Data.Functor.Product

-- -------------------------------------------------------------------- --
-- Higher traversable functors

class HFunctor t => HTraversable t where
  htraverse :: HApplicative u => (forall i. f i -> u g i) -> t f ~> u (t g)
  htraverse f = hsequence . hmap f
  {-# inline htraverse #-}

  hsequence :: HApplicative u => t (u f) ~> u (t f)
  hsequence = htraverse id
  {-# inline hsequence #-}
  {-# minimal htraverse | hsequence #-}

instance HTraversable HIdentity where
  htraverse f (HIdentity a) = hmap HIdentity (f a)
  {-# inline htraverse #-}

instance Functor f => HTraversable (HConst f) where
  htraverse _ = hpure . coerce
  {-# inline htraverse #-}

instance (HTraversable t, HTraversable u) => HTraversable (HCompose t u) where
  htraverse f (HCompose t) = hmap HCompose $ htraverse (htraverse f) t
  {-# inline htraverse #-}

instance Functor f => HTraversable (Product f) where
  htraverse f (Pair a b) = hlift2 $ Pair (hpure a) (f b)

-- -------------------------------------------------------------------- --
-- Type-indexed traversable functors

class SemiHFunctor t => SemiHTraversable t where
  atraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f -> h (t g)
  {-# minimal atraverse #-}
