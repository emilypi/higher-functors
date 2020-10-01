{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.HFunctor.Semi
( SemiHFunctor(..)
, Applied(..)
, mapIdentityT
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.NaturalTransformation
import Data.Proxy

import GHC.Generics


-- | 'SemiHFunctor's are "1 and a 1/2" functors from the 2-category of Haskell functors
-- to the 1-category of Haskell types. For a true Functor on Functors, see
-- 'Data.HFunctor.HFunctor'.
--
class SemiHFunctor t where
  fmapT :: (f ~> g) -> t f -> t g
  default fmapT :: (Generic1 t, SemiHFunctor (Rep1 t)) => (f ~> g) -> t f -> t g
  fmapT f = to1 . fmapT f . from1
  {-# minimal fmapT #-}

newtype Applied a f = Applied { runApplied :: f a }

instance SemiHFunctor (Applied a) where
  fmapT f = Applied . f . runApplied

instance SemiHFunctor Proxy where
  fmapT _ Proxy = Proxy

instance SemiHFunctor (Const a) where
  fmapT _ = Const . getConst

mapIdentityT :: SemiHFunctor t => (forall x. f x -> x) -> t f -> t Identity
mapIdentityT f = fmapT (Identity . f)
