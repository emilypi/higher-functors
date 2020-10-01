{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.HFunctor.Trans
( FunctorT(..)
, Applied(..)
, mapIdentityT
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.NaturalTransformation
import Data.Proxy

import GHC.Generics


class FunctorT t where
  fmapT :: (f ~> g) -> t f -> t g
  default fmapT :: (Generic1 t, FunctorT (Rep1 t)) => (f ~> g) -> t f -> t g
  fmapT f = to1 . fmapT f . from1
  {-# minimal fmapT #-}

newtype Applied a f = Applied { runApplied :: f a }

instance FunctorT (Applied a) where
  fmapT f = Applied . f . runApplied

instance FunctorT Proxy where
  fmapT _ Proxy = Proxy

instance FunctorT (Const a) where
  fmapT _ = Const . getConst

mapIdentityT :: FunctorT t => (forall x. f x -> x) -> t f -> t Identity
mapIdentityT f = fmapT (Identity . f)
