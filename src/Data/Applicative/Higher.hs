{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Applicative.Higher
( HApplicative(..)
, SemiHApplicative(..)
) where


import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadTrans(..), lift)

import Data.Function.Higher (type (~>))
import Data.Functor.Higher (SemiHFunctor, HFunctor(..))
import Data.Functor.Higher.Identity (HIdentity(..))
import Data.Functor.Higher.Compose (HCompose(..))
import Data.Functor.Higher.Const (HConst(..))
import Data.Functor.Product (Product(..))


-- -------------------------------------------------------------------- --
-- Higher Applicatives

class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  default hpure :: (Monad f, MonadTrans t) => f ~> t f
  hpure = lift
  {-# inline hpure #-}

  hlift2 :: Product (t f) (t g) ~> t (Product f g)
  {-# minimal hpure, hlift2 #-}

instance (HApplicative t, HApplicative u) => HApplicative (HCompose t u) where
  hpure = HCompose . hpure . hpure
  {-# inline hpure #-}

  hlift2 (Pair (HCompose tuf) (HCompose tug))
    = HCompose $ hmap hlift2 (hlift2 (Pair tuf tug))
  {-# inline hlift2 #-}

instance Alternative f => HApplicative (HConst f) where
  hpure _ = HConst empty
  {-# inline hpure #-}

  hlift2 (Pair (HConst a) (HConst b)) = HConst (a <|> b)
  {-# inline hlift2 #-}

instance HApplicative HIdentity where
  hpure = HIdentity
  {-# inline hpure #-}

  hlift2 (Pair (HIdentity f) (HIdentity g)) = HIdentity (Pair f g)
  {-# inline hlift2 #-}

-- -------------------------------------------------------------------- --
-- Type-indexed Applicatives
--

class SemiHFunctor t => SemiHApplicative t where
  semipure :: Applicative f => f a -> t f
  semilift2 :: Applicative f => (a -> b -> c) -> t f -> t g -> t h
  {-# minimal semilift2, semipure #-}
