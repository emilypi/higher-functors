{-# LANGUAGE QuantifiedConstraints #-}
{-# language TupleSections #-}
{-# language EmptyCase #-}
{-# language LambdaCase #-}
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


import Control.Applicative (Alternative(..), liftA2)
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Coerce (coerce)
import Data.Function.Higher (type (~>))
import Data.Functor.Higher (SemiHFunctor, HFunctor(..))
import Data.Functor.Higher.Applied
import Data.Functor.Higher.Identity (HIdentity(..))
import Data.Functor.Higher.Compose (HCompose(..))
import Data.Functor.Higher.Const (HConst(..), SemiHConst(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.Proxy

import GHC.Generics

-- -------------------------------------------------------------------- --
-- Higher Applicatives

class HFunctor t => HApplicative t where
  hpure :: Functor f => f ~> t f

  hlift2 :: Product (t f) (t g) ~> t (Product f g)
  {-# minimal hpure, hlift2 #-}

instance (HApplicative t, HApplicative u, forall f. Functor (u f))
  => HApplicative (HCompose t u) where
  hpure = HCompose . hpure . hpure
  {-# inline hpure #-}

  hlift2 (Pair (HCompose a) (HCompose b))
    = HCompose $ hmap hlift2 (hlift2 (Pair a b))
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

instance HApplicative IdentityT where
  hpure = IdentityT
  {-# inline hpure #-}

  hlift2 (Pair (IdentityT a) (IdentityT b)) = IdentityT (Pair a b)
  {-# inline hlift2 #-}

instance HApplicative (ReaderT r) where
  hpure = ReaderT . const
  {-# inline hpure #-}

  hlift2 (Pair (ReaderT a) (ReaderT b)) =
    ReaderT $ \r -> Pair (a r) (b r)
  {-# inline hlift2 #-}

instance HApplicative (StateT s) where
  hpure a = StateT $ \s -> (,s) <$> a
  {-# inline hpure #-}

  hlift2 (Pair (StateT a) (StateT b)) = StateT $ \s -> Pair (a s) (b s)
  {-# inline hlift2 #-}

instance Monoid w => HApplicative (WriterT w) where
  hpure a = WriterT $ (,mempty) <$> a
  {-# inline hpure #-}

  hlift2 (Pair (WriterT a) (WriterT b)) = WriterT $ Pair a b
  {-# inline hlift2 #-}

instance HApplicative MaybeT where
  hpure a = MaybeT $ Just <$> a
  {-# inline hpure #-}

  hlift2 (Pair (MaybeT a) (MaybeT b)) = MaybeT $ Pair a b
  {-# inline hlift2 #-}

instance HApplicative (ExceptT e) where
  hpure a = ExceptT $ Right <$> a
  {-# inline hpure #-}

  hlift2 (Pair (ExceptT a) (ExceptT b)) = ExceptT $ Pair a b
  {-# inline hlift2 #-}

instance HApplicative (AccumT w) where
  hpure a = AccumT $ \w -> (,w) <$> a
  {-# inline hpure #-}

  hlift2 (Pair (AccumT a) (AccumT b)) = AccumT $ \w -> Pair (a w) (b w)
  {-# inline hlift2 #-}

instance Monoid w => HApplicative (RWST r w s) where
  hpure a = RWST $ \_ s -> (,s,mempty) <$> a
  {-# inline hpure #-}

  hlift2 (Pair (RWST a) (RWST b)) = RWST $ \r s -> Pair (a r s) (b r s)
  {-# inline hlift2 #-}

-- -------------------------------------------------------------------- --
-- Type-indexed Applicatives
--

class SemiHFunctor t => SemiHApplicative t where
  semilift2
    :: Applicative f
    => (forall a. f a -> g a -> h a)
    -> t f
    -> t g
    -> t h
  {-# minimal semilift2 #-}

instance SemiHApplicative (Applied a) where
  semilift2 f (Applied t) (Applied u) = Applied (f t u)
  {-# inline semilift2 #-}

instance SemiHApplicative Proxy where
  semilift2 _ _ _ = Proxy

instance Semigroup a => SemiHApplicative (Const a) where
  semilift2 _ (Const a) (Const b) = Const (a <> b)

instance Semigroup a => SemiHApplicative (SemiHConst a) where
  semilift2 _ (SemiHConst a) (SemiHConst b) = SemiHConst (a <> b)

instance SemiHApplicative V1 where
  semilift2 _ _ = \case

instance SemiHApplicative U1 where
  semilift2 _ _ = coerce

instance SemiHApplicative (K1 i c) where
  semilift2 _ _ = coerce

instance SemiHApplicative f => SemiHApplicative (Rec1 f) where
  semilift2 f (Rec1 t) (Rec1 u) = Rec1 $ semilift2 f t u

instance SemiHApplicative f => SemiHApplicative (M1 i c f) where
  semilift2 f (M1 a) (M1 b) = M1 (semilift2 f a b)

instance (Applicative f, SemiHApplicative g) => SemiHApplicative (Compose f g) where
  semilift2 f (Compose a) (Compose b) = Compose (liftA2 (semilift2 f) a b)
