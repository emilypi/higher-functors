{-# LANGUAGE EmptyCase #-}
{-# language RoleAnnotations #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Monad.Higher
( HMonad(..)
, (>>>=)
, (=<<<)
) where


import Control.Applicative (Alternative(..))
import Control.Monad (join)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))

import Data.Function.Higher
import Data.Functor.Higher (SemiHFunctor, HFunctor(..))
import Data.Functor.Higher.Const
import Data.Functor.Higher.Identity
import Data.Functor.Higher.Applied
import Data.Functor ((<&>))
import Data.Proxy
import Data.Functor.Const
import Data.Coerce
import GHC.Generics


infixl 1 >>>=
infixr 1 =<<<

-- -------------------------------------------------------------------- --
-- Higher Monads

class HFunctor t => HMonad t where
  hreturn :: Monad m => m ~> t m
  default hreturn :: (MonadTrans t, Monad m) => m ~> t m
  hreturn = lift
  {-# inline hreturn #-}

  hbind :: (Monad f, Monad g, Monad (t g)) => t f a -> (f ~> t g) -> t g a
  hbind t f = hjoin (hmap f t)
  {-# inlinable hbind #-}

  hjoin :: (Monad f, Monad (t f)) => t (t f) ~> t f
  hjoin t = hbind t id
  {-# inlinable hjoin #-}

  hbound :: (Monad f, Monad (t f)) => t f a -> (a -> f b) -> t f b
  hbound t k = t >>= hreturn . k
  {-# inlinable hbound #-}
  {-# minimal hreturn, (hjoin | hbind) #-}

instance HMonad HIdentity where
  hreturn = HIdentity
  {-# inline hreturn #-}

  hjoin = runHIdentity
  {-# inlinable hjoin #-}

instance Alternative f => HMonad (HConst f) where
  hreturn _ = HConst empty
  {-# inline hreturn #-}

  hjoin (HConst a) = HConst a
  {-# inlinable hjoin #-}

instance HMonad (ReaderT r) where
  hreturn = lift
  {-# inline hreturn #-}

  hjoin a = ReaderT $ \r -> runReaderT (runReaderT a r) r
  {-# inlinable hjoin #-}

instance Monoid w => HMonad (WriterT w) where
  hreturn = lift
  {-# inline hreturn #-}

  hbind m k = WriterT $
    runWriterT (k (runWriterT m)) <&> \ ~((a,w), w') -> (a, w <> w')
  {-# inlinable hbind #-}

instance HMonad MaybeT where
  hreturn = lift
  {-# inline hreturn #-}

  hjoin (MaybeT (MaybeT f)) = MaybeT $ fmap join f
  {-# inlinable hjoin #-}

instance HMonad IdentityT where
  hreturn = lift
  {-# inline hreturn #-}

  hjoin = runIdentityT
  {-# inlinable hjoin #-}

instance HMonad (ExceptT e) where
  hreturn = lift
  {-# inline hreturn #-}

  hjoin (ExceptT (ExceptT a)) = ExceptT $ go <$> a
    where
      go (Left e) = Left e
      go (Right (Left e)) = Left e
      go (Right (Right t)) = Right t
  {-# inlinable hjoin #-}


-- -------------------------------------------------------------------- --
-- Combinators

(>>>=) :: (HMonad t, Monad f, Monad g, Monad (t g)) => t f a -> (f ~> t g) -> t g a
t >>>= k = hbind t k
{-# inlinable (>>>=) #-}

(=<<<) :: (HMonad t, Monad f, Monad g, Monad (t g)) => (f ~> t g) -> t f a -> t g a
k =<<< t = hbind t k
{-# inlinable (=<<<) #-}

-- -------------------------------------------------------------------- --
-- Type-indexed monads

class SemiHFunctor t => SemiHBind t where
  semibind :: (Monad m, Monad n) => t m -> (forall a. m a -> t n) -> t n
  {-# minimal semibind #-}

instance SemiHBind (Applied a) where
  semibind (Applied a) k = k a

instance SemiHBind Proxy where
  semibind _ _ = Proxy

instance SemiHBind (Const a) where
  semibind a _ = coerce a

instance SemiHBind (SemiHConst a) where
  semibind a _ = coerce a

instance SemiHBind V1 where
  semibind a _ = coerce a

instance SemiHBind U1 where
  semibind a _ = coerce a

instance SemiHBind f => SemiHBind (Rec1 f) where
  semibind (Rec1 f) k = Rec1 $ semibind f (coerce . k)

instance SemiHBind f => SemiHBind (M1 i c f) where
  semibind (M1 a) k = M1 $ semibind a (coerce . k)
