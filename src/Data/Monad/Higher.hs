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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer (WriterT(..))

import Data.Function.Higher
import Data.Functor.Higher (HFunctor(..))
import Data.Functor.Higher.Const
import Data.Functor.Higher.Identity
import Control.Monad (join)


class HFunctor t => HMonad t where
  -- move this out to its own class (HFunctor => HPointed)?
  --
  hunit :: Monad m => m ~> t m
  default hunit :: (MonadTrans t, Monad m) => m ~> t m
  hunit = lift
  {-# inlinable hunit #-}

  hbind :: (Monad f, Monad g, Monad (t g)) => t f a -> (f ~> t g) -> t g a
  hbind t f = hjoin (hmap f t)
  {-# inlinable hbind #-}

  hjoin :: (Monad f, Monad (t f)) => t (t f) ~> t f
  hjoin t = hbind t id
  {-# inlinable hjoin #-}
  {-# minimal (hjoin | hbind) #-}

instance HMonad HIdentity where
  hunit = HIdentity
  {-# inlinable hunit #-}

  hjoin = runHIdentity
  {-# inlinable hjoin #-}

instance Alternative f => HMonad (HConst f) where
  hunit _ = HConst empty
  {-# inlinable hunit #-}

  hjoin (HConst a) = HConst a

instance HMonad (ReaderT r) where
  hjoin a = ReaderT $ \r -> runReaderT (runReaderT a r) r

instance Monoid w => HMonad (WriterT w) where
  hbind m k = WriterT $ do
    ~((a,w),w') <- runWriterT $ k $ runWriterT m
    return (a, w <> w')

instance HMonad MaybeT where
  hjoin (MaybeT (MaybeT f)) = MaybeT $ fmap join f

instance HMonad IdentityT where
  hjoin = runIdentityT

instance HMonad (ExceptT e) where
  hjoin (ExceptT (ExceptT a)) = ExceptT $ go <$> a
    where
      go (Left e) = Left e
      go (Right (Left e)) = Left e
      go (Right (Right t)) = Right t


(>>>=) :: (HMonad t, Monad f, Monad g, Monad (t g)) => t f a -> (f ~> t g) -> t g a
t >>>= k = hbind t k

(=<<<) :: (HMonad t, Monad f, Monad g, Monad (t g)) => (f ~> t g) -> t f a -> t g a
k =<<< t = hbind t k
