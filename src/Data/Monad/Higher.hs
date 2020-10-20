{-# language RoleAnnotations #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Monad.Higher where


import Control.Applicative.Lift (Lift(..))
import Control.Monad.Trans.Accum (runAccumT, AccumT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))

import Data.Applicative.Higher
import Data.Function.Higher
import Data.Functor.Higher (HFunctor(hmap))
import Control.Monad (join)


class HFunctor t => HMonad t where
  hbind
    :: Functor f
    => Functor g
    => Functor (t g)
    => t f a
    -> (f ~> t g)
    -> t g a
  hbind t f = hjoin (hmap f t)
  {-# inline hbind #-}

  hjoin :: (Functor f, Functor (t f)) => t (t f) ~> t f
  hjoin t = hbind t id
  {-# inline hjoin #-}
  {-# minimal (hjoin | hbind) #-}

instance HMonad (ReaderT r) where
  hjoin f = ReaderT $ \r -> runReaderT (runReaderT f r) r

instance HMonad (StateT s) where
  hbind st k = StateT $ \s ->
    runStateT (k $ fmap fst $ runStateT st s) s

instance HMonad (WriterT w) where
  hbind w k = k $ fmap fst $ runWriterT w

instance HMonad MaybeT where
  hjoin (MaybeT (MaybeT f)) = MaybeT $ fmap join f

instance HMonad IdentityT where
  hjoin = runIdentityT
