{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Monad.Higher.Free
  ( MonadHFree (..),
    HFree (..),
  )
where

import Data.Function.Higher
import Data.Kind
import Data.Monad.Higher

class HMonad m => MonadHFree t m | m -> t where
  hwrap :: t (m f) ~> m (t f)

type HFree :: ((i -> Type) -> j -> Type) -> (k -> Type) -> Type

type role HFree nominal nominal

data HFree t f where
  HPure :: f a -> HFree t f
  HFree :: t (HFree t) f -> HFree t f
