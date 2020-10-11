{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeOperators #-}
module Data.Functor.Higher.Compose
( HCompose(..)
) where

import Data.Kind

type HCompose
  :: ((k -> Type) -> k -> Type)
  -> ((k -> Type) -> k -> Type)
  -> (k -> Type) -> k -> Type
type role HCompose nominal nominal nominal nominal
newtype HCompose
  (t :: (k -> Type) -> k -> Type)
  (u :: (k -> Type) -> k -> Type)
  (f :: k -> Type)
  (a :: k) where
  HCompose :: t (u f) a -> HCompose t u f a
