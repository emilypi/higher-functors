{-# language PolyKinds #-}
{-# language FlexibleContexts #-}
{-# language DefaultSignatures #-}
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
newtype HCompose t u f a where
  HCompose :: { getHCompose :: t (u f) a } -> HCompose t u f a
