{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Fix where


import Data.Kind


type HFix :: ((i -> Type) -> i -> Type) -> i -> Type
type role HFix nominal nominal
newtype HFix f i where
  HFix :: { unHFix :: f (HFix f) i } -> HFix f i
