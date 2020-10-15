{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language RoleAnnotations #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language DerivingVia #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Functor.Higher.Identity
( HIdentity(..)
) where


import Data.Kind (Type)
import Data.Data (Data, Typeable)

import GHC.Generics ( Generic )
import Data.Functor.Contravariant.Generic ()


-- | The identity functor in the 2-category @[Hask,Hask]@ of
-- Haskell endo-functors
--
type HIdentity :: (i -> Type) -> i -> Type
type role HIdentity nominal nominal
newtype HIdentity f a where
  HIdentity :: { runHIdentity :: f a } -> HIdentity f a
  deriving stock
    ( Functor, Foldable, Traversable
    , Data, Typeable, Generic
    )
  deriving newtype
    ( Eq, Ord, Show, Read
    , Num, Fractional, Real
    , Semigroup, Monoid
    )
