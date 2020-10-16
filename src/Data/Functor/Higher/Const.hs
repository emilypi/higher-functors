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
module Data.Functor.Higher.Const where


import Data.Kind (Type)
import Data.Data (Data, Typeable)

import GHC.Generics ( Generic )
import Data.Functor.Contravariant.Generic ()

-- | A higher 'Const'ant functor. A type is indexed by a higher
-- functor @t@. This is a constant functor in the 2-category @[Hask,Hask]@
--
type HConst :: (i -> Type) -> (j -> Type) -> i -> Type
type role HConst nominal phantom nominal
newtype HConst f g a where
  HConst :: { getHConst :: f a } -> HConst f g a
  deriving stock
    ( Functor, Foldable, Traversable
    , Data, Typeable, Generic
    )
  deriving newtype
    ( Eq, Ord, Show, Read
    , Num, Fractional, Real
    , Semigroup, Monoid
    )


-- | A type-constructor indexed variant of 'Const'
--
type SemiHConst :: Type -> (i -> Type) -> Type
type role SemiHConst nominal phantom
newtype SemiHConst a f where
  SemiHConst :: { getSemiHConst :: a } -> SemiHConst a f
  deriving stock (Data, Typeable, Generic)
  deriving newtype
    ( Eq, Ord, Show, Read
    , Num, Fractional, Real
    , Semigroup, Monoid
    )
