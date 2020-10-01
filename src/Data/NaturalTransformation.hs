{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.NaturalTransformation
( -- * Transformations
  NT(..)
, NIso(..)
  -- * Type synonyms
, type (~>)
, type (<~>)
) where

type f ~> g = forall a. f a -> g a
type f <~> g = NIso f g

type NT :: (* -> *) -> (* -> *) -> *
type role NT nominal nominal
newtype NT f g where
  NT :: (f ~> g) -> NT f g

type NIso :: (* -> *) -> (* -> *) -> *
type role NIso nominal nominal
data NIso f g where
  NIso :: (f ~> g) -> (g ~> f) -> NIso f g
