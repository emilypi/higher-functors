{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language RoleAnnotations #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeOperators #-}
{-# language RankNTypes #-}
module Data.Function.Higher
( -- * Natural Transformations
  NT(..)
, NIso(..)
, Nop(..)
  -- * Type synonyms
, type (~>)
, type (<~)
, type (<~>)
) where


import Data.Kind (Type)


infixr 0 ~>, <~>, <~

-- | A synonym for 'NT'.
--
type (~>) (f :: k -> Type) (g :: k -> Type)
    = forall a. f a -> g a

-- | A synonym for 'Nop'. The pun here is "natural 'Op'"
--
type (<~) (f :: k -> Type) (g :: k -> Type)
    = forall a. g a -> f a

-- | A synonym for 'NIso'
--
type (<~>) (f :: k -> Type) (g :: k -> Type)
    = (f ~> g) -> (g ~> f)

-- | The type of natural transformations. Note that in general
-- this is a stronger condition than naturality due to the presence
-- of parametricity.
--
type NT :: (k -> Type) -> (k -> Type) -> Type
type role NT nominal nominal
newtype NT f g where
  NT :: (f ~> g) -> NT f g

-- | The type of natural isomorphisms.
--
type NIso :: (i -> Type) -> (j -> Type) -> Type
type role NIso nominal nominal
data NIso f g where
  NIso :: (f <~> g) -> NIso f g

-- | The type of natural transformations in the opposite
-- functor category. @('->')@ is to 'NT' as 'Op' is to 'Nop'
--
type Nop :: (i -> Type) -> (i -> Type) -> Type
type role Nop nominal nominal
newtype Nop f g where
  Nop :: (f <~ g) -> Nop f g
