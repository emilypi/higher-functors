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
type NT :: (k -> Type) -> (k -> Type) -> k -> Type
type role NT nominal nominal nominal
newtype NT f g a where
  NT :: { runNT :: (f ~> g) } -> NT f g a

-- | The type of natural isomorphisms.
--
type NIso :: (i -> Type) -> (j -> Type) -> k -> Type
type role NIso nominal nominal nominal
data NIso f g a where
  NIso :: { runNTIso :: (f <~> g) } -> NIso f g a

-- | The type of natural transformations in the opposite
-- functor category. @('->')@ is to 'NT' as 'Op' is to 'Nop'
--
type Nop :: (i -> Type) -> (i -> Type) -> i -> Type
type role Nop nominal nominal nominal
newtype Nop f g a where
  Nop :: { runNop :: (f <~ g) } -> Nop f g a
