{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Data.Function.Higher
( -- * Natural Transformations
  NT(..)
, NIso(..)
  -- * Type synonyms
, type (~>)
, type (<~>)
) where


import Data.Kind (Type)


-- match the fixity of '(->)' to force the parenthetical
infixr 0 ~>
infixr 0 <~>

-- | A synonym for 'NT'.
--
type (~>) (f :: k -> Type) (g :: k -> Type)
    = forall a. f a -> g a

-- | A synonym for 'NIso'
--
type f <~> g = NIso f g


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
type NIso :: (k -> Type) -> (k -> Type) -> Type
type role NIso nominal nominal
data NIso f g where
  NIso :: (f ~> g) -> (g ~> f) -> NIso f g
