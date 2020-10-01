{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Data.HFunctor where


import Control.Comonad

import Data.NaturalTransformation


-- -------------------------------------------------------------------- --
-- Data


type HCompose
  :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *
type role HCompose nominal nominal nominal nominal
newtype HCompose t u f a where
  HCompose :: t (u f) a -> HCompose t u f a

type HConst :: (* -> *) -> (* -> *) -> *
type role HConst nominal phantom
newtype HConst f g where
  HConst :: (forall a. f a) -> HConst f g

type HAlongside :: (* -> *) -> (* -> *) -> *
type role HAlongside phantom nominal
newtype HAlongside f g where
  HAlongside :: (forall a. g a) -> HAlongside f g

type HIdentity :: (* -> *) -> *
type role HIdentity nominal
newtype HIdentity f where
  HIdentity :: (forall a. f a) -> HIdentity f

type HFix :: ((* -> *) -> * -> *) -> * -> *
type role HFix nominal nominal
newtype HFix f i where
  HFix :: { unHFix :: f (HFix f) i } -> HFix f i

-- -------------------------------------------------------------------- --
-- Higher Optics? tl;dr dunno, haven't thought too hard about it.

-- type HTraversal s t a b = forall u . HApplicative u => (a ~> u b) -> s ~> u t
-- type HLens s t a b = forall u. HFunctor u => (a ~> u b) -> s ~> u t

-- -------------------------------------------------------------------- --
-- Classes

class HFunctor t where
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap #-}

hhoist :: (HFunctor t, HFunctor u) => (forall k. t k ~> u k) -> t f ~> u f
hhoist = ($)

class HFunctor t => HComonad t where
  hcopure :: t g ~> g

  cextract :: Comonad w => t w a -> (w a -> b) -> t w b

  hcobind :: t g a -> (t g ~> f) -> t f a
  hcobind t f = hmap f (hcojoin t)

  hcojoin :: t f ~> t (t f)
  hcojoin t = hcobind t id
  {-# minimal hcopure, cextract, (hcobind | hcojoin) #-}
