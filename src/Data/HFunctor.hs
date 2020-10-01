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


import Data.Kind

-- -------------------------------------------------------------------- --
-- Data

type f ~> g = forall a. f a -> g a
type f <~> g = (f ~> g) -> (g ~> f)

type NT :: (* -> *) -> (* -> *) -> *
type role NT nominal nominal
newtype NT f g where
  NT :: (f ~> g) -> NT f g

type HCompose
  :: ((* -> *) -> * -> *)
  -> (* -> * -> *) -> * -> * -> *
type role HCompose nominal nominal nominal nominal
newtype HCompose f g a b where
  HCompose :: f (g a) b -> HCompose f g a b

type HConst :: (* -> *) -> (* -> *) -> *
type role HConst nominal phantom
newtype HConst f g where
  HConst :: (forall a. f a) -> HConst f g

type HIdentity :: (* -> *) -> *
type role HIdentity nominal
newtype HIdentity f where
  HIdentity :: (forall a. f a) -> HIdentity f

-- -------------------------------------------------------------------- --
-- Classes

class HFunctor t where
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap #-}

class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  hday :: (forall x. f x -> g x -> h x) -> t f a -> t g a -> t h a
  {-# minimal hpure, hday #-}

class HContravariant t where
  hcontramap :: (f ~> g) -> (t g ~> t f)
  {-# minimal hcontramap #-}

class HContravariant t => HDivisible t where
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a
  {-# minimal hdivide #-}

class HDivisible t => HDecideable t where
  hdecide :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  {-# minimal hdecide #-}

class HFunctor t => HTraversable t where
  htraverse :: Applicative u => (forall x. f x -> u (g x)) -> t f a -> u (t g a)
  {-# minimal htraverse #-}

class HFunctor t => HDistributive t where
  hdistribute :: HFunctor u => u (t f) ~> t (u g)
  {-# minimal hdistribute #-}

class HApplicative t => HMonad t where
  hbind :: t f a -> (f ~> (t g)) -> t g a
  hbind t f = hjoin (hmap f t)

  hjoin :: t (t f) ~> t f
  hjoin t = hbind t id
  {-# minimal hjoin | hbind #-}

class HFunctor t => HComonad t where
  hcopure :: t g ~> g

  hcobind :: t g a -> (t g ~> f) -> t f a
  hcobind t f = hmap f (hcojoin t)

  hcojoin :: t f ~> t (t f)
  hcojoin t = hcobind t id
  {-# minimal hcopure, (hcobind | hcojoin) #-}

class (HFunctor l, HFunctor r) => HAdjunction l r where
  hunit :: a ~> r (l a)
  hcounit :: l (r a) ~> a

  hleftAdjunct :: (l a ~> b) -> a ~> r b
  hleftAdjunct f a = hmap f (hunit a)

  hrightAdjunct :: (a ~> r b) -> l a ~> b
  hrightAdjunct f a = hcounit (hmap f a)
  {-# minimal hunit, hcounit #-}

class HProfunctor p where
  hdimap :: (s ~> a) -> (b ~> t) -> p a b -> p s t
  {-# minimal hdimap #-}

class HFunctor t => HRepresentable t where
  type HRep t :: * -> *

  htabulate :: HRep t ~> t f
  hindex :: t f ~> HRep t
  {-# minimal htabulate, hindex #-}
