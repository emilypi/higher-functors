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

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Profunctor
import Data.Void (Void)


-- -------------------------------------------------------------------- --
-- Data

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

type HCompose
  :: ((* -> *) -> * -> *)
  -> ((* -> *) -> * -> *)
  -> (* -> *)
  -> *
  -> *
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

type HTraversal s t a b = forall u . HApplicative u => (a ~> u b) -> s ~> u t
type HLens s t a b = forall u. HFunctor u => (a ~> u b) -> s ~> u t

-- -------------------------------------------------------------------- --
-- Classes

class HFunctor t where
  ffmap :: Functor f => (a -> b) -> t f a -> t f b
  hmap :: (f ~> g) -> (t f ~> t g)
  {-# minimal hmap, ffmap #-}

class HFunctor t => HApplicative t where
  hpure :: f ~> t f
  happly :: (forall x. f x -> g x -> u f g x) -> t f a -> t g b -> t (u f g) c

  alift2 :: Applicative f => (a -> b -> c) -> t f a -> t f b -> t f c
  {-# minimal alift2, hpure, happly #-}

class HContravariant t where
  fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f ~> g) -> (t g ~> t f)
  {-# minimal hcontramap, fcontramap #-}

class HContravariant t => HDivisible t where
  adivide :: Divisible f => (a -> (b,c)) -> t f b -> t f c -> t f a
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a

  hconquer :: t f a
  {-# minimal hdivide, adivide, hconquer #-}

class HDivisible t => HDecideable t where
  adecide :: Decidable f => (a -> Either b c) -> t f b -> t f c -> t f a
  hdecide :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  hlose :: (f a -> f Void) -> t f a
  {-# minimal hdecide, adecide, hlose #-}

class HFunctor t => HTraversable t where
  atraverse :: Applicative h => (forall i. f i -> h (g i)) -> t f a -> h (t g b)

  htraverse :: HApplicative u => (forall i. f i -> u g i) -> t f ~> u (t g)
  htraverse f = hsequence . hmap f

  hsequence :: HApplicative u => t (u f) ~> u (t f)
  hsequence = htraverse id
  {-# minimal atraverse, (htraverse | hsequence) #-}

class HFunctor t => HDistributive t where
  acotraverse :: Functor h => (forall i. h (g i) -> f i) -> h (t g b) -> t f a

  hcotraverse :: HFunctor u => (forall i. u g i -> f i) -> u (t g) ~> t f
  hcotraverse f = hmap f . hdistribute

  hdistribute :: HFunctor u => u (t f) ~> t (u f)
  hdistribute = hcotraverse id
  {-# minimal acotraverse, (hdistribute | hdistribute) #-}

class HApplicative t => HMonad t where
  mbind :: Monad m => t m a -> (a -> m b) -> t m b

  hbind :: t f a -> (f ~> t g) -> t g a
  hbind t f = hjoin (hmap f t)

  hjoin :: t (t f) ~> t f
  hjoin t = hbind t id
  {-# minimal mbind, (hjoin | hbind) #-}

class HFunctor t => HComonad t where
  hcopure :: t g ~> g

  cextract :: Comonad w => t w a -> (w a -> b) -> t w b

  hcobind :: t g a -> (t g ~> f) -> t f a
  hcobind t f = hmap f (hcojoin t)

  hcojoin :: t f ~> t (t f)
  hcojoin t = hcobind t id
  {-# minimal hcopure, cextract, (hcobind | hcojoin) #-}

class (HFunctor l, HFunctor r) => HAdjunction l r where
  hunit :: a ~> r (l a)
  hcounit :: l (r a) ~> a

  hleftAdjunct :: (l a ~> b) -> a ~> r b
  hleftAdjunct f a = hmap f (hunit a)

  hrightAdjunct :: (a ~> r b) -> l a ~> b
  hrightAdjunct f a = hcounit (hmap f a)
  {-# minimal hunit, hcounit #-}

class HProfunctor p where
  pdimap :: Profunctor q => (s -> a) -> (b -> t) -> p (q a) (q b) -> p (q s) (q t)
  hdimap :: (s ~> a) -> (b ~> t) -> p a b -> p s t
  {-# minimal hdimap, pdimap #-}

class HFunctor t => HRepresentable t where
  type HRep t :: * -> *

  htabulate :: HRep t ~> t f
  htabulate = case hrepresent of NIso _ g -> g

  hindex :: t f ~> HRep t
  hindex = case hrepresent of NIso f _ -> f

  hrepresent :: t f <~> HRep t
  hrepresent = NIso hindex htabulate
  {-# minimal (hindex, htabulate) | hrepresent  #-}
