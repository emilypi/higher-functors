{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Monad.Higher where



import Data.Applicative.Higher
import Data.Function.Higher
import Data.Functor.Higher (HFunctor(hmap))


class HApplicative t => HMonad t where
  -- mbind :: t m -> (a -> m b) -> t n

  hbind :: t f a -> (f ~> t g) -> t g a
  hbind t f = hjoin (hmap f t)

  hjoin :: t (t f) ~> t f
  hjoin t = hbind t id
  {-# minimal (hjoin | hbind) #-}
