{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Functor.Higher.Tensor
( HTensor(..)
, hfst
, hsnd
) where


import Control.Applicative


data HTensor f g a where
  HTensor :: f a -> g a -> HTensor f g a


hfst :: HTensor f g a -> f a
hfst (HTensor f _) = f

hsnd :: HTensor f g a -> g a
hsnd (HTensor _ g) = g


instance (Functor f, Functor g) => Functor (HTensor f g) where
  fmap f (HTensor t u) = HTensor (fmap f t) (fmap f u)

instance (Applicative f, Applicative g) => Applicative (HTensor f g) where
  pure a = HTensor (pure a) (pure a)
  HTensor f g <*> HTensor a b = HTensor (f <*> a) (g <*> b)

instance (Alternative f, Alternative g) => Alternative (HTensor f g) where
  empty = HTensor empty empty
  HTensor f g <|> HTensor f' g' = HTensor (f <|> f') (g <|> g')

instance (Monad f, Monad g) => Monad (HTensor f g) where
  return = pure
  -- HTensor f g a -> (a -> HTensor f g b) -> HTensor f g b
  HTensor f g >>= k = HTensor (f >>= hfst . k) (g >>= hsnd . k)
