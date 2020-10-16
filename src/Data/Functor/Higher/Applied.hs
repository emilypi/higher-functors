{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Applied
( Applied(..)
) where


import Data.Kind


-- | 'Applied' represents a type-constructor applied to a haskell type.
-- The container can be changed by 'semimap'ping constructors.
--
type Applied :: i -> (i -> Type) -> Type
type role Applied nominal nominal
newtype Applied a f = Applied { runApplied :: f a }
