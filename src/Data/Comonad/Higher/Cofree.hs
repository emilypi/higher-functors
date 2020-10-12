{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DefaultSignatures #-}
{-# language RankNTypes #-}
module Data.Comonad.Higher.Cofree where


data HCofree t f = forall a. f a :<< f (HCofree t f)
