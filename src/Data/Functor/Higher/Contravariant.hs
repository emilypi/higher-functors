{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Data.Functor.Higher.Contravariant
( HContravariant(..)
, HDivisible(..)
, HDecidable(..)
) where


import Control.Applicative (Alternative(..))
import Control.Applicative.Lift (Lift(..))
import Control.Monad.Trans.Accum (runAccumT, AccumT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS (RWST(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Function.Higher
import Data.Void (Void, absurd)
import Data.Functor.Higher.Const (HConst(..))
import Data.Functor.Contravariant.Divisible (lost, lose, Decidable)


class HContravariant (t :: (i -> Type) -> j -> Type) where
  --fcontramap :: Contravariant f => (a -> b) -> t f b -> t f a
  hcontramap :: (f <~ g) -> (t f ~> t g)
  {-# minimal hcontramap #-}

instance HContravariant (Nop f) where
  hcontramap f (Nop g) = Nop $ g . f

instance HContravariant (HConst f) where
  hcontramap _ = coerce

class HContravariant t => HDivisible t where
  -- adivide :: Divisible f => (a -> (b,c)) -> t f b -> t f c -> t f a
  hdivide :: (forall x. f x -> (g x, h x)) -> t g a -> t h a -> t f a

  hconquer :: t f a
  {-# minimal hdivide, hconquer #-}

instance Alternative f => HDivisible (Nop f) where
  hdivide f (Nop g) (Nop h) = Nop $ \t -> case f t of
    (a,b) -> g a <|> h b
  {-# inline hdivide #-}

  hconquer = Nop $ const empty
  {-# inline hconquer #-}


instance Alternative f => HDivisible (HConst f) where
  hdivide _ _ = coerce
  {-# inline hdivide #-}

  hconquer = HConst empty
  {-# noinline hconquer #-}

class HDivisible t => HDecidable t where
  -- adecide :: Decidable f => (a -> Either b c) -> t f b -> t f c -> t f a
  hchoose :: (forall x. f x -> Either (g x) (h x)) -> t g a -> t h a -> t f a
  hlose :: (forall x. f x -> Void) -> t f a
  {-# minimal hchoose, hlose #-}

instance Alternative f => HDecidable (Nop f) where
  hchoose f (Nop g) (Nop h) = Nop $ \t -> case f t of
    Left a -> g a
    Right b -> h b
  {-# inline hchoose #-}

  hlose v = Nop $ absurd . v
  {-# inline hlose #-}

instance (Decidable f, Alternative f) => HDecidable (HConst f) where
  hchoose _ _ = coerce
  hlose _ = HConst empty
