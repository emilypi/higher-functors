module Data.Profunctor.Higher where


class HProfunctor p where
  pdimap :: Profunctor q => (s -> a) -> (b -> t) -> p (q a) (q b) -> p (q s) (q t)
  hdimap :: (s ~> a) -> (b ~> t) -> p a b -> p s t
  {-# minimal hdimap, pdimap #-}
