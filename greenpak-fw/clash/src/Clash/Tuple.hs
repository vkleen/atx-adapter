{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language DefaultSignatures #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Clash.Tuple where

import Clash.Prelude

import GHC.Generics
import Generics.Kind

class GToClashTuple (f :: LoT k -> Type) where
  type GClashTuple f (x :: LoT k)
  gtoClashTuple :: f x -> GClashTuple f x
  gfromClashTuple :: GClashTuple f x -> f x

instance GToClashTuple U1 where
  type GClashTuple U1 x = ()
  gtoClashTuple _ = ()
  gfromClashTuple _ = U1

instance GToClashTuple (M1 S ('MetaSel ('Just name) unp str dec) (Field t)) where
  type GClashTuple (M1 S ('MetaSel ('Just name) unp str dec) (Field t)) x = name ::: Interpret t x
  gtoClashTuple (M1 (Field x)) = x
  gfromClashTuple x = M1 (Field x)

instance GToClashTuple f => GToClashTuple (M1 C c f) where
  type GClashTuple (M1 C c f) x = GClashTuple f x
  gtoClashTuple (M1 x) = gtoClashTuple x
  gfromClashTuple x = M1 (gfromClashTuple x)

instance GToClashTuple f => GToClashTuple (M1 D c f) where
  type GClashTuple (M1 D c f) x = GClashTuple f x
  gtoClashTuple (M1 x) = gtoClashTuple x
  gfromClashTuple x = M1 (gfromClashTuple x)

instance (GToClashTuple f, GToClashTuple g) => GToClashTuple (f :*: g) where
  type GClashTuple (f :*: g) x = (GClashTuple f x, GClashTuple g x)
  gtoClashTuple (a :*: b) = (gtoClashTuple a, gtoClashTuple b)
  gfromClashTuple (a, b) = gfromClashTuple a :*: gfromClashTuple b

class ToClashTuple (f :: k -> Type) where
  type ClashTuple f (x :: k) :: Type
  toClashTuple :: forall (x :: k). f x -> ClashTuple f x
  fromClashTuple :: forall (x :: k). ClashTuple f x -> f x

  default toClashTuple :: forall (x :: k) .
                        ( GenericK f
                        , GToClashTuple (RepK f)
                        , ClashTuple f x ~ GClashTuple (RepK f) (x :&&: LoT0)
                        )
                       => f x -> ClashTuple f x
  toClashTuple = gtoClashTuple . fromK @_ @f @(x :&&: LoT0)

  default fromClashTuple :: forall (x :: k) .
                          ( GenericK f
                          , GToClashTuple (RepK f)
                          , ClashTuple f x ~ GClashTuple (RepK f) (x :&&: LoT0)
                          )
                         => ClashTuple f x -> f x
  fromClashTuple = toK @_ @f @(x :&&: LoT0) . gfromClashTuple

