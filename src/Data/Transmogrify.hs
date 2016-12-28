{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, TypeApplications,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

{- |
   Module      : Data.Transmogrify
   Description : Transmogrify between types with the same underlying shape
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Transmogrify where

import Data.Int
import Data.Proxy
import Data.Word
import GHC.Generics

--------------------------------------------------------------------------------

transmogrify :: (SameShape a b) => a -> b
transmogrify = fromRaw . toRaw

class (RawShape a, RawShape b, Raw a ~ Raw b) => SameShape a b
instance (RawShape a, RawShape b, Raw a ~ Raw b) => SameShape a b

--------------------------------------------------------------------------------

class RawShape' a (r :: Bool) where
  type Raw' a r :: * -> *

  toRaw' :: Proxy r -> a -> Raw' a r x

  fromRaw' :: Proxy r -> Raw' a r x -> a

type family CanRecurse a :: Bool where
  CanRecurse Int     = 'False
  CanRecurse Int8    = 'False
  CanRecurse Int16   = 'False
  CanRecurse Int32   = 'False
  CanRecurse Int64   = 'False
  CanRecurse Integer = 'False
  CanRecurse Word    = 'False
  CanRecurse Word8   = 'False
  CanRecurse Word16  = 'False
  CanRecurse Word32  = 'False
  CanRecurse Word64  = 'False
  CanRecurse Float   = 'False
  CanRecurse Double  = 'False
  CanRecurse a       = 'True

instance (Generic a, GRawShape (Rep a)) => RawShape' a 'True where
  type Raw' a 'True = GRaw (Rep a)

  toRaw' _ = gToRaw . from

  fromRaw' _ = to . gFromRaw

instance RawShape' a 'False where
  type Raw' a 'False = Rec0 a

  toRaw' _ = K1

  fromRaw' _ = unK1

class (RawShape' a (CanRecurse a)) => RawShape a
instance (RawShape' a (CanRecurse a)) => RawShape a

type Raw a = Raw' a (CanRecurse a)

toRaw :: forall a x. (RawShape a) => a -> Raw a x
toRaw = toRaw' (Proxy @(CanRecurse a))

fromRaw :: forall a x. (RawShape a) => Raw a x -> a
fromRaw = fromRaw' (Proxy @(CanRecurse a))

--------------------------------------------------------------------------------

class GRawShape (f :: * -> *) where

  type GRaw f :: * -> *

  gToRaw :: f x -> GRaw f x

  gFromRaw :: GRaw f x -> f x

instance GRawShape U1 where

  type GRaw U1 = U1

  gToRaw = id

  gFromRaw = id

instance (GRawShape f) => GRawShape (M1 i t f) where

  type GRaw (M1 i t f) = GRaw f

  gToRaw = gToRaw . unM1

  gFromRaw = M1 . gFromRaw

instance (RawShape c) => GRawShape (K1 i c) where

  type GRaw (K1 i c) = Raw c

  gToRaw = toRaw . unK1

  gFromRaw = K1 . fromRaw
