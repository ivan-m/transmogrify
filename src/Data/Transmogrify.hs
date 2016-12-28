{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
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

import Control.Arrow (first)
import Data.Int
import Data.Proxy
import Data.Word
import GHC.Generics

--------------------------------------------------------------------------------

transmogrify :: (SameShape a b) => a -> b
transmogrify = fromRaw . toRaw

class (RawShape a, RawShape b, Raw a ~ Raw b) => SameShape a b
instance (RawShape a, RawShape b, Raw a ~ Raw b) => SameShape a b

cons :: (SameShape (a, b) c) => a -> b -> c
cons a = transmogrify . (,) a

snoc :: (SameShape (a, b) c) => a -> b -> c
snoc = cons

uncons :: (SameShape c (a, b)) => c -> (a, b)
uncons = transmogrify

unsnoc :: (SameShape c (a, b)) => c -> (a, b)
unsnoc = uncons

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

instance (GRawProduct f, GRawShape g) => GRawShape (f :*: g) where

  type GRaw (f :*: g) = GPRaw f (GRaw g)

  gToRaw (a :*: b) = gPToRaw a (gToRaw b)

  gFromRaw r = let (a, r') = gPFromRaw r
               in a :*: gFromRaw r'

--------------------------------------------------------------------------------

class GRawProduct f where

  type GPRaw f (r :: * -> *) :: * -> *

  gPToRaw :: f x -> r x -> GPRaw f r x

  gPFromRaw :: GPRaw f r x -> (f x, r x)

instance (GRawProduct f, GRawProduct g) => GRawProduct (f :*: g) where

  type GPRaw (f :*: g) r = GPRaw f (GPRaw g r)

  gPToRaw (f :*: g) r = gPToRaw f (gPToRaw g r)

  gPFromRaw r = let (f, r') = gPFromRaw r
                in first (f :*:) (gPFromRaw r')

-- Technically does'nt apply to Datatypes, but having two repeated
-- instances for Constructors and Selectors would just be redundant.
instance (GRawProduct f) => GRawProduct (M1 i t f) where

  type GPRaw (M1 i t f) r = GPRaw f r

  gPToRaw = gPToRaw . unM1

  gPFromRaw = first M1 . gPFromRaw

instance (RawProdEnd c) => GRawProduct (K1 i c) where

  type GPRaw (K1 i c) r = GPERaw (Raw c) r

  gPToRaw = peToRaw . unK1

  gPFromRaw = first K1 . peFromRaw

-- | How to handle a value contained within a product; we want to
--   smush products within this overall outer one.
class GRawProdEnd' (f :: * -> *) (b :: Bool) where

  type GPERaw' f b (r :: * -> *) :: * -> *

  gPEToRaw' :: Proxy b -> f x -> r x -> GPERaw' f b r x

  gPEFromRaw' :: Proxy b -> GPERaw' f b r x -> (f x, r x)

type family IsProduct (f :: * -> *) :: Bool where
  IsProduct (f :*: g) = 'True
  IsProduct f         = 'False

instance (GRawProduct f, GRawProduct g) => GRawProdEnd' (f :*: g) 'True where

  type GPERaw' (f :*: g) 'True r = GPRaw (f :*: g) r

  gPEToRaw' _ = gPToRaw

  gPEFromRaw' _ = gPFromRaw

instance (GRawShape f) => GRawProdEnd' f 'False where

  type GPERaw' f 'False r = (GRaw f) :*: r

  gPEToRaw' _ f r = gToRaw f :*: r

  gPEFromRaw' _ (f :*: r) = (gFromRaw f, r)

type RawProdEnd a = (RawShape a, GRawProdEnd (Raw a))

class (GRawProdEnd' f (IsProduct f)) => GRawProdEnd f
instance (GRawProdEnd' f (IsProduct f)) => GRawProdEnd f

type GPERaw f r = GPERaw' f (IsProduct f) r

type PERaw a r = GPERaw (Raw a) r

gPEToRaw :: forall f r x. (GRawProdEnd f) => f x -> r x -> GPERaw f r x
gPEToRaw = gPEToRaw' (Proxy @(IsProduct f))

peToRaw :: (RawProdEnd a) => a -> r x -> PERaw a r x
peToRaw = gPEToRaw . toRaw

gPEFromRaw :: forall f r x. (GRawProdEnd f) => GPERaw f r x -> (f x, r x)
gPEFromRaw = gPEFromRaw' (Proxy @(IsProduct f))

peFromRaw :: (RawProdEnd a) => PERaw a r x -> (a, r x)
peFromRaw = first fromRaw . gPEFromRaw
