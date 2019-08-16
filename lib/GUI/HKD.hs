{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GUI.HKD (
  HKD(..)
  , Identity
  , fromIO
  ) where

import Data.Functor.Identity(Identity)

import GHC.Generics(Generic, K1(..), M1(..), Rep(..), V1(..), U1(..)
                   , (:*:)(..), (:+:)(..), from, to)


type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

fromIO :: ( Generic (f IO)
          , Generic (f Identity)
          , GFromIO (Rep (f IO)) (Rep (f Identity))
          ) => f IO -> IO (f Identity)
fromIO = fmap to . gFromIO . from

class GFromIO i o where
  gFromIO :: i p -> IO (o g)

instance GFromIO (K1 a (IO k)) (K1 a k) where
  gFromIO (K1 k) = K1 <$> k

instance (GFromIO i o, GFromIO i' o')
     => GFromIO (i :*: i') (o :*: o') where
  gFromIO (l :*: r) = (:*:)
                    <$> gFromIO l
                    <*> gFromIO r
  {-# INLINE gFromIO #-}

instance (GFromIO i o, GFromIO i' o')
    => GFromIO (i :+: i') (o :+: o') where
  gFromIO (L1 l) = L1 <$> gFromIO l
  gFromIO (R1 r) = R1 <$> gFromIO r
  {-# INLINE gFromIO #-}

instance GFromIO i o => GFromIO (M1 _a _b i) (M1 _a' _b' o) where
  gFromIO (M1 x) = M1 <$> gFromIO x
  {-# INLINE gFromIO #-}

instance GFromIO V1 V1 where
  gFromIO = undefined
  {-# INLINE gFromIO #-}

instance GFromIO U1 U1 where
  gFromIO U1 = return U1
  {-# INLINE gFromIO #-}
