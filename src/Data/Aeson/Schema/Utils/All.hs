{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.Utils.All (
  All (..),
) where

import Data.Proxy (Proxy (..))

-- | A type family for traversing a type-level list.
class All f xs where
  mapAll :: forall a. (forall x. f x => Proxy x -> a) -> [a]
  mapAll f = foldrAll @f @xs f' []
    where
      f' :: forall x. f x => Proxy x -> [a] -> [a]
      f' proxy acc = f proxy : acc

  foldrAll :: (forall x. f x => Proxy x -> a -> a) -> a -> a

instance All f '[] where
  foldrAll _ acc = acc

instance (f x, All f xs) => All f (x ': xs) where
  foldrAll f acc = f (Proxy @x) (foldrAll @f @xs f acc)
