{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Data.Aeson.Schema.Utils.Sum
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'SumType' data type that represents a sum type consisting of types
specified in a type-level list.
-}
module Data.Aeson.Schema.Utils.Sum (
  SumType (..),
  fromSumType,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError, type (-))

{- | Represents a sum type.

 Loads the first type that successfully parses the JSON value.

 Example:

 @
 data Owl = Owl
 data Cat = Cat
 data Toad = Toad
 type Animal = SumType '[Owl, Cat, Toad]

 Here Owl                         :: Animal
 There (Here Cat)                 :: Animal
 There (There (Here Toad))        :: Animal

 {\- Fails at compile-time
 Here True                        :: Animal
 Here Cat                         :: Animal
 There (Here Owl)                 :: Animal
 There (There (There (Here Owl))) :: Animal
 -\}
 @
-}
data SumType (types :: [Type]) where
  Here :: forall x xs. x -> SumType (x ': xs)
  There :: forall x xs. SumType xs -> SumType (x ': xs)

deriving instance (Show x, Show (SumType xs)) => Show (SumType (x ': xs))
instance Show (SumType '[]) where
  show = \case

deriving instance (Eq x, Eq (SumType xs)) => Eq (SumType (x ': xs))
instance Eq (SumType '[]) where
  _ == _ = True

deriving instance (Ord x, Ord (SumType xs)) => Ord (SumType (x ': xs))
instance Ord (SumType '[]) where
  compare _ _ = EQ

instance (FromJSON x, FromJSON (SumType xs)) => FromJSON (SumType (x ': xs)) where
  parseJSON v = (Here <$> parseJSON v) <|> (There <$> parseJSON v)

instance FromJSON (SumType '[]) where
  parseJSON _ = fail "Could not parse sum type"

instance (ToJSON x, ToJSON (SumType xs)) => ToJSON (SumType (x ': xs)) where
  toJSON = \case
    Here x -> toJSON x
    There xs -> toJSON xs

instance ToJSON (SumType '[]) where
  toJSON = \case

{- Extracting sum type branches -}

class FromSumType (n :: Nat) (types :: [Type]) (x :: Type) where
  fromSumType' :: 'Just x ~ GetIndex n types => proxy1 n -> SumType types -> Maybe x

instance {-# OVERLAPPING #-} FromSumType 0 (x ': xs) x where
  fromSumType' _ = \case
    Here x -> Just x
    There _ -> Nothing

instance
  {-# OVERLAPPABLE #-}
  ( FromSumType (n - 1) xs x
  , 'Just x ~ GetIndex (n - 1) xs
  ) =>
  FromSumType n (_x ': xs) x
  where
  fromSumType' _ = \case
    Here _ -> Nothing
    There xs -> fromSumType' (Proxy @(n - 1)) xs

{- | Extract a value from a 'SumType'

 Example:

 @
 type Animal = SumType '[Owl, Cat, Toad]
 let someAnimal = ... :: Animal

 fromSumType (Proxy :: Proxy 0) someAnimal :: Maybe Owl
 fromSumType (Proxy :: Proxy 1) someAnimal :: Maybe Cat
 fromSumType (Proxy :: Proxy 2) someAnimal :: Maybe Toad

 -- Compile-time error
 -- fromSumType (Proxy :: Proxy 3) someAnimal
 @
-}
fromSumType ::
  ( IsInRange n types
  , 'Just result ~ GetIndex n types
  , FromSumType n types result
  ) =>
  proxy n ->
  SumType types ->
  Maybe result
fromSumType = fromSumType'

{- Helpers -}

type family IsInRange (n :: Nat) (xs :: [Type]) :: Constraint where
  IsInRange n xs =
    IsInRange'
      ( TypeError
          ( 'Text "Index "
              ':<>: 'ShowType n
              ':<>: 'Text " does not exist in list: "
              ':<>: 'ShowType xs
          )
      )
      n
      xs

type family IsInRange' typeErr (n :: Nat) (xs :: [Type]) :: Constraint where
  IsInRange' typeErr _ '[] = typeErr
  IsInRange' _ 0 (_ ': _) = ()
  IsInRange' typeErr n (_ ': xs) = IsInRange' typeErr (n - 1) xs

type family GetIndex (n :: Nat) (types :: [Type]) :: Maybe Type where
  GetIndex 0 (x ': xs) = 'Just x
  GetIndex _ '[] = 'Nothing
  GetIndex n (_ ': xs) = GetIndex (n - 1) xs
