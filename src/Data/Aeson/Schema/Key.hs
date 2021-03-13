{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

{- |
Module      :  Data.Aeson.Schema.Key
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines a SchemaKey.
-}
module Data.Aeson.Schema.Key (
  SchemaKey' (..),
  SchemaKeyV,
  fromSchemaKeyV,
  showSchemaKeyV,
  getContext,
  toContext,
  SchemaKey,
  IsSchemaKey (..),
  fromSchemaKey,
  showSchemaKey,
) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Syntax (Lift)

import Data.Aeson.Schema.Utils.Invariant (unreachable)

-- | A key in a JSON object schema.
data SchemaKey' s
  = NormalKey s
  | -- | A key that doesn't actually exist in the object, but whose content should be parsed from
    -- the current object.
    PhantomKey s
  deriving (Show, Eq, Generic, Hashable, Lift)

-- | A value-level SchemaKey
type SchemaKeyV = SchemaKey' String

fromSchemaKeyV :: SchemaKeyV -> String
fromSchemaKeyV (NormalKey key) = key
fromSchemaKeyV (PhantomKey key) = key

showSchemaKeyV :: SchemaKeyV -> String
showSchemaKeyV (NormalKey key) = show key
showSchemaKeyV (PhantomKey key) = "[" ++ key ++ "]"

{- | Given schema `{ key: innerSchema }` for JSON data `{ key: val1 }`, get the JSON
 Value that `innerSchema` should parse.
-}
getContext :: SchemaKeyV -> Aeson.Object -> Aeson.Value
getContext = \case
  -- `innerSchema` should parse `val1`
  NormalKey key -> HashMap.lookupDefault Aeson.Null (Text.pack key)
  -- `innerSchema` should parse the same object that `key` is in
  PhantomKey _ -> Aeson.Object

{- | Given JSON data `val` adhering to `innerSchema`, get the JSON object that should be
 merged with the outer JSON object.
-}
toContext :: SchemaKeyV -> Aeson.Value -> Aeson.Object
toContext = \case
  -- `val` should be inserted with key `key`
  NormalKey key -> HashMap.singleton (Text.pack key)
  -- If `val` is an object, it should be merged with the outer JSON object
  PhantomKey _ -> \case
    Aeson.Object o -> o
    -- `Try` schema could store `Nothing`, which would return `Null`. In this case, there is no
    -- context to merge
    Aeson.Null -> mempty
    v -> unreachable $ "Invalid value for phantom key: " ++ show v

-- | A type-level SchemaKey
type SchemaKey = SchemaKey' Symbol

class KnownSymbol (FromSchemaKey key) => IsSchemaKey (key :: SchemaKey) where
  type FromSchemaKey key :: Symbol
  toSchemaKeyV :: Proxy key -> SchemaKeyV

instance KnownSymbol key => IsSchemaKey ( 'NormalKey key) where
  type FromSchemaKey ( 'NormalKey key) = key
  toSchemaKeyV _ = NormalKey $ symbolVal $ Proxy @key

instance KnownSymbol key => IsSchemaKey ( 'PhantomKey key) where
  type FromSchemaKey ( 'PhantomKey key) = key
  toSchemaKeyV _ = PhantomKey $ symbolVal $ Proxy @key

fromSchemaKey :: forall key. IsSchemaKey key => String
fromSchemaKey = fromSchemaKeyV $ toSchemaKeyV $ Proxy @key

showSchemaKey :: forall key. IsSchemaKey key => String
showSchemaKey = showSchemaKeyV $ toSchemaKeyV $ Proxy @key
