{-|
Module      :  Data.Aeson.Schema.Key
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines a SchemaKey.
-}

module Data.Aeson.Schema.Key
  ( SchemaKey(..)
  , fromSchemaKey
  , showSchemaKey
  ) where

-- | A key in a JSON object schema.
data SchemaKey
  = NormalKey String
  | PhantomKey String
    -- ^ A key that doesn't actually exist in the object, but whose content should be parsed from
    -- the current object.
  deriving (Show,Eq)

fromSchemaKey :: SchemaKey -> String
fromSchemaKey (NormalKey key) = key
fromSchemaKey (PhantomKey key) = key

showSchemaKey :: SchemaKey -> String
showSchemaKey (NormalKey key) = show key
showSchemaKey (PhantomKey key) = "[" ++ key ++ "]"
