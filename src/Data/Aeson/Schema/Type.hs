{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Data.Aeson.Schema.Type
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines SchemaType, the AST that defines a JSON schema.
-}
module Data.Aeson.Schema.Type (
  Schema' (..),
  SchemaType' (..),
  SchemaV,
  SchemaTypeV,
  SchemaObjectMapV,
  toSchemaObjectV,
  fromSchemaV,
  showSchemaV,
  showSchemaTypeV,
  Schema,
  SchemaType,
  ToSchemaObject,
  FromSchema,
  IsSchemaType (..),
  IsSchemaObjectMap,
  toSchemaV,
) where

import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import GHC.TypeLits (Symbol)

import Data.Aeson.Schema.Key (
  IsSchemaKey (..),
  SchemaKey,
  SchemaKey',
  SchemaKeyV,
  showSchemaKeyV,
 )
import Data.Aeson.Schema.Utils.All (All (..))
import Data.Aeson.Schema.Utils.Invariant (unreachable)
import Data.Aeson.Schema.Utils.NameLike (NameLike (..), fromName)

-- | The schema definition for a JSON object.
data Schema' s ty = Schema (SchemaObjectMap' s ty)
  deriving (Show, Eq)

-- | The AST defining a JSON schema.
data SchemaType' s ty
  = SchemaScalar ty
  | SchemaMaybe (SchemaType' s ty)
  | -- | @since v1.2.0
    SchemaTry (SchemaType' s ty)
  | SchemaList (SchemaType' s ty)
  | -- | @since v1.1.0
    SchemaUnion [SchemaType' s ty]
  | SchemaObject (SchemaObjectMap' s ty)
  | -- | An optimization for including schemas.
    --
    -- Will always be 'Left' when used in a value-level schema and 'Right' when used in
    -- a type-level schema. We can't use a type parameter for this because type synonyms
    -- can't be recursive (e.g. `type Schema = Schema' Symbol Type Schema`).
    --
    -- @since v1.3.2
    SchemaInclude (Either ty (Schema' s ty))
  deriving (Show, Eq)

type SchemaObjectMap' s ty = [(SchemaKey' s, SchemaType' s ty)]

{- Value-level schema types -}

type SchemaV = Schema' String NameLike
type SchemaTypeV = SchemaType' String NameLike
type SchemaObjectMapV = SchemaObjectMap' String NameLike

toSchemaObjectV :: SchemaV -> SchemaTypeV
toSchemaObjectV (Schema schema) = SchemaObject schema

fromSchemaV :: SchemaV -> SchemaObjectMapV
fromSchemaV (Schema schema) = schema

-- | Show the given schema, as "{ key: Schema, ... }"
showSchemaV :: SchemaV -> String
showSchemaV = showSchemaTypeV' . toSchemaObjectV

-- | Pretty show the given SchemaType.
showSchemaTypeV :: SchemaTypeV -> String
showSchemaTypeV schema = case schema of
  SchemaScalar _ -> "SchemaScalar " ++ showSchemaTypeV' schema
  SchemaMaybe inner -> "SchemaMaybe " ++ showSchemaTypeV' inner
  SchemaTry inner -> "SchemaTry " ++ showSchemaTypeV' inner
  SchemaList inner -> "SchemaList " ++ showSchemaTypeV' inner
  SchemaUnion _ -> "SchemaUnion " ++ showSchemaTypeV' schema
  SchemaObject _ -> "SchemaObject " ++ showSchemaTypeV' schema
  SchemaInclude _ -> "SchemaInclude " ++ showSchemaTypeV' schema

showSchemaTypeV' :: SchemaTypeV -> String
showSchemaTypeV' = \case
  SchemaScalar ty -> fromName ty
  SchemaMaybe inner -> "Maybe " ++ showSchemaTypeV' inner
  SchemaTry inner -> "Try " ++ showSchemaTypeV' inner
  SchemaList inner -> "List " ++ showSchemaTypeV' inner
  SchemaUnion schemas -> "( " ++ mapJoin showSchemaTypeV' " | " schemas ++ " )"
  SchemaObject pairs -> "{ " ++ mapJoin showPair ", " pairs ++ " }"
  SchemaInclude (Left name) -> fromName name
  SchemaInclude (Right _) -> unreachable "Found 'SchemaInclude Right' when showing schema type"
  where
    showPair (key, inner) = showSchemaKeyV key ++ ": " ++ showSchemaTypeV' inner

    mapJoin f delim = intercalate delim . map f

{- Type-level schema types -}

{- | The kind of schemas that may be used with Object; e.g.

 > data Payload (schema :: Schema) = Payload
 >   { getPayload :: Object schema
 >   , timestamp  :: UTCTime
 >   }
-}
type Schema = Schema' Symbol Type

type SchemaType = SchemaType' Symbol Type

type SchemaObjectMap = SchemaObjectMap' Symbol Type

type family ToSchemaObject (schema :: Schema) :: SchemaType where
  ToSchemaObject ( 'Schema schema) = 'SchemaObject schema

type family FromSchema (schema :: Schema) :: SchemaObjectMap where
  FromSchema ( 'Schema schema) = schema

toSchemaV :: forall schema. IsSchemaObjectMap (FromSchema schema) => Proxy schema -> SchemaV
toSchemaV _ = Schema $ toSchemaTypeMapV $ Proxy @(FromSchema schema)

toSchemaTypeMapV :: forall pairs. IsSchemaObjectMap pairs => Proxy pairs -> SchemaObjectMapV
toSchemaTypeMapV _ = mapAll @IsSchemaObjectPair @pairs toSchemaTypePairV

class IsSchemaType (schemaType :: SchemaType) where
  toSchemaTypeV :: Proxy schemaType -> SchemaTypeV

instance Typeable inner => IsSchemaType ( 'SchemaScalar inner) where
  toSchemaTypeV _ = SchemaScalar (NameRef $ tyConName $ typeRepTyCon $ typeRep $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ( 'SchemaMaybe inner) where
  toSchemaTypeV _ = SchemaMaybe (toSchemaTypeV $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ( 'SchemaTry inner) where
  toSchemaTypeV _ = SchemaTry (toSchemaTypeV $ Proxy @inner)

instance IsSchemaType inner => IsSchemaType ( 'SchemaList inner) where
  toSchemaTypeV _ = SchemaList (toSchemaTypeV $ Proxy @inner)

instance All IsSchemaType schemas => IsSchemaType ( 'SchemaUnion schemas) where
  toSchemaTypeV _ = SchemaUnion (mapAll @IsSchemaType @schemas toSchemaTypeV)

instance IsSchemaObjectMap pairs => IsSchemaType ( 'SchemaObject pairs) where
  toSchemaTypeV _ = SchemaObject (toSchemaTypeMapV $ Proxy @pairs)

instance IsSchemaObjectMap (FromSchema schema) => IsSchemaType ( 'SchemaInclude ( 'Right schema)) where
  toSchemaTypeV _ = toSchemaObjectV $ toSchemaV $ Proxy @schema

type IsSchemaObjectMap (pairs :: SchemaObjectMap) = All IsSchemaObjectPair pairs

class IsSchemaObjectPair (a :: (SchemaKey, SchemaType)) where
  toSchemaTypePairV :: Proxy a -> (SchemaKeyV, SchemaTypeV)

instance (IsSchemaKey key, IsSchemaType inner) => IsSchemaObjectPair '(key, inner) where
  toSchemaTypePairV _ = (toSchemaKeyV $ Proxy @key, toSchemaTypeV $ Proxy @inner)
