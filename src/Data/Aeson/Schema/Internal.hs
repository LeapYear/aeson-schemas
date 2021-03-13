{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Data.Aeson.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for declaring JSON schemas.
-}
module Data.Aeson.Schema.Internal where

import Control.Applicative (Alternative (..))

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Fcf (type (<=<), type (=<<))
import qualified Fcf
import GHC.Exts (toList)
import GHC.TypeLits (
  ErrorMessage (..),
  KnownSymbol,
  Symbol,
  TypeError,
  symbolVal,
 )

import Data.Aeson.Schema.Key (
  IsSchemaKey (..),
  SchemaKey,
  SchemaKey' (..),
  fromSchemaKeyV,
  getContext,
  showSchemaKey,
  toContext,
 )
import Data.Aeson.Schema.Type (
  FromSchema,
  IsSchemaObjectMap,
  IsSchemaType (..),
  Schema,
  Schema' (..),
  SchemaType,
  SchemaType' (..),
  ToSchemaObject,
  showSchemaTypeV,
  showSchemaV,
  toSchemaV,
 )
import Data.Aeson.Schema.Utils.All (All (..))
import Data.Aeson.Schema.Utils.Invariant (unreachable)
import Data.Aeson.Schema.Utils.Sum (SumType (..))

{- Schema-validated JSON object -}

{- | The object containing JSON data and its schema.

 Has a 'FromJSON' instance, so you can use the usual @Data.Aeson@ decoding functions.

 > obj = decode "{\"a\": 1}" :: Maybe (Object [schema| { a: Int } |])
-}
newtype Object (schema :: Schema) = UnsafeObject (HashMap Text Dynamic)

instance IsSchema schema => Show (Object schema) where
  showsPrec _ = showValue @(ToSchemaObject schema)

instance IsSchema schema => Eq (Object schema) where
  a == b = toJSON a == toJSON b

instance IsSchema schema => FromJSON (Object schema) where
  parseJSON = parseValue @(ToSchemaObject schema) []

instance IsSchema schema => ToJSON (Object schema) where
  toJSON = toValue @(ToSchemaObject schema)

{- | Convert an 'Object' into a 'HashMap', losing the type information in the schema.

 @since 1.3.0
-}
toMap :: IsSchema ( 'Schema schema) => Object ( 'Schema schema) -> Aeson.Object
toMap = toValueMap

{- Type-level schema definitions -}

{- | The constraint for most operations involving @Object schema@. If you're writing functions
 on general Objects, you should use this constraint. e.g.

 > logObject :: (MonadLogger m, IsSchema schema) => Object schema -> m ()
 > logObject = logInfoN . Text.pack . show

 @since 1.3.0
-}
type IsSchema (schema :: Schema) =
  ( HasSchemaResult (ToSchemaObject schema)
  , All HasSchemaResultPair (FromSchema schema)
  , IsSchemaObjectMap (FromSchema schema)
  , SchemaResult (ToSchemaObject schema) ~ Object schema
  )

{- | Show the given schema.

 Usage:

 > type MySchema = [schema| { a: Int } |]
 > showSchema @MySchema
-}
showSchema :: forall (schema :: Schema). IsSchema schema => String
showSchema = "SchemaObject " ++ showSchemaV schema -- TODO: Remove "SchemaObject" prefix? Or rename to "Schema"?
  where
    schema = toSchemaV $ Proxy @schema

showSchemaType :: forall (schemaType :: SchemaType). HasSchemaResult schemaType => String
showSchemaType = showSchemaTypeV schemaType
  where
    schemaType = toSchemaTypeV $ Proxy @schemaType

{- Conversions from schema types into Haskell types -}

-- | A type family mapping SchemaType to the corresponding Haskell type.
type family SchemaResult (schema :: SchemaType) where
  SchemaResult ( 'SchemaScalar inner) = inner
  SchemaResult ( 'SchemaMaybe inner) = Maybe (SchemaResult inner)
  SchemaResult ( 'SchemaTry inner) = Maybe (SchemaResult inner)
  SchemaResult ( 'SchemaList inner) = [SchemaResult inner]
  SchemaResult ( 'SchemaUnion schemas) = SumType (SchemaResultList schemas)
  SchemaResult ( 'SchemaObject inner) = Object ( 'Schema inner)
  SchemaResult ( 'SchemaInclude ( 'Right schema)) = SchemaResult (ToSchemaObject schema)

type family SchemaResultList (xs :: [SchemaType]) where
  SchemaResultList '[] = '[]
  SchemaResultList (x ': xs) = SchemaResult x ': SchemaResultList xs

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class IsSchemaType schema => HasSchemaResult (schema :: SchemaType) where
  parseValue :: [Text] -> Value -> Parser (SchemaResult schema)
  default parseValue :: FromJSON (SchemaResult schema) => [Text] -> Value -> Parser (SchemaResult schema)
  parseValue path value = parseJSON value <|> parseFail @schema path value

  toValue :: SchemaResult schema -> Value
  default toValue :: ToJSON (SchemaResult schema) => SchemaResult schema -> Value
  toValue = toJSON

  -- Note: Using ShowS here instead of just returning String to avoid quadratic performance when
  -- using (++)
  showValue :: SchemaResult schema -> ShowS
  default showValue :: Show (SchemaResult schema) => SchemaResult schema -> ShowS
  showValue = shows

instance (Show inner, Typeable inner, FromJSON inner, ToJSON inner) => HasSchemaResult ( 'SchemaScalar inner)

instance (HasSchemaResult inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => HasSchemaResult ( 'SchemaMaybe inner) where
  parseValue path = \case
    Null -> return Nothing
    value -> (Just <$> parseValue @inner path value)

instance (HasSchemaResult inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => HasSchemaResult ( 'SchemaTry inner) where
  parseValue path = wrapTry . parseValue @inner path
    where
      wrapTry parser = (Just <$> parser) <|> pure Nothing

instance (HasSchemaResult inner, Show (SchemaResult inner), ToJSON (SchemaResult inner)) => HasSchemaResult ( 'SchemaList inner) where
  parseValue path = \case
    Array a -> traverse (parseValue @inner path) (toList a)
    value -> parseFail @( 'SchemaList inner) path value

instance
  ( All HasSchemaResult schemas
  , All IsSchemaType schemas
  , Show (SchemaResult ( 'SchemaUnion schemas))
  , FromJSON (SchemaResult ( 'SchemaUnion schemas))
  , ToJSON (SchemaResult ( 'SchemaUnion schemas))
  , ParseSumType schemas
  ) =>
  HasSchemaResult ( 'SchemaUnion (schemas :: [SchemaType]))
  where
  parseValue path value = parseSumType @schemas path value <|> parseFail @( 'SchemaUnion schemas) path value

class ParseSumType xs where
  parseSumType :: [Text] -> Value -> Parser (SumType (SchemaResultList xs))

instance ParseSumType '[] where
  parseSumType _ _ = empty

instance (HasSchemaResult schema, ParseSumType schemas) => ParseSumType (schema ': schemas) where
  parseSumType path value = parseHere <|> parseThere
    where
      parseHere = Here <$> parseValue @schema path value
      parseThere = There <$> parseSumType @schemas path value

instance (All HasSchemaResultPair pairs, IsSchemaObjectMap pairs) => HasSchemaResult ( 'SchemaObject pairs) where
  parseValue path = \case
    Aeson.Object o -> UnsafeObject . HashMap.fromList <$> parseValueMap o
    value -> parseFail @( 'SchemaObject pairs) path value
    where
      parseValueMap :: Aeson.Object -> Parser [(Text, Dynamic)]
      parseValueMap o = sequence $ mapAll @HasSchemaResultPair @pairs $ \proxy -> parseValuePair proxy path o

  toValue = Aeson.Object . toValueMap

  showValue o = showString "{ " . intercalateShowS ", " (map fromPair pairs) . showString " }"
    where
      fromPair (k, v) = showString k . showString ": " . v
      pairs = mapAll @HasSchemaResultPair @pairs $ \proxy -> showValuePair proxy o

      -- intercalate for ShowS
      intercalateShowS :: String -> [ShowS] -> ShowS
      intercalateShowS s = concatShowS . intersperse (showString s)

      concatShowS :: [ShowS] -> ShowS
      concatShowS = foldr (.) id

toValueMap :: forall pairs. All HasSchemaResultPair pairs => Object ( 'Schema pairs) -> Aeson.Object
toValueMap o = HashMap.unions $ mapAll @HasSchemaResultPair @pairs (\proxy -> toValuePair proxy o)

class HasSchemaResultPair (a :: (SchemaKey, SchemaType)) where
  parseValuePair :: Proxy a -> [Text] -> Aeson.Object -> Parser (Text, Dynamic)
  toValuePair :: Proxy a -> Object schema -> Aeson.Object
  showValuePair :: Proxy a -> Object schema -> (String, ShowS)

instance
  ( IsSchemaKey key
  , HasSchemaResult inner
  , Typeable (SchemaResult inner)
  ) =>
  HasSchemaResultPair '(key, inner)
  where
  parseValuePair _ path o = do
    inner <- parseValue @inner (key : path) $ getContext schemaKey o
    return (key, toDyn inner)
    where
      schemaKey = toSchemaKeyV $ Proxy @key
      key = Text.pack $ fromSchemaKeyV schemaKey

  toValuePair _ o = toContext schemaKey (toValue @inner val)
    where
      schemaKey = toSchemaKeyV $ Proxy @key
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

  showValuePair _ o = (showSchemaKey @key, showValue @inner val)
    where
      val = unsafeGetKey @inner (Proxy @(FromSchemaKey key)) o

instance IsSchema schema => HasSchemaResult ( 'SchemaInclude ( 'Right schema)) where
  parseValue = parseValue @(ToSchemaObject schema)
  toValue = toValue @(ToSchemaObject schema)
  showValue = showValue @(ToSchemaObject schema)

-- | A helper for creating fail messages when parsing a schema.
parseFail :: forall (schema :: SchemaType) m a. (MonadFail m, HasSchemaResult schema) => [Text] -> Value -> m a
parseFail path value = fail $ msg ++ ": " ++ ellipses 200 (show value)
  where
    msg =
      if null path
        then "Could not parse schema " ++ schema'
        else "Could not parse path '" ++ path' ++ "' with schema " ++ schema'
    path' = Text.unpack . Text.intercalate "." $ reverse path
    schema' = "`" ++ showSchemaType @schema ++ "`"
    ellipses n s = if length s > n then take n s ++ "..." else s

{- Lookups within SchemaObject -}

data UnSchemaKey :: SchemaKey -> Fcf.Exp Symbol
type instance Fcf.Eval (UnSchemaKey ( 'NormalKey key)) = Fcf.Eval (Fcf.Pure key)
type instance Fcf.Eval (UnSchemaKey ( 'PhantomKey key)) = Fcf.Eval (Fcf.Pure key)

-- first-class-families-0.3.0.1 doesn't support partially applying Lookup
type Lookup a = Fcf.Map Fcf.Snd <=< Fcf.Find (Fcf.TyEq a <=< Fcf.Fst)

-- | The type-level function that return the schema of the given key in a 'SchemaObject'.
type family LookupSchema (key :: Symbol) (schema :: Schema) :: SchemaType where
  LookupSchema key ( 'Schema schema) =
    Fcf.Eval
      ( Fcf.FromMaybe
          ( TypeError
              ( 'Text "Key '"
                  ':<>: 'Text key
                  ':<>: 'Text "' does not exist in the following schema:"
                  ':$$: 'ShowType schema
              )
          )
          =<< Lookup key
          =<< Fcf.Map (Fcf.Bimap UnSchemaKey Fcf.Pure) schema
      )

{- | Get a key from the given 'Data.Aeson.Schema.Internal.Object', returned as the type encoded in
 its schema.

 > let o = .. :: Object
 >             ( 'SchemaObject
 >                '[ '("foo", 'SchemaInt)
 >                 , '("bar", 'SchemaObject
 >                      '[ '("name", 'SchemaText)
 >                       ]
 >                 , '("baz", 'SchemaMaybe 'SchemaBool)
 >                 ]
 >             )
 >
 > getKey (Proxy @"foo") o                  :: Bool
 > getKey (Proxy @"bar") o                  :: Object ('SchemaObject '[ '("name", 'SchemaText) ])
 > getKey (Proxy @"name") $ getKey @"bar" o :: Text
 > getKey (Proxy @"baz") o                  :: Maybe Bool
-}
getKey ::
  forall (key :: Symbol) (schema :: Schema) (endSchema :: SchemaType) result.
  ( endSchema ~ LookupSchema key schema
  , result ~ SchemaResult endSchema
  , KnownSymbol key
  , Typeable result
  , Typeable endSchema
  ) =>
  Proxy key ->
  Object schema ->
  result
getKey = unsafeGetKey @endSchema

unsafeGetKey ::
  forall (endSchema :: SchemaType) (key :: Symbol) (schema :: Schema).
  (KnownSymbol key, Typeable (SchemaResult endSchema)) =>
  Proxy key ->
  Object schema ->
  SchemaResult endSchema
unsafeGetKey keyProxy (UnsafeObject object) =
  fromMaybe (unreachable $ "Could not load key: " ++ key) $
    fromDynamic (object ! Text.pack key)
  where
    key = symbolVal keyProxy
