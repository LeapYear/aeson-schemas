{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}
module Data.Aeson.Schema.TH.Schema (schema) where

import Control.Monad (unless, (>=>))
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Data.Aeson.Schema.Key (SchemaKey' (..), SchemaKeyV, fromSchemaKeyV)
import Data.Aeson.Schema.TH.Parse (
  SchemaDef (..),
  SchemaDefObjItem (..),
  SchemaDefObjKey (..),
  parseSchemaDef,
 )
import Data.Aeson.Schema.TH.Utils (reifySchema, schemaVToTypeQ)
import Data.Aeson.Schema.Type (
  Schema' (..),
  SchemaObjectMapV,
  SchemaType' (..),
  SchemaTypeV,
  fromSchemaV,
  showSchemaTypeV,
 )
import Data.Aeson.Schema.Utils.Invariant (unreachable)
import Data.Aeson.Schema.Utils.NameLike (NameLike (..))

{- | Defines a QuasiQuoter for writing schemas.

 Example:

 > import Data.Aeson.Schema (schema)
 >
 > type MySchema = [schema|
 >   {
 >     foo: {
 >       a: Int,
 >       // you can add comments like this
 >       nodes: List {
 >         b: Maybe Bool,
 >       },
 >       c: Text,
 >       d: Text,
 >       e: MyType,
 >       f: Maybe List {
 >         name: Text,
 >       },
 >     },
 >   }
 > |]

 Syntax:

 * @{ key: \<schema\>, ... }@ corresponds to a JSON 'Data.Aeson.Schema.Object' with the given key
   mapping to the given schema.

 * @Bool@, @Int@, @Double@, and @Text@ correspond to the usual Haskell values.

 * @Maybe \<schema\>@ and @List \<schema\>@ correspond to @Maybe@ and @[]@, containing values
   specified by the provided schema (no parentheses needed).

 * @Try \<schema\>@ corresponds to @Maybe@, where the value will be @Just@ if the given schema
   successfully parses the value, or @Nothing@ otherwise. Different from @Maybe \<schema\>@,
   where parsing @{ "foo": true }@ with @{ foo: Try Int }@ returns @Nothing@, whereas it would
   be a parse error with @{ foo: Maybe Int }@ (added in v1.2.0)

 * Any other uppercase identifier corresponds to the respective type in scope -- requires a
   FromJSON instance.

 Advanced syntax:

 * @\<schema1\> | \<schema2\>@ corresponds to a JSON value that matches one of the given schemas.
   When extracted from an 'Data.Aeson.Schema.Object', it deserializes into a
   'Data.Aeson.Schema.Utils.Sum.JSONSum' object. (added in v1.1.0)

 * @{ [key]: \<schema\> }@ uses the current object to resolve the keys in the given schema. Only
   object schemas are allowed here. (added in v1.2.0)

 * @{ key: #Other, ... }@ maps the given key to the @Other@ schema. The @Other@ schema needs to
   be defined in another module.

 * @{ #Other, ... }@ extends this schema with the @Other@ schema. The @Other@ schema needs to
   be defined in another module.
-}
schema :: QuasiQuoter
schema =
  QuasiQuoter
    { quoteExp = error "Cannot use `schema` for Exp"
    , quoteDec = error "Cannot use `schema` for Dec"
    , quoteType =
        parseSchemaDef >=> \case
          SchemaDefObj items -> generateSchemaObject items
          _ -> fail "`schema` definition must be an object"
    , quotePat = error "Cannot use `schema` for Pat"
    }
  where
    generateSchemaObject items = schemaVToTypeQ . Schema =<< generateSchemaObjectV items

data KeySource = Provided | Imported
  deriving (Show, Eq)

generateSchemaObjectV :: NonEmpty SchemaDefObjItem -> Q SchemaObjectMapV
generateSchemaObjectV schemaDefObjItems = do
  schemaObjectMapsWithSource <- mapM getSchemaObjectMap schemaDefObjItems

  let schemaObjectMaps :: LookupMap SchemaKeyV (KeySource, SchemaTypeV)
      schemaObjectMaps = concatMap (uncurry distribute) schemaObjectMapsWithSource

  either fail return $ resolveKeys schemaObjectMaps

{- | Get the SchemaObjectMapV for the given SchemaDefObjItem, along with where the SchemaObjectMapV
 came from.
-}
getSchemaObjectMap :: SchemaDefObjItem -> Q (SchemaObjectMapV, KeySource)
getSchemaObjectMap = \case
  SchemaDefObjPair (schemaDefKey, schemaDefType) -> do
    let schemaKey = fromSchemaDefKey schemaDefKey
    schemaType <- fromSchemaDefType schemaDefType

    case schemaKey of
      PhantomKey _ ->
        unless (isValidPhantomSchema schemaType) $
          fail $ "Invalid schema for '" ++ fromSchemaKeyV schemaKey ++ "': " ++ showSchemaTypeV schemaType
      _ -> return ()

    return ([(schemaKey, schemaType)], Provided)
  SchemaDefObjExtend other -> do
    schemaV <- reifySchema other
    return (fromSchemaV schemaV, Imported)
  where
    -- should return true if it's at all possible to get a valid parse
    isValidPhantomSchema = \case
      SchemaMaybe inner -> isValidPhantomSchema inner
      SchemaTry _ -> True -- even if inner is a non-object schema, it'll still parse to be Nothing
      SchemaUnion schemas -> any isValidPhantomSchema schemas
      SchemaObject _ -> True
      SchemaInclude _ -> True
      _ -> False

{- | Resolve the given keys with the following rules:

 1. Any explicitly provided keys shadow/overwrite imported keys
 2. Fail if duplicate keys are both explicitly provided
 3. Fail if duplicate keys are both imported
-}
resolveKeys :: forall a. Show a => LookupMap SchemaKeyV (KeySource, a) -> Either String (LookupMap SchemaKeyV a)
resolveKeys = mapM (uncurry resolveKey) . groupByKeyWith fromSchemaKeyV
  where
    resolveKey :: SchemaKeyV -> [(KeySource, a)] -> Either String (SchemaKeyV, a)
    resolveKey key sourcesAndVals =
      let provided = lookupAll Provided sourcesAndVals
          imported = lookupAll Imported sourcesAndVals
       in if
              | length provided > 1 -> Left $ "Key '" ++ fromSchemaKeyV key ++ "' specified multiple times"
              | [val] <- provided -> Right (key, val)
              | length imported > 1 -> Left $ "Key '" ++ fromSchemaKeyV key ++ "' declared in multiple imported schemas"
              | [val] <- imported -> Right (key, val)
              | otherwise -> unreachable $ "resolveKey received: " ++ show (key, sourcesAndVals)

{- SchemaDef conversions -}

fromSchemaDefKey :: SchemaDefObjKey -> SchemaKeyV
fromSchemaDefKey = \case
  SchemaDefObjKeyNormal key -> NormalKey key
  SchemaDefObjKeyPhantom key -> PhantomKey key

fromSchemaDefType :: SchemaDef -> Q SchemaTypeV
fromSchemaDefType = \case
  SchemaDefType name -> return $ SchemaScalar $ NameRef name
  SchemaDefMaybe inner -> SchemaMaybe <$> fromSchemaDefType inner
  SchemaDefTry inner -> SchemaTry <$> fromSchemaDefType inner
  SchemaDefList inner -> SchemaList <$> fromSchemaDefType inner
  SchemaDefInclude other -> return $ SchemaInclude $ Left $ NameRef other
  SchemaDefUnion schemas -> SchemaUnion . NonEmpty.toList <$> mapM fromSchemaDefType schemas
  SchemaDefObj items -> SchemaObject <$> generateSchemaObjectV items

{- LookupMap utilities -}

type LookupMap k v = [(k, v)]

-- | Distribute the given element across the values in the map.
distribute :: LookupMap k v -> a -> LookupMap k (a, v)
distribute lookupMap a = map (fmap (a,)) lookupMap

{- | Find all values with the same key (according to the given function) and group them.

 Invariants:
 * [v] has length > 0
 * If the first occurence of k1 is before the first occurence of k2, k1 is before k2
   in the result
-}
groupByKeyWith :: (Eq a, Hashable a) => (k -> a) -> LookupMap k v -> LookupMap k [v]
groupByKeyWith f pairs = map (\key -> (key, groups HashMap.! f key)) distinctKeys
  where
    -- don't use sort; keys should stay in the same order
    distinctKeys = nubBy ((==) `on` f) $ map fst pairs

    groups = HashMap.fromListWith (flip (++)) $ map (\(k, v) -> (f k, [v])) pairs

{- Utilities -}

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll a = map snd . filter ((== a) . fst)
