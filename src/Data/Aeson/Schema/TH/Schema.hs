{-|
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Schema (schema) where

import Control.Monad (unless, (>=>))
import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (Schema(..), SchemaType(..), ToSchemaObject)
import Data.Aeson.Schema.Key (SchemaKey(..), fromSchemaKey)
import qualified Data.Aeson.Schema.Show as SchemaShow
import Data.Aeson.Schema.TH.Parse
import Data.Aeson.Schema.TH.Utils
    (parseSchemaType, schemaPairsToTypeQ, typeQListToTypeQ, typeToSchemaPairs)

-- | Defines a QuasiQuoter for writing schemas.
--
-- Example:
--
-- > import Data.Aeson.Schema (schema)
-- >
-- > type MySchema = [schema|
-- >   {
-- >     foo: {
-- >       a: Int,
-- >       // you can add comments like this
-- >       nodes: List {
-- >         b: Maybe Bool,
-- >       },
-- >       c: Text,
-- >       d: Text,
-- >       e: MyType,
-- >       f: Maybe List {
-- >         name: Text,
-- >       },
-- >     },
-- >   }
-- > |]
--
-- Syntax:
--
-- * @{ key: \<schema\>, ... }@ corresponds to a JSON 'Data.Aeson.Schema.Object' with the given key
--   mapping to the given schema.
--
-- * @Bool@, @Int@, @Double@, and @Text@ correspond to the usual Haskell values.
--
-- * @Maybe \<schema\>@ and @List \<schema\>@ correspond to @Maybe@ and @[]@, containing values
--   specified by the provided schema (no parentheses needed).
--
-- * @Try \<schema\>@ corresponds to @Maybe@, where the value will be @Just@ if the given schema
--   successfully parses the value, or @Nothing@ otherwise. Different from @Maybe \<schema\>@,
--   where parsing @{ "foo": true }@ with @{ foo: Try Int }@ returns @Nothing@, whereas it would
--   be a parse error with @{ foo: Maybe Int }@ (added in v1.2.0)
--
-- * Any other uppercase identifier corresponds to the respective type in scope -- requires a
--   FromJSON instance.
--
-- Advanced syntax:
--
-- * @\<schema1\> | \<schema2\>@ corresponds to a JSON value that matches one of the given schemas.
--   When extracted from an 'Data.Aeson.Schema.Object', it deserializes into a
--   'Data.Aeson.Schema.Utils.Sum.JSONSum' object. (added in v1.1.0)
--
-- * @{ [key]: \<schema\> }@ uses the current object to resolve the keys in the given schema. Only
--   object schemas are allowed here. (added in v1.2.0)
--
-- * @{ key: #Other, ... }@ maps the given key to the @Other@ schema. The @Other@ schema needs to
--   be defined in another module.
--
-- * @{ #Other, ... }@ extends this schema with the @Other@ schema. The @Other@ schema needs to
--   be defined in another module.
schema :: QuasiQuoter
schema = QuasiQuoter
  { quoteExp = error "Cannot use `schema` for Exp"
  , quoteDec = error "Cannot use `schema` for Dec"
  , quoteType = parse schemaDef >=> \case
      SchemaDefObj items -> [t| 'Schema $(generateSchemaObject items) |]
      _ -> fail "`schema` definition must be an object"
  , quotePat = error "Cannot use `schema` for Pat"
  }

generateSchemaObject :: [SchemaDefObjItem] -> TypeQ
generateSchemaObject = concatMapM toParts >=> resolveParts >=> schemaPairsToTypeQ
  where
    concatMapM f = fmap concat . mapM f

generateSchema :: SchemaDef -> TypeQ
generateSchema = \case
  SchemaDefType "Bool"   -> [t| 'SchemaBool |]
  SchemaDefType "Int"    -> [t| 'SchemaInt |]
  SchemaDefType "Double" -> [t| 'SchemaDouble |]
  SchemaDefType "Text"   -> [t| 'SchemaText |]
  SchemaDefType other    -> [t| 'SchemaCustom $(getType other) |]
  SchemaDefMaybe inner   -> [t| 'SchemaMaybe $(generateSchema inner) |]
  SchemaDefTry inner     -> [t| 'SchemaTry $(generateSchema inner) |]
  SchemaDefList inner    -> [t| 'SchemaList $(generateSchema inner) |]
  SchemaDefInclude other -> [t| ToSchemaObject $(getType other) |]
  SchemaDefObj items     -> [t| 'SchemaObject $(generateSchemaObject items) |]
  SchemaDefUnion schemas -> [t| 'SchemaUnion $(typeQListToTypeQ $ map generateSchema schemas) |]

{- Helpers -}

getName :: String -> Q Name
getName ty = maybe (fail $ "Unknown type: " ++ ty) return =<< lookupTypeName ty

getType :: String -> TypeQ
getType = getName >=> conT

data KeySource = Provided | Imported
  deriving (Show,Eq)

-- | Parse SchemaDefObjItem into a list of tuples, each containing a key to add to the schema,
-- the value for the key, and the source of the key.
toParts :: SchemaDefObjItem -> Q [(SchemaKey, TypeQ, KeySource)]
toParts = \case
  SchemaDefObjPair (schemaDefKey, schemaDefType) -> do
    let schemaKey = schemaDefToSchemaKey schemaDefKey
    schemaType <- generateSchema schemaDefType

    case schemaKey of
      PhantomKey _ -> do
        let schemaTypeShow = parseSchemaType schemaType
        unless (isValidPhantomSchema schemaTypeShow) $
          fail $ "Invalid schema for '" ++ fromSchemaKey schemaKey ++ "': " ++ SchemaShow.showSchemaType schemaTypeShow
      _ -> return ()

    pure . tagAs Provided $ [(schemaKey, pure schemaType)]
  SchemaDefObjExtend other -> do
    name <- getName other
    reify name >>= \case
      TyConI (TySynD _ _ (AppT (PromotedT ty) inner)) | ty == 'Schema ->
        pure . tagAs Imported . map (second pure) $ typeToSchemaPairs inner
      _ -> fail $ "'" ++ show name ++ "' is not a Schema"
  where
    tagAs source = map $ \(k,v) -> (k,v,source)
    schemaDefToSchemaKey = \case
      SchemaDefObjKeyNormal key -> NormalKey key
      SchemaDefObjKeyPhantom key -> PhantomKey key
    isValidPhantomSchema = \case
      SchemaShow.SchemaTry _ -> True
      SchemaShow.SchemaObject _ -> True
      SchemaShow.SchemaUnion schemas -> all isValidPhantomSchema schemas
      _ -> False

-- | Resolve the parts returned by 'toParts' as such:
--
-- 1. Any explicitly provided keys shadow/overwrite imported keys
-- 2. Fail if duplicate keys are both explicitly provided
-- 3. Fail if duplicate keys are both imported
resolveParts :: [(SchemaKey, TypeQ, KeySource)] -> Q [(SchemaKey, TypeQ)]
resolveParts parts = do
  resolved <- resolveParts' $ HashMap.fromListWith (++) $ map nameAndSource parts
  return $ mapMaybe (alignWithResolved resolved) parts
  where
    nameAndSource (name, _, source) = (fromSchemaKey name, [source])
    resolveParts' = HashMap.traverseWithKey $ \name sources -> do
      -- invariant: length sources > 0
      let numOf source = length $ filter (== source) sources
      case (numOf Provided, numOf Imported) of
        (1, _) -> return Provided
        (0, 1) -> return Imported
        (x, _) | x > 1 -> fail $ "Key '" ++ name ++ "' specified multiple times"
        (_, x) | x > 1 -> fail $ "Key '" ++ name ++ "' declared in multiple imported schemas"
        _ -> fail "Broken invariant in resolveParts"
    alignWithResolved resolved (key, ty, source) =
      let resolvedSource = resolved HashMap.! fromSchemaKey key
      in if resolvedSource == source
        then Just (key, ty)
        else Nothing
