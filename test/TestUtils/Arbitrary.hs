{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtils.Arbitrary
  ( ArbitraryObject(..)
  , forAllArbitraryObjects
  ) where

import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON(..), Value(..), encode)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub, stripPrefix)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH (ExpQ, listE, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Internal (SchemaType(..))
import qualified Data.Aeson.Schema.Internal as Internal
import Data.Aeson.Schema.Key
    (IsSchemaKey(..), SchemaKey, SchemaKey'(..), SchemaKeyV, toContext)
import qualified Data.Aeson.Schema.Show as SchemaShow
import Data.Aeson.Schema.Utils.All (All(..))

-- Orphan instances
deriving instance Lift SchemaShow.SchemaType

data ArbitraryObject where
  ArbitraryObject
    :: ( Eq (Object schema)
       , Show (Object schema)
       , FromJSON (Object schema)
       , ToJSON (Object schema)
       )
    => Proxy (Object schema)
    -> Value
    -> SchemaShow.SchemaType
    -> ArbitraryObject

-- Show the value and schema as something that could be copied/pasted into GHCi.
instance Show ArbitraryObject where
  show (ArbitraryObject _ v schemaType) = unlines
    [ "ArbitraryObject:"
    , "  " ++ show (encode v)
    , "  [schema| " ++ showSchemaType schemaType ++ " |]"
    ]

-- | A Template Haskell function to generate a splice for QuickCheck tests to generate arbitrary
-- objects with arbitrary schemas.
--
-- Note that for repeated runs of the test suite, the schemas will be the same, with the actual
-- JSON values generated randomly. You need to recompile in order to generate different schemas.
arbitraryObject :: ExpQ
arbitraryObject = do
  arbitrarySchemas <- runIO $ genSchemaTypes 20

  [| oneof $(listE $ map mkSchemaGen arbitrarySchemas) |]
  where
    mkSchemaGen schemaShow =
      let schemaType = quoteType schema $ showSchemaType schemaShow
      in [| genSchema' (Proxy :: Proxy (Object $schemaType)) schemaShow |]

-- | Splices to a 'forAll' with 'arbitraryObject', outputting information about the object
-- generated, to ensure we get good generation.
--
-- $(forAllArbitraryObjects) :: Testable prop => ArbitraryObject -> prop
forAllArbitraryObjects :: ExpQ
forAllArbitraryObjects = [| \runTest ->
  forAll $arbitraryObject $ \o@(ArbitraryObject _ _ schemaType) ->
    tabulate' "Key types" (map getKeyType $ getKeys schemaType) $
    tabulate' "Schema types" (getSchemaTypes schemaType) $
    tabulate' "Object sizes" (map show $ getObjectSizes schemaType) $
    tabulate' "Object depth" [show $ getObjectDepth schemaType] $
    runTest o
  |]

{- Run time helpers -}

genSchema' :: forall schema.
  ( ArbitrarySchema ('SchemaObject schema)
  , Internal.IsSchemaType ('SchemaObject schema)
  )
  => Proxy (Object ('Internal.Schema schema)) -> SchemaShow.SchemaType -> Gen ArbitraryObject
genSchema' proxy schemaType = do
  v <- genSchema @('SchemaObject schema)
  return $ ArbitraryObject proxy v schemaType

getKeyType :: SchemaKeyV -> String
getKeyType = \case
  NormalKey _ -> "Normal"
  PhantomKey _ -> "Phantom"

getKeys :: SchemaShow.SchemaType -> [SchemaKeyV]
getKeys = \case
  SchemaShow.SchemaMaybe inner -> getKeys inner
  SchemaShow.SchemaTry inner -> getKeys inner
  SchemaShow.SchemaList inner -> getKeys inner
  SchemaShow.SchemaUnion schemas -> concatMap getKeys schemas
  SchemaShow.SchemaObject pairs -> concatMap (\(key, inner) -> key : getKeys inner) pairs
  _ -> []

getSchemaTypes :: SchemaShow.SchemaType -> [String]
getSchemaTypes = \case
  SchemaShow.SchemaScalar s -> [s]
  SchemaShow.SchemaMaybe inner -> "SchemaMaybe" : getSchemaTypes inner
  SchemaShow.SchemaTry inner -> "SchemaTry" : getSchemaTypes inner
  SchemaShow.SchemaList inner -> "SchemaList" : getSchemaTypes inner
  SchemaShow.SchemaUnion schemas -> "SchemaUnion" : concatMap getSchemaTypes schemas
  SchemaShow.SchemaObject pairs -> "SchemaObject" : concatMap (getSchemaTypes . snd) pairs

getObjectSizes :: SchemaShow.SchemaType -> [Int]
getObjectSizes = \case
  SchemaShow.SchemaScalar _ -> []
  SchemaShow.SchemaMaybe inner -> getObjectSizes inner
  SchemaShow.SchemaTry inner -> getObjectSizes inner
  SchemaShow.SchemaList inner -> getObjectSizes inner
  SchemaShow.SchemaUnion schemas -> concatMap getObjectSizes schemas
  SchemaShow.SchemaObject pairs -> length pairs : concatMap (getObjectSizes . snd) pairs

getObjectDepth :: SchemaShow.SchemaType -> Int
getObjectDepth = \case
  SchemaShow.SchemaScalar _ -> 0
  SchemaShow.SchemaMaybe inner -> getObjectDepth inner
  SchemaShow.SchemaTry inner -> getObjectDepth inner
  SchemaShow.SchemaList inner -> getObjectDepth inner
  SchemaShow.SchemaUnion schemas -> maximum $ map getObjectDepth schemas
  SchemaShow.SchemaObject pairs -> 1 + maximum (map (getObjectDepth . snd) pairs)

tabulate' :: String -> [String] -> Property -> Property
#if MIN_VERSION_QuickCheck(2,12,0)
tabulate' = tabulate
#else
tabulate' _ _ = id
#endif

{- Generating schemas -}

class ArbitrarySchema (schema :: SchemaType) where
  genSchema :: Gen Value

instance {-# OVERLAPS #-} ArbitrarySchema ('SchemaScalar Text) where
  genSchema = toJSON <$> arbitrary @String

instance (Arbitrary inner, ToJSON inner, Typeable inner) => ArbitrarySchema ('SchemaScalar inner) where
  genSchema = toJSON <$> arbitrary @inner

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaMaybe inner) where
  genSchema = frequency
    [ (3, genSchema @inner)
    , (1, pure Null)
    ]

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaTry inner) where
  genSchema = frequency
    [ (3, genSchema @inner)
    , (1, genValue)
    ]
    where
      genValue = oneof
        [ pure Null
        , Number . realToFrac <$> arbitrary @Double
        , Bool <$> arbitrary
        , String . Text.pack <$> arbitrary
        ]

instance ArbitrarySchema inner => ArbitrarySchema ('SchemaList inner) where
  genSchema = Array . fromList <$> listOf (genSchema @inner)

instance All ArbitrarySchema schemas => ArbitrarySchema ('SchemaUnion schemas) where
  genSchema = oneof $ mapAll @ArbitrarySchema @schemas genSchemaElem
    where
      genSchemaElem :: forall schema. ArbitrarySchema schema => Proxy schema -> Gen Value
      genSchemaElem _ = genSchema @schema

instance All ArbitraryObjectPair pairs => ArbitrarySchema ('SchemaObject (pairs :: [(SchemaKey, SchemaType)])) where
  genSchema = Object . HashMap.unions <$> genSchemaPairs
    where
      genSchemaPairs :: Gen [Aeson.Object]
      genSchemaPairs = sequence $ mapAll @ArbitraryObjectPair @pairs genSchemaPair

class IsSchemaKey (Fst pair) => ArbitraryObjectPair (pair :: (SchemaKey, SchemaType)) where
  genSchemaPair :: Proxy pair -> Gen Aeson.Object
  genSchemaPair _ = toContext schemaKey <$> genInnerSchema @pair
    where
      schemaKey = toSchemaKeyV $ Proxy @(Fst pair)

  genInnerSchema :: Gen Value

instance (IsSchemaKey key, ArbitrarySchema schema) => ArbitraryObjectPair '(key, schema) where
  genInnerSchema = genSchema @schema

-- For phantom keys, Maybe is only valid for Objects. Since phantom keys parse the schema with
-- the current object as the context, we should guarantee that this only generates objects, and
-- not Null.
instance {-# OVERLAPS #-} (KnownSymbol key, inner ~ 'SchemaObject a, ArbitrarySchema inner)
  => ArbitraryObjectPair '( 'PhantomKey key, 'SchemaMaybe inner ) where
  genInnerSchema = genSchema @inner

-- For phantom keys, Try can be used on any schema, but for all non-object schemas, need to ensure
-- we generate 'Null', because Try on a non-object schema will always be an invalid parse.
instance {-# OVERLAPS #-} (KnownSymbol key, ArbitrarySchema ('SchemaTry inner))
  => ArbitraryObjectPair '( 'PhantomKey key, 'SchemaTry inner ) where
  genInnerSchema = castNull <$> genSchema @('SchemaTry inner)
    where
      castNull inner =
        case inner of
          Object _ -> inner
          _ -> Null

-- For phantom keys, Union can be used on any schemas, as long as at least one is an object schema.
instance {-# OVERLAPS #-} (KnownSymbol key, FilterObjectSchemas schemas ~ objectSchemas, ArbitrarySchema ('SchemaUnion objectSchemas))
  => ArbitraryObjectPair '( 'PhantomKey key, 'SchemaUnion schemas ) where
  genInnerSchema = genSchema @('SchemaUnion objectSchemas)

{- Generating schema definitions -}

genSchemaTypes :: Int -> IO [SchemaShow.SchemaType]
genSchemaTypes numSchemasToGenerate =
  generate $ sequence $ take numSchemasToGenerate
    [ resize n arbitrary | n <- [0,2..] ]

instance Arbitrary SchemaShow.SchemaType where
  arbitrary = sized genSchemaObject

-- | Generate an arbitrary schema.
--
-- SchemaType is a recursive definition, so we want to make sure that generating a schema will
-- terminate, and also not take too long. The ways we account for that are:
--  * Providing an upper bound on the depth of any object schemas in the current object (n / 2)
--  * Providing an upper bound on the number of keys in the current object (n / 3)
--  * Providing an upper bound on the number of schemas in a union (n / 5)
genSchemaObject :: Int -> Gen SchemaShow.SchemaType
genSchemaObject n = do
  keys <- genUniqList1 (n `div` 3) genKey
  pairs <- forM keys $ \key -> frequency
    [ (10, genSchemaObjectPairNormal key)
    , (1, genSchemaObjectPairPhantom key)
    ]

  return $ SchemaShow.SchemaObject pairs
  where
    genSchemaObject' = do
      n' <- choose (0, n `div` 2)
      genSchemaObject n'

    genSchemaObjectPairNormal key = do
      schemaType <- frequency $ if n == 0
        then scalarSchemaTypes
        else allSchemaTypes
      return (NormalKey key, schemaType)

    genSchemaObjectPairPhantom key = do
      schemaType <- frequency
        [ (2, SchemaShow.SchemaMaybe <$> genSchemaObject')
        , (2, SchemaShow.SchemaTry <$> frequency nonNullableSchemaTypes)
        , (4, genSchemaObject')
        , (1, genSchemaUnion genSchemaObject')
        ]
      return (PhantomKey key, schemaType)

    scalarSchemaTypes =
      [ (4, pure $ SchemaShow.SchemaScalar "Bool")
      , (4, pure $ SchemaShow.SchemaScalar "Int")
      , (4, pure $ SchemaShow.SchemaScalar "Double")
      , (4, pure $ SchemaShow.SchemaScalar "Text")
      ]

    nonNullableSchemaTypes =
      scalarSchemaTypes ++
      [ (2, SchemaShow.SchemaList <$> frequency allSchemaTypes)
      , (1, genSchemaUnion $ frequency allSchemaTypes)
      , (2, genSchemaObject')
      ]

    allSchemaTypes =
      nonNullableSchemaTypes ++
      [ (2, SchemaShow.SchemaMaybe <$> frequency nonNullableSchemaTypes)
      , (2, SchemaShow.SchemaTry <$> frequency nonNullableSchemaTypes)
      ]

    -- avoid generating big unions by scaling list length
    genSchemaUnion gen = SchemaShow.SchemaUnion <$> genUniqList1 (n `div` 5) gen


-- | Generate a valid JSON key
-- See Data.Aeson.Schema.TH.Parse.jsonKey'
genKey :: Gen String
genKey = listOf1 $ arbitraryPrintableChar `suchThat` (`notElem` " \"\\!?[](),.@:{}#")

-- | Generate a non-empty and unique list of the given generator.
--
-- Takes in the max size of the list.
genUniqList1 :: Eq a => Int -> Gen a -> Gen [a]
genUniqList1 n gen = do
  k <- choose (1, max 1 n)
  take k . nub <$> infiniteListOf gen

-- | Show the given SchemaType as appropriate for the schema quasiquoter.
showSchemaType :: SchemaShow.SchemaType -> String
showSchemaType schemaType =
  case "SchemaObject " `stripPrefix` schemaType' of
    Just s -> s
    Nothing -> error $ "Invalid schema: " ++ schemaType'
  where
    schemaType' = SchemaShow.showSchemaType schemaType

{- Helper type families -}

type family Fst x where
  Fst '(a, _) = a

type family FilterObjectSchemas schemas where
  FilterObjectSchemas '[] = '[]
  FilterObjectSchemas ('SchemaObject inner ': xs) = 'SchemaObject inner : FilterObjectSchemas xs
  FilterObjectSchemas (_ ': xs) = FilterObjectSchemas xs
