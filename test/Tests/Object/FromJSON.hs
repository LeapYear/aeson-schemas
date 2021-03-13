{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Object.FromJSON where

import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Aeson.Schema (Object)
import TestUtils (parseProxy, testGolden)
import TestUtils.Arbitrary (ArbitraryObject (..), forAllArbitraryObjects)
import Tests.Object.FromJSON.TH

test :: TestTree
test =
  testGroup "FromJSON instance" $
    map runTestCase testCases
      ++ [ testProperty "QuickCheck arbitrary Schema" $
            $(forAllArbitraryObjects) $ \(ArbitraryObject proxy v _) ->
              case parseProxy proxy v of
                Right _ -> property ()
                Left e -> error $ "Could not parse: " ++ e
         ]

testCases :: [FromJSONTestCase]
testCases =
  [ CheckValid
      "Scalar valid"
      [schemaProxy| { foo: Text } |]
      $ \(s :: String) -> [aesonQQ| { "foo": #{s} } |]
  , CheckError
      "Scalar invalid"
      "fromjson_scalar_invalid.golden"
      [schemaProxy| { foo: Text } |]
      [aesonQQ| { "foo": 1 } |]
  , CheckValid
      "Maybe valid"
      [schemaProxy| { foo: Maybe Int } |]
      $ \(x :: Maybe Int) -> [aesonQQ| { "foo": #{x} } |]
  , CheckError
      "Maybe invalid"
      "fromjson_maybe_invalid.golden"
      [schemaProxy| { foo: Maybe Int } |]
      [aesonQQ| { "foo": true } |]
  , CheckValid
      "Try valid with valid parse"
      [schemaProxy| { foo: Try Bool } |]
      $ \(x :: Bool) -> [aesonQQ| { "foo": #{x} } |]
  , CheckValid
      "Try valid with invalid parse"
      [schemaProxy| { foo: Try Bool } |]
      $ \(s :: String) -> [aesonQQ| { "foo": #{s} } |]
  , CheckValid
      "List valid"
      [schemaProxy| { foo: List Double } |]
      $ \(xs :: [Double]) -> [aesonQQ| { "foo": #{xs} } |]
  , CheckError
      "List invalid"
      "fromjson_list_invalid.golden"
      [schemaProxy| { foo: List Double } |]
      [aesonQQ| { "foo": true } |]
  , CheckError
      "List invalid inner"
      "fromjson_list_inner_invalid.golden"
      [schemaProxy| { foo: List Double } |]
      [aesonQQ| { "foo": [true] } |]
  , CheckError
      "Object invalid"
      "fromjson_object_invalid.golden"
      [schemaProxy| { foo: Int } |]
      [aesonQQ| 1 |]
  , CheckError
      "Object invalid in later keys"
      "fromjson_object_later_keys_invalid.golden"
      [schemaProxy| { foo: Int, bar: Int } |]
      [aesonQQ| { "foo": 1, "bar": true } |]
  , CheckValid
      "Nested object valid"
      [schemaProxy| { foo: { bar: Int } } |]
      $ \(x :: Int) -> [aesonQQ| { "foo": { "bar": #{x} } } |]
  , CheckError
      "Nested object invalid"
      "fromjson_nested_invalid.golden"
      [schemaProxy| { foo: { bar: Int } } |]
      [aesonQQ| { "foo": true } |]
  , CheckError
      "Nested object invalid inner"
      "fromjson_nested_inner_invalid.golden"
      [schemaProxy| { foo: { bar: Int } } |]
      [aesonQQ| { "foo": { "bar": true } } |]
  , CheckValid
      "Union object valid"
      [schemaProxy| { foo: Int | Text } |]
      $ \(x :: Int) -> [aesonQQ| { "foo": #{x} } |]
  , CheckError
      "Union object invalid"
      "fromjson_union_invalid.golden"
      [schemaProxy| { foo: Int | Text } |]
      [aesonQQ| { "foo": true } |]
  , CheckValid
      "Phantom key valid object"
      [schemaProxy| { [foo]: { bar: Int } } |]
      $ \(x :: Int) -> [aesonQQ| { "bar": #{x} } |]
  , CheckValid
      "Phantom key valid non-object try"
      [schemaProxy| { [foo]: Try Bool } |]
      $ \(b :: Bool) -> [aesonQQ| { "bar": #{b} } |]
  , CheckError
      "Phantom key invalid"
      "fromjson_phantom_invalid.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| 1 |]
  , CheckError
      "Phantom key missing inner"
      "fromjson_phantom_inner_missing.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| { "foo": true } |]
  , CheckError
      "Phantom key invalid inner"
      "fromjson_phantom_inner_invalid.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| { "bar": true } |]
  , CheckError
      "Decode failure messages are truncated"
      "fromjson_error_messages_truncate.golden"
      [schemaProxy| { foo: Int } |]
      [aesonQQ|
        {
          "foo": [
            { "bar": 1 },
            { "bar": 2 },
            { "bar": 3 },
            { "bar": 4 },
            { "bar": 5 },
            { "bar": 6 },
            { "bar": 7 },
            { "bar": 8 },
            { "bar": 9 }
          ]
        }
      |]
  ]

{- Helpers -}

data FromJSONTestCase where
  CheckValid ::
    (Arbitrary a, Show a, FromJSON (Object schema)) =>
    -- | Name of test case
    TestName ->
    -- | The schema to parse with
    Proxy (Object schema) ->
    -- | A function that builds a Value that should satisfy the schema
    (a -> Value) ->
    FromJSONTestCase
  CheckError ::
    (FromJSON (Object schema), Show (Object schema)) =>
    -- | Name of test case
    TestName ->
    -- | Name of golden file
    String ->
    -- | The schema to parse with
    Proxy (Object schema) ->
    -- | The value that should fail parsing the given schema
    Value ->
    FromJSONTestCase

runTestCase :: FromJSONTestCase -> TestTree
runTestCase = \case
  CheckValid name schema valueGen ->
    testProperty name $ \a ->
      case parse schema (valueGen a) of
        Right _ -> ()
        Left e -> error $ "Unexpected failure: " ++ e
  CheckError name fp schema value ->
    testGolden name fp $
      case parse schema value of
        Right o -> error $ "Unexpectedly parsed: " ++ show o
        Left e -> e

parse :: FromJSON a => Proxy a -> Value -> Either String a
parse _ = parseEither parseJSON
