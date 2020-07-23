{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Object.FromJSON where

import Data.Aeson (FromJSON(..), Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy)
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import Data.Aeson.Schema (Object)
import Tests.Object.FromJSON.TH

test :: TestTree
test = testGroup "FromJSON instance" $ map runTestCase
  [ CheckValid "Scalar valid"
      [schemaProxy| { foo: Text } |]
      $ \(s :: String) -> [aesonQQ| { "foo": #{s} } |]
  , CheckError "Scalar invalid" "fromjson_scalar_invalid.golden"
      [schemaProxy| { foo: Text } |]
      [aesonQQ| { "foo": 1 } |]

  , CheckValid "Maybe valid"
      [schemaProxy| { foo: Maybe Int } |]
      $ \(x :: Maybe Int) -> [aesonQQ| { "foo": #{x} } |]
  , CheckError "Maybe invalid" "fromjson_maybe_invalid.golden"
      [schemaProxy| { foo: Maybe Int } |]
      [aesonQQ| { "foo": true } |]

  , CheckValid "Try valid with valid parse"
      [schemaProxy| { foo: Try Bool } |]
      $ \(x :: Bool) -> [aesonQQ| { "foo": #{x} } |]
  , CheckValid "Try valid with invalid parse"
      [schemaProxy| { foo: Try Bool } |]
      $ \(s :: String) -> [aesonQQ| { "foo": #{s} } |]

  , CheckValid "List valid"
      [schemaProxy| { foo: List Double } |]
      $ \(xs :: [Double]) -> [aesonQQ| { "foo": #{xs} } |]
  , CheckError "List invalid" "fromjson_list_invalid.golden"
      [schemaProxy| { foo: List Double } |]
      [aesonQQ| { "foo": true } |]
  , CheckError "List invalid inner" "fromjson_list_inner_invalid.golden"
      [schemaProxy| { foo: List Double } |]
      [aesonQQ| { "foo": [true] } |]

  , CheckError "Object invalid" "fromjson_object_invalid.golden"
      [schemaProxy| { foo: Int } |]
      [aesonQQ| 1 |]
  , CheckError "Object invalid in later keys" "fromjson_object_later_keys_invalid.golden"
      [schemaProxy| { foo: Int, bar: Int } |]
      [aesonQQ| { "foo": 1, "bar": true } |]

  , CheckValid "Nested object valid"
      [schemaProxy| { foo: { bar: Int } } |]
      $ \(x :: Int) -> [aesonQQ| { "foo": { "bar": #{x} } } |]
  , CheckError "Nested object invalid" "fromjson_nested_invalid.golden"
      [schemaProxy| { foo: { bar: Int } } |]
      [aesonQQ| { "foo": true } |]
  , CheckError "Nested object invalid inner" "fromjson_nested_inner_invalid.golden"
      [schemaProxy| { foo: { bar: Int } } |]
      [aesonQQ| { "foo": { "bar": true } } |]

  , CheckValid "Union object valid"
      [schemaProxy| { foo: Int | Text } |]
      $ \(x :: Int) -> [aesonQQ| { "foo": #{x} } |]
  , CheckError "Union object invalid" "fromjson_union_invalid.golden"
      [schemaProxy| { foo: Int | Text } |]
      [aesonQQ| { "foo": true } |]

  , CheckValid "Phantom key valid"
      [schemaProxy| { [foo]: { bar: Int } } |]
      $ \(x :: Int) -> [aesonQQ| { "bar": #{x} } |]
  , CheckError "Phantom key invalid" "fromjson_phantom_invalid.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| 1 |]
  , CheckError "Phantom key missing inner" "fromjson_phantom_inner_missing.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| { "foo": true } |]
  , CheckError "Phantom key invalid inner" "fromjson_phantom_inner_invalid.golden"
      [schemaProxy| { [foo]: { bar: Int } } |]
      [aesonQQ| { "bar": true } |]

  , CheckError "Decode failure messages are truncated" "fromjson_error_messages_truncate.golden"
      [schemaProxy| { foo: Int } |]
      [aesonQQ|
        {
          "foo": [
            { "bar": 1, "baz": "a" },
            { "bar": 2, "baz": "b" },
            { "bar": 3, "baz": "c" },
            { "bar": 4, "baz": "d" }
          ]
        }
      |]
  ]

{- Helpers -}

data FromJSONTestCase where
  CheckValid
    :: (Arbitrary a, Show a, FromJSON (Object schema))
    => TestName              -- ^ Name of test case
    -> Proxy (Object schema) -- ^ The schema to parse with
    -> (a -> Value)          -- ^ A function that builds a Value that should satisfy the schema
    -> FromJSONTestCase

  CheckError
    :: (FromJSON (Object schema), Show (Object schema))
    => TestName              -- ^ Name of test case
    -> String                -- ^ Name of golden file
    -> Proxy (Object schema) -- ^ The schema to parse with
    -> Value                 -- ^ The value that should fail parsing the given schema
    -> FromJSONTestCase

runTestCase :: FromJSONTestCase -> TestTree
runTestCase = \case
  CheckValid name schema valueGen ->
    testProperty name $ \a ->
      case parse schema (valueGen a) of
        Right _ -> ()
        Left e -> error $ "Unexpected failure: " ++ e

  CheckError name golden schema value ->
    goldenVsString name ("test/goldens/" ++ golden) $
      case parse schema value of
        Right o -> error $ "Unexpectedly parsed: " ++ show o
        Left e -> return $ fromString e

parse :: FromJSON a => Proxy a -> Value -> Either String a
parse _ = parseEither parseJSON
