{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.UnwrapQQ where

import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Object, get, unwrap)
import Tests.UnwrapQQ.TH
import TestUtils (ShowSchemaResult(..), json)

test :: TestTree
test = testGroup "`unwrap` quasiquoter"
  [ testValidUnwrapDefs
  , testInvalidUnwrapDefs
  ]

type User = [unwrap| MySchema.users[] |]

testValidUnwrapDefs :: TestTree
testValidUnwrapDefs = testGroup "Valid unwrap definitions"
  [ testCase "Can unwrap a list" $ do
      assertSchemaResultMatches @[unwrap| ListSchema.ids |] "[Int]"
      assertSchemaResultMatches @[unwrap| ListSchema.ids[] |] "Int"

  , testCase "Can unwrap a maybe" $ do
      assertSchemaResultMatches @[unwrap| MaybeSchema.class |] "Maybe Text"
      assertSchemaResultMatches @[unwrap| MaybeSchema.class! |] "Text"
      assertSchemaResultMatches @[unwrap| MaybeSchema.class? |] "Text"

  , testCase "Can unwrap a sum type" $ do
      assertSchemaResultMatches @[unwrap| SumSchema.verbosity@0 |] "Int"
      assertSchemaResultMatches @[unwrap| SumSchema.verbosity@1 |] "Bool"

  , testCase "Can use unwrapped type" $ do
      let result :: Object MySchema
          result = [json|
            {
              "users": [
                { "name": "Alice" },
                { "name": "Bob" },
                { "name": "Claire" }
              ]
            }
          |]

          users :: [User]
          users = [get| result.users |]

          getName :: User -> String
          getName = Text.unpack . [get| .name |]

      map getName users @?= ["Alice", "Bob", "Claire"]
  ]

testInvalidUnwrapDefs :: TestTree
testInvalidUnwrapDefs = testGroup "Invalid unwrap definitions"
  [ testCase "Unwrap maybe on non-maybe" $ do
      $(getUnwrapQQErr "ListSchema.ids!") @?= "Cannot use `!` operator on schema: SchemaList Int"
      $(getUnwrapQQErr "ListSchema.ids?") @?= "Cannot use `?` operator on schema: SchemaList Int"

  , testCase "Unwrap list on non-list" $
      $(getUnwrapQQErr "MaybeSchema.class[]") @?= "Cannot use `[]` operator on schema: SchemaMaybe Text"

  , testCase "Unwrap nonexistent key" $
      $(getUnwrapQQErr "ListSchema.foo") @?= [r|Key 'foo' does not exist in schema: SchemaObject {"ids": List Int}|]

  , testCase "Unwrap branch on non-branch" $
      $(getUnwrapQQErr "MaybeSchema.class@0") @?= "Cannot use `@` operator on schema: SchemaMaybe Text"

  , testCase "Unwrap out of bounds branch" $
      $(getUnwrapQQErr "SumSchema.verbosity@10") @?= "Branch out of bounds for schema: SchemaUnion ( Int | Bool )"
  ]

assertSchemaResultMatches :: forall schema. ShowSchemaResult schema => String -> Assertion
assertSchemaResultMatches = (schemaStr @?=)
  where
    schemaStr = showSchemaResult @schema
