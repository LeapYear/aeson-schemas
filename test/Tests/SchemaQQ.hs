{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ where

import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import TestUtils (testParseError)
import Tests.SchemaQQ.TH

test :: TestTree
test =
  testGroup
    "`schema` quasiquoter"
    [ testValidSchemas
    , testInvalidSchemas
    , testKeys
    ]

testValidSchemas :: TestTree
testValidSchemas =
  testGroup
    "Valid schemas"
    [ testCase "Object with Bool field" $
        assertMatches
          [schemaRep| { a: Bool } |]
          [r| SchemaObject { "a": Bool } |]
    , testCase "Object with Int field" $
        assertMatches
          [schemaRep| { a: Int } |]
          [r| SchemaObject { "a": Int } |]
    , testCase "Object with Double field" $
        assertMatches
          [schemaRep| { foo123: Double } |]
          [r| SchemaObject { "foo123": Double } |]
    , testCase "Object with Text field" $
        assertMatches
          [schemaRep| { some_text: Text } |]
          [r| SchemaObject { "some_text": Text } |]
    , testCase "Object with a field with a custom type" $
        assertMatches
          [schemaRep| { status: Status } |]
          [r| SchemaObject { "status": Status } |]
    , testCase "Object with a field with a Maybe type" $
        assertMatches
          [schemaRep| { a: Maybe Bool } |]
          [r| SchemaObject { "a": Maybe Bool } |]
    , testCase "Object with a field with a Try type" $
        assertMatches
          [schemaRep| { a: Try Bool } |]
          [r| SchemaObject { "a": Try Bool } |]
    , testCase "Object with a nested object" $
        assertMatches
          [schemaRep| { a: { b: Int } } |]
          [r| SchemaObject { "a": { "b": Int } } |]
    , testCase "Object with a nullable nested object" $
        assertMatches
          [schemaRep| { a: Maybe { b: Int } } |]
          [r| SchemaObject { "a": Maybe { "b": Int } } |]
    , testCase "Object with a list of nested objects" $
        assertMatches
          [schemaRep| { a: List { b: Int } } |]
          [r| SchemaObject { "a": List { "b": Int } } |]
    , testCase "Object with an imported schema" $
        assertMatches
          [schemaRep| { user: #UserSchema } |]
          [r| SchemaObject { "user": { "name": Text } } |]
    , testCase "Object with a qualified imported schema" $
        assertMatches
          [schemaRep| { user: #(Tests.SchemaQQ.TH.UserSchema) } |]
          [r| SchemaObject { "user": { "name": Text } } |]
    , testCase "Object with an imported schema that uses a non-imported type" $
        assertMatches
          [schemaRep| { a: #SchemaWithHiddenImport } |]
          [r| SchemaObject { "a": { "a": CBool } } |]
    , testCase "Object with an imported schema that itself imports a schema" $
        assertMatches
          [schemaRep| { a: #WithUser } |]
          [r| SchemaObject { "a": { "user": { "name": Text } } } |]
    , testCase "Object with an extended schema" $
        assertMatches
          [schemaRep| { a: Int, #ExtraSchema } |]
          [r| SchemaObject { "a": Int, "extra": Text } |]
    , testCase "Object with a qualified extended schema" $
        assertMatches
          [schemaRep| { a: Int, #(Tests.SchemaQQ.TH.ExtraSchema) } |]
          [r| SchemaObject { "a": Int, "extra": Text } |]
    , testCase "Object with an extended schema that uses a non-imported type" $
        assertMatches
          [schemaRep| { #SchemaWithHiddenImport } |]
          [r| SchemaObject { "a": CBool } |]
    , testCase "Object with an extended schema with a shadowed key" $
        assertMatches
          [schemaRep| { extra: Bool, #ExtraSchema } |]
          [r| SchemaObject { "extra": Bool } |]
    , testCase "Object with a qualified extended schema with a shadowed key" $
        assertMatches
          [schemaRep| { extra: Bool, #(Tests.SchemaQQ.TH.ExtraSchema) } |]
          [r| SchemaObject { "extra": Bool } |]
    , testCase "Object with a union field" $
        assertMatches
          [schemaRep| { a: List Int | Text } |]
          [r| SchemaObject { "a": ( List Int | Text ) } |]
    , testCase "Object with a union field with a group" $
        assertMatches
          [schemaRep| { a: List (Int | Text) } |]
          [r| SchemaObject { "a": List ( Int | Text ) } |]
    , testCase "Object with a phantom key for an object" $
        assertMatches
          [schemaRep| { [a]: { b: Int } } |]
          [r| SchemaObject { [a]: { "b": Int } } |]
    , testCase "Object with a phantom key for a Maybe" $
        assertMatches
          [schemaRep| { [a]: Maybe { b: Int } } |]
          [r| SchemaObject { [a]: Maybe { "b": Int } } |]
    , testCase "Object with a phantom key for a Try" $
        assertMatches
          [schemaRep| { [a]: Try { b: Int } } |]
          [r| SchemaObject { [a]: Try { "b": Int } } |]
    , testCase "Object with a phantom key for a non-object Try" $
        assertMatches
          [schemaRep| { [a]: Try Bool } |]
          [r| SchemaObject { [a]: Try Bool } |]
    , testCase "Object with a phantom key for a union of valid schemas" $
        assertMatches
          [schemaRep| { [a]: { b: Int } | Int } |]
          [r| SchemaObject { [a]: ( { "b": Int } | Int ) } |]
    ]

testInvalidSchemas :: TestTree
testInvalidSchemas =
  testGroup
    "Invalid schemas"
    [ testCase "Object with a duplicate key" $
        [schemaErr| { a: Int, a: Bool } |] @?= "Key 'a' specified multiple times"
    , testCase "Object with a duplicate phantom key" $
        [schemaErr| { a: Int, [a]: { b: Bool } } |] @?= "Key 'a' specified multiple times"
    , testCase "Object with a duplicate key from extending" $
        [schemaErr| { #ExtraSchema, #ExtraSchema2 } |] @?= "Key 'extra' declared in multiple imported schemas"
    , testCase "Quasiquoter defining a non-object" $
        [schemaErr| List { a: Int } |] @?= "`schema` definition must be an object"
    , testCase "Object with a field with an unknown type" $
        [schemaErr| { a: HelloWorld } |] @?= "Unknown type: HelloWorld"
    , testCase "Object extending a non-schema" $
        [schemaErr| { #Int } |] @?= "'GHC.Types.Int' is not a Schema"
    , testCase "Object importing an unknown schema" $
        [schemaErr| { foo: #FooSchema } |] @?= "Unknown schema: FooSchema"
    , testCase "Object extending an unknown schema" $
        [schemaErr| { #FooSchema } |] @?= "Unknown schema: FooSchema"
    , testCase "Object with a phantom key for a scalar" $
        [schemaErr| { [a]: Int } |] @?= "Invalid schema for 'a': SchemaScalar Int"
    , testCase "Object with a phantom key for a list" $
        [schemaErr| { [a]: List Int } |] @?= "Invalid schema for 'a': SchemaList Int"
    , testCase "Object with a phantom key for a non-object Maybe" $
        [schemaErr| { [a]: Maybe Int } |] @?= "Invalid schema for 'a': SchemaMaybe Int"
    , testCase "Object with a phantom key for an invalid union" $
        [schemaErr| { [a]: Bool | Int } |] @?= "Invalid schema for 'a': SchemaUnion ( Bool | Int )"
    ]

testKeys :: TestTree
testKeys =
  testGroup
    "Keys in schemas"
    [ testCase "Quoted key same as plain key" $
        [schemaRep| { a: Int } |] @?= [schemaRep| { "a": Int } |]
    , testParseError
        "Key with invalid character"
        "schemaqq_key_with_invalid_character.golden"
        [schemaErr| { "a:b": Int } |]
    , testCase "Key with escaped invalid character" $
        assertMatches
          [schemaRep| { "a\:b": Int } |]
          [r| SchemaObject { "a:b": Int } |]
    , testParseError
        "Key with trailing escape"
        "schemaqq_key_with_trailing_escape.golden"
        [schemaErr| { "a\": Int } |]
    , testCase "Quoted key that starts with '//'" $
        assertMatches
          [schemaRep| { "//a": { b: Int } } |]
          [r| SchemaObject { "//a": { "b": Int } } |]
    , testCase "Phantom key that starts with '//'" $
        assertMatches
          [schemaRep| { [//a]: { b: Int } } |]
          [r| SchemaObject { [//a]: { "b": Int } } |]
    ]

{- Helpers -}

assertMatches :: String -> String -> Assertion
assertMatches a b = strip a @?= strip b
  where
    strip = Text.unpack . Text.strip . Text.pack
