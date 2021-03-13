{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.GetQQ where

import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.Haskell.Interpreter as Hint
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.TH (mkEnum)
import Data.Aeson.Schema.Utils.Sum (SumType (..))
import TestUtils (parseObject, testGoldenIO, testParseError)
import Tests.GetQQ.TH

mkEnum "Greeting" ["HELLO", "GOODBYE"]

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show, Eq)

instance ToJSON Coordinate where
  toJSON (Coordinate (x, y)) = toJSON $ show x ++ "," ++ show y

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> return $ Coordinate (x, y)
      _ -> fail $ "Bad Coordinate: " ++ Text.unpack s

{- Tests -}

test :: TestTree
test =
  testGroup
    "`get` quasiquoter"
    [ testValidExpressions
    , testInvalidExpressions
    , testCompileTimeErrors
    ]

testValidExpressions :: TestTree
testValidExpressions =
  testGroup
    "Valid get expressions"
    [ testScalarExpressions
    , testBasicExpressions
    , testNullableExpressions
    , testListExpressions
    , testUnionExpressions
    , testPhantomExpressions
    , testNestedExpressions
    ]

testScalarExpressions :: TestTree
testScalarExpressions =
  testGroup
    "Scalar expressions"
    [ testProperty "Get Bool key from object" $ \b ->
        let o = $(parseObject "{ foo: Bool }") [aesonQQ| { "foo": #{b} } |]
         in [runGet| o.foo |] === b
    , testProperty "Get Int key from object" $ \x ->
        let o = $(parseObject "{ foo: Int }") [aesonQQ| { "foo": #{x} } |]
         in [runGet| o.foo |] === x
    , testProperty "Get Double key from object" $ \x ->
        let o = $(parseObject "{ foo: Double }") [aesonQQ| { "foo": #{x} } |]
         in [runGet| o.foo |] === x
    , testProperty "Get Text key from object" $ \(UnicodeText s) ->
        let o = $(parseObject "{ foo: Text }") [aesonQQ| { "foo": #{s} } |]
         in [runGet| o.foo |] === s
    , testProperty "Get Custom key from object" $ \coordinate ->
        let o = $(parseObject "{ foo: Coordinate }") [aesonQQ| { "foo": #{coordinate} } |]
         in [runGet| o.foo |] === coordinate
    , testProperty "Get Enum key from object" $ \greeting ->
        let o = $(parseObject "{ foo: Greeting }") [aesonQQ| { "foo": #{greeting} } |]
         in [runGet| o.foo |] === greeting
    ]

testBasicExpressions :: TestTree
testBasicExpressions =
  testGroup
    "Basic expressions"
    [ testCase "Can query fields on namespaced object" $
        [runGet| (Tests.GetQQ.TH.testData).foo |] @?= [runGet| testData.foo |]
    , testProperty "Can query quoted keys" $ \x ->
        let o = $(parseObject "{ foo: Int }") [aesonQQ| { "foo": #{x :: Int} } |]
         in [runGet| o."foo" |] === [runGet| o.foo |]
    , testProperty "Can query nested fields" $ \x ->
        let o = $(parseObject "{ foo: { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
         in [runGet| o.foo.bar |] === x
    , testProperty "Can generate a lambda expression" $ \x ->
        let o = $(parseObject "{ foo: Int }") [aesonQQ| { "foo": #{x} } |]
         in [runGet| .foo |] o === x
    , testProperty "Can extract a list of elements" $ \x y bar ->
        let o =
              $(parseObject "{ x: Int, y: Int, foo: { bar: Int } }")
                [aesonQQ|
            {
              "x": #{x},
              "y": #{y},
              "foo": { "bar": #{bar} }
            }
          |]
         in [runGet| o.[x, y, x, foo.bar] |] === [x, y, x, bar]
    , testProperty "Can extract a tuple of elements" $ \x b bar ->
        let o =
              $(parseObject "{ x: Int, b: Bool, foo: { bar: Int } }")
                [aesonQQ|
            {
              "x": #{x},
              "b": #{b},
              "foo": { "bar": #{bar} }
            }
          |]
         in [runGet| o.(x,b,x,foo.bar) |] === (x, b, x, bar)
    ]

testNullableExpressions :: TestTree
testNullableExpressions =
  testGroup
    "Nullable expressions"
    [ testCase "Get Maybe key from object with value" $ do
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": true } |]
        [runGet| o.foo |] @?= Just True
        [runGet| o.foo! |] @?= True
    , testCase "Get Maybe key from object with null value" $
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": null } |]
         in [runGet| o.foo |] @?= Nothing
    , testCase "Get Maybe key from object without value" $
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| {} |]
         in [runGet| o.foo |] @?= Nothing
    , testCase "Can run operations within existing Maybe value" $
        let o =
              $(parseObject "{ foo: Maybe List { bar: Bool } }")
                [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
         in [runGet| o.foo?[].bar |] @?= Just [True, True, False]
    , testCase "Can run operations within nonexisting Maybe value" $
        let o = $(parseObject "{ foo: Maybe List { bar: Bool } }") [aesonQQ| { "foo": null } |]
         in [runGet| o.foo?[].bar |] @?= Nothing
    , testCase "Can run operations after unwrapping Maybe value" $
        let o =
              $(parseObject "{ foo: Maybe List { bar: Bool } }")
                [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
         in [runGet| o.foo![].bar |] @?= [True, True, False]
    , testCase "Get Try key from object with parsed value" $ do
        let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": true } |]
        [runGet| o.foo |] @?= Just True
        [runGet| o.foo! |] @?= True
    , testCase "Get Try key from object with invalid value" $
        let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": 1 } |]
         in [runGet| o.foo |] @?= Nothing
    , testCase "Can run operations within parsed Try value" $
        let o =
              $(parseObject "{ foo: Try List { bar: Bool } }")
                [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
         in [runGet| o.foo?[].bar |] @?= Just [True, True, False]
    , testCase "Can run operations within invalid Try value" $
        let o = $(parseObject "{ foo: Try List { bar: Bool } }") [aesonQQ| { "foo": [{ "baz": 1 }] } |]
         in [runGet| o.foo?[].bar |] @?= Nothing
    , testCase "Can run operations after unwrapping Try value" $
        let o =
              $(parseObject "{ foo: Try List { bar: Bool } }")
                [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
         in [runGet| o.foo![].bar |] @?= [True, True, False]
    , testFromJustErrors
    ]

-- test error message for bang operator, to get some coverage on startDisplay + showGetterOps
testFromJustErrors :: TestTree
testFromJustErrors =
  testGroup
    "fromJust errors"
    [ testCase "Plain fromJust" $
        assertError "Called 'fromJust' on null expression" $ [runGet| ! |] (Nothing :: Maybe Int)
    , testCase "With start" $ do
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": null } |]
        assertError
          "Called 'fromJust' on null expression: o.foo"
          [runGet| o.foo! |]
    , testCase "With qualified start" $
        assertError
          "Called 'fromJust' on null expression: (Tests.GetQQ.TH.testData).foo"
          [runGet| (Tests.GetQQ.TH.testData).foo! |]
    , testCase "With lambda" $ do
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": null } |]
        assertError "Called 'fromJust' on null expression: .foo" $ [runGet| .foo! |] o
    , testCase "Within nested list of keys" $ do
        let o = $(parseObject "{ foo: { a: Bool, b: Maybe Bool } }") [aesonQQ| { "foo": { "a": true, "b": null } } |]
        assertError "Called 'fromJust' on null expression: o.foo.b" [runGet| o.foo.[a, b!] |]
    , testCase "Within list of keys" $ do
        let o = $(parseObject "{ foo: Bool, bar: Maybe Bool }") [aesonQQ| { "foo": true, "bar": null } |]
        assertError "Called 'fromJust' on null expression: o.bar" [runGet| o.[foo, bar!] |]
    , testCase "Within nested tuple of keys" $ do
        let o = $(parseObject "{ foo: { a: Int, b: Maybe Bool } }") [aesonQQ| { "foo": { "a": 1, "b": null } } |]
        assertError "Called 'fromJust' on null expression: o.foo.b" [runGet| o.foo.(a, b!) |]
    , testCase "Within tuple of keys" $ do
        let o = $(parseObject "{ foo: Int, bar: Maybe Bool }") [aesonQQ| { "foo": 1, "bar": null } |]
        assertError "Called 'fromJust' on null expression: o.bar" [runGet| o.(foo, bar!) |]
    , testCase "Within list" $ do
        let o = $(parseObject "{ foo: List Maybe Bool }") [aesonQQ| { "foo": [null] } |]
        assertError "Called 'fromJust' on null expression: o.foo[]" [runGet| o.foo[]! |]
    , testCase "On incorrect branch selector" $ do
        let o = $(parseObject "{ foo: Bool | Int }") [aesonQQ| { "foo": 1 } |]
        assertError "Called 'fromJust' on null expression: o.foo@0" [runGet| o.foo@0! |]
    ]
  where
    assertError msg x =
      try @SomeException (x `deepseq` pure ()) >>= \case
        Right _ -> error "Unexpectedly succeeded"
        Left e -> (head . lines . show) e @?= msg

testListExpressions :: TestTree
testListExpressions =
  testGroup
    "List expressions"
    [ testCase "Get List key from object" $
        let o = $(parseObject "{ foo: List Int }") [aesonQQ| { "foo": [1,2,3] } |]
         in [runGet| o.foo |] @?= [1, 2, 3]
    , testProperty "Ending with a `[]` operator is a noop" $ \(xs :: [Int]) ->
        let o = $(parseObject "{ foo: List Int }") [aesonQQ| { "foo": #{xs} } |]
         in [runGet| o.foo |] === [runGet| o.foo[] |]
    , testCase "Can run operations within list" $
        let o =
              $(parseObject "{ foo: List Maybe { bar: Int } }")
                [aesonQQ|
            {
              "foo": [
                { "bar": 1 },
                { "bar": 2 },
                null,
                { "bar": 3 }
              ]
            }
          |]
         in [runGet| o.foo[]?.bar |] @?= [Just 1, Just 2, Nothing, Just 3]
    ]

testUnionExpressions :: TestTree
testUnionExpressions =
  testGroup
    "Union expressions"
    [ testCase "Get Union key from object" $ do
        let o = $(parseObject "{ foo: Bool | Int }") [aesonQQ| { "foo": 1 } |]
        [runGet| o.foo |] @?= There (Here 1)
        [runGet| o.foo@0 |] @?= Nothing
        [runGet| o.foo@1 |] @?= Just 1
    , testCase "Can run operations after extracting branch" $ do
        let o = $(parseObject "{ foo: { bar: Bool } | { baz: Int } }") [aesonQQ| { "foo": { "bar": true } } |]
        [runGet| o.foo@0?.bar |] @?= Just True
        [runGet| o.foo@0!.bar |] @?= True
        [runGet| o.foo@1?.baz |] @?= Nothing
    ]

testPhantomExpressions :: TestTree
testPhantomExpressions =
  testGroup
    "Phantom expressions"
    [ testProperty "Get Phantom object key from object" $ \x ->
        let o = $(parseObject "{ [foo]: { bar: Int } }") [aesonQQ| { "bar": #{x} } |]
         in [runGet| o.foo.bar |] === x
    , testProperty "Get Phantom object Try key from object" $ \x ->
        let o = $(parseObject "{ [foo]: Try { bar: Int } }") [aesonQQ| { "bar": #{x} } |]
         in [runGet| o.foo?.bar |] === Just x
    , testProperty "Get Phantom non-object Try key from object" $ \(x :: Int) ->
        let o = $(parseObject "{ [foo]: Try Int }") [aesonQQ| { "foo": #{x} } |]
         in [runGet| o.foo |] === Nothing
    ]

testNestedExpressions :: TestTree
testNestedExpressions =
  testGroup
    "Nested expressions"
    [ testProperty "Extracted object from Object can be queried further" $ \x ->
        let o = $(parseObject "{ foo: { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
            foo = [runGet| o.foo |]
         in [runGet| foo.bar |] === x
    , testProperty "Extracted object from Maybe can be queried further" $ \x ->
        let o = $(parseObject "{ foo: Maybe { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
            foo = [runGet| o.foo! |]
         in [runGet| foo.bar |] === x
    , testProperty "Extracted object from Try can be queried further" $ \x ->
        let o = $(parseObject "{ foo: Try { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
            foo = [runGet| o.foo! |]
         in [runGet| foo.bar |] === x
    , testProperty "Extracted object from List can be queried further" $ \xs ->
        classify (length xs > 1) "non-trivial" $
          let bars = map (\x -> [aesonQQ| { "bar": #{x} } |]) xs
              o = $(parseObject "{ foo: List { bar: Int } }") [aesonQQ| { "foo": #{bars} } |]
              getBar :: Object [schema| { bar: Int } |] -> Int
              getBar foo = [runGet| foo.bar |]
           in map getBar [runGet| o.foo |] === xs
    , testProperty "Extracted objects from list of keys can be queried further" $ \fooId barId ->
        let o =
              $(parseObject "{ foo: { id: Int }, bar: { id: Int } }")
                [aesonQQ|
            {
              "foo": { "id": #{fooId} },
              "bar": { "id": #{barId} }
            }
          |]
            getId :: Object [schema| { id: Int } |] -> Int
            getId inner = [runGet| inner.id |]
         in map getId [runGet| o.[foo, bar] |] === [fooId, barId]
    , testProperty "Extracted objects from tuple of keys can be queried further" $ \a b ->
        let o =
              $(parseObject "{ foo: { a: Int }, bar: { b: Bool } }")
                [aesonQQ|
            {
              "foo": { "a": #{a} },
              "bar": { "b": #{b} }
            }
          |]
            (foo, bar) = [runGet| o.(foo, bar) |]
         in [runGet| foo.a |] === a .&&. [runGet| bar.b |] === b
    , testProperty "Extracted objects from union can be queried further" $ \x ->
        let o = $(parseObject "{ foo: { x: Bool } | { x: Int } }") [aesonQQ| { "foo": { "x": #{x} } } |]
         in case [runGet| o.foo |] of
              There (Here foo) -> [runGet| foo.x |] === x
              foo -> error $ "Unexpected failure: o.foo = " ++ show foo ++ ", x = " ++ show x
    ]

testInvalidExpressions :: TestTree
testInvalidExpressions =
  testGroup
    "Invalid expressions"
    [ testParseError
        "Empty expression"
        "getqq_empty_expression.golden"
        [getErr| |]
    , testParseError
        "No operators"
        "getqq_no_operators.golden"
        [getErr| o |]
    , testParseError
        "Operators after tuple of keys"
        "getqq_ops_after_tuple.golden"
        [getErr| o.(a,b).foo |]
    , testParseError
        "Operators after list of keys"
        "getqq_ops_after_list.golden"
        [getErr| o.[a,b].foo |]
    ]

testCompileTimeErrors :: TestTree
testCompileTimeErrors =
  testGroup
    "Compile-time errors"
    [ testGoldenIO "Key not in schema" "getqq_missing_key.golden" $
        getCompileError "GetMissingKey"
    ]
  where
    getCompileError name = do
      let fp = "test/wont-compile/" ++ name ++ ".hs"
      Hint.runInterpreter (Hint.loadModules [fp]) >>= \case
        Left (Hint.WontCompile errors) -> return $ unlines $ map Hint.errMsg errors
        Left e -> fail $ show e
        Right _ -> fail "Compilation unexpectedly succeeded"

{- Helpers -}

instance Arbitrary Greeting where
  arbitrary = elements [HELLO, GOODBYE]

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> arbitrary

newtype UnicodeText = UnicodeText Text
  deriving (Show)

instance Arbitrary UnicodeText where
  arbitrary = UnicodeText . Text.pack . getUnicodeString <$> arbitrary
