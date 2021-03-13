{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Object.Show where

import Data.Aeson.QQ (aesonQQ)
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import TestUtils (parseObject)
import Tests.Object.Show.TH

test :: TestTree
test =
  testGroup
    "Show instance"
    [ testProperty "Scalar key" $ \(s :: String) ->
        let o = $(parseObject "{ foo: Text }") [aesonQQ| { "foo": #{s} } |]
         in show o === [i|{ "foo": #{show s} }|]
    , testProperty "Object with multiple keys" $ \(b :: Double, x :: Int) ->
        let o = $(parseObject "{ foo: Double, bar: Int }") [aesonQQ| { "foo": #{b}, "bar": #{x} } |]
         in show o === [i|{ "foo": #{show b}, "bar": #{show x} }|]
    , testProperty "Nested object" $ \(x :: Int) ->
        let o = $(parseObject "{ foo: { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
         in show o === [i|{ "foo": { "bar": #{show x} } }|]
    , testProperty "Object with existing Maybe key" $ \(x :: Bool) ->
        let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": #{x} } |]
         in show o === [i|{ "foo": Just #{show x} }|]
    , testCase "Object with non-existing Maybe key" $
        let o = $(parseObject "{ foo: Maybe Double }") [aesonQQ| { "foo": null } |]
         in show o @?= [i|{ "foo": Nothing }|]
    , testProperty "Object with valid Try key" $ \(b :: Bool) ->
        let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": #{b} } |]
         in show o === [i|{ "foo": Just #{show b} }|]
    , testProperty "Object with invalid Try key" $ \(x :: Int) ->
        let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": #{x} } |]
         in show o === [i|{ "foo": Nothing }|]
    , testProperty "Object with List key" $ \(x :: [Int]) ->
        let o = $(parseObject "{ foo: List Int }") [aesonQQ| { "foo": #{x} } |]
         in show o === [i|{ "foo": #{show x} }|]
    , testProperty "Object with Union key branch 0" $ \(Positive (x :: Int)) ->
        let o = $(parseObject "{ foo: Int | Text }") [aesonQQ| { "foo": #{x} } |]
         in show o === [i|{ "foo": Here #{show x} }|]
    , testProperty "Object with Union key branch 1" $ \(s :: String) ->
        let o = $(parseObject "{ foo: Int | Text }") [aesonQQ| { "foo": #{s} } |]
         in show o === [i|{ "foo": There (Here #{show s}) }|]
    , testProperty "Object with referenced Object" $ \(name :: String) ->
        let o = $(parseObject "{ user: #UserSchema }") [aesonQQ| { "user": { "name": #{name} } } |]
         in show o === [i|{ "user": { "name": #{show name} } }|]
    , testProperty "Object with extended Object" $ \(name :: String, age :: Int) ->
        let o = $(parseObject "{ #UserSchema, age: Int }") [aesonQQ| { "name": #{name}, "age": #{age} } |]
         in show o === [i|{ "name": #{show name}, "age": #{show age} }|]
    , testProperty "Object with Phantom key" $ \(x :: Int) ->
        let o = $(parseObject "{ [foo]: { bar: Int } }") [aesonQQ| { "bar": #{x} } |]
         in show o === [i|{ [foo]: { "bar": #{show x} } }|]
    ]
