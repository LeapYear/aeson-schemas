{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.EnumTH where

import Data.Aeson (decode, encode)
import Data.Char (toLower, toUpper)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Schema.TH (genFromJSONEnum, genToJSONEnum, mkEnum)

mkEnum "State" ["OPEN", "CLOSED"]

data Color = Red | LightBlue | Yellow | DarkGreen | Black | JustABitOffWhite
  deriving (Show, Eq, Enum)

genFromJSONEnum ''Color
genToJSONEnum ''Color

{- Tests -}

test :: TestTree
test =
  testGroup
    "Enum TH helpers"
    [ testMkEnum
    , testGenJSONEnum
    ]

testMkEnum :: TestTree
testMkEnum =
  testGroup
    "mkEnum"
    [ testProperty "mkEnum decode is case insensitive" $ do
        (val, enumVal) <-
          elements
            [ ("OPEN", OPEN)
            , ("CLOSED", CLOSED)
            ]
        casedVal <- randomlyCased val
        return $ decode (encode casedVal) === Just enumVal
    , testCase "mkEnum encode keeps case of constructor" $ do
        encode OPEN @?= "\"OPEN\""
        encode CLOSED @?= "\"CLOSED\""
    , testProperty "mkEnum: (fromJust . decode . encode) === id" $ do
        enumVal <- elements [OPEN, CLOSED]
        return $ (decode . encode) enumVal === Just enumVal
    ]

testGenJSONEnum :: TestTree
testGenJSONEnum =
  testGroup
    "gen{To,From}JSONEnum"
    [ testProperty "genFromJSONEnum decode is case insensitive" $ do
        (val, enumVal) <-
          elements
            [ ("Red", Red)
            , ("LightBlue", LightBlue)
            , ("Yellow", Yellow)
            , ("DarkGreen", DarkGreen)
            , ("Black", Black)
            , ("JustABitOffWhite", JustABitOffWhite)
            ]
        casedVal <- randomlyCased val
        return $ decode (encode casedVal) === Just enumVal
    , testCase "genToJSONEnum encode keeps case of constructor" $ do
        encode Red @?= "\"Red\""
        encode LightBlue @?= "\"LightBlue\""
        encode Yellow @?= "\"Yellow\""
        encode DarkGreen @?= "\"DarkGreen\""
        encode Black @?= "\"Black\""
        encode JustABitOffWhite @?= "\"JustABitOffWhite\""
    , testProperty "genFromJSONEnum + genToJSONEnum: (fromJust . decode . encode) === id" $ do
        enumVal <-
          elements
            [ Red
            , LightBlue
            , Yellow
            , DarkGreen
            , Black
            , JustABitOffWhite
            ]
        return $ (decode . encode) enumVal === Just enumVal
    ]

randomlyCased :: String -> Gen String
randomlyCased s = do
  caseFuncs <- infiniteListOf $ elements [toLower, toUpper]
  return $ zipWith ($) caseFuncs s
