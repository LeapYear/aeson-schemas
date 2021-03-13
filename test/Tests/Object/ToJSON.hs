{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Object.ToJSON where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils (parseProxy)
import TestUtils.Arbitrary (ArbitraryObject (..), forAllArbitraryObjects)

test :: TestTree
test =
  testGroup
    "ToJSON instance"
    [ testProperty "parseJSON . toJSON === pure" $
        $(forAllArbitraryObjects) $ \(ArbitraryObject proxy v _) ->
          let o = either error id $ parseProxy proxy v
           in (parseJSON . toJSON) o === pure o
    ]

{- Realizing Aeson.Parser -}

-- We're defining two Parsers to be equivalent if they evaluate to the same result.
instance Eq a => Eq (Aeson.Parser a) where
  a == b = runParser a == runParser b

instance Show a => Show (Aeson.Parser a) where
  show = show . runParser

runParser :: Aeson.Parser a -> Aeson.Result a
runParser = Aeson.parse id
