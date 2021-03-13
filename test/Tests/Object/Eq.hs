{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Object.Eq where

import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils (parseProxy)
import TestUtils.Arbitrary (ArbitraryObject (..), forAllArbitraryObjects)

test :: TestTree
test =
  testGroup
    "Eq instance"
    [ testProperty "o === o" $
        $(forAllArbitraryObjects) $ \(ArbitraryObject proxy v _) ->
          let o = either error id $ parseProxy proxy v
           in o === o
    ]
