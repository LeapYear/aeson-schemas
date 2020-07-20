module Tests.Object where

import Test.Tasty

import qualified Tests.Object.FromJSON
import qualified Tests.Object.Show

test :: TestTree
test = testGroup "Object"
  [ Tests.Object.Show.test
  , Tests.Object.FromJSON.test
  ]
