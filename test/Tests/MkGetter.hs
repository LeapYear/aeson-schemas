{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.MkGetter where

import Control.DeepSeq (deepseq)
import Data.Text (Text)
import Language.Haskell.TH.TestUtils (
  QMode (..),
  QState (..),
  loadNames,
  runTestQ,
  runTestQErr,
 )
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Object, get, mkGetter, schema)
import TestUtils (json, showSchemaResult)
import TestUtils.DeepSeq ()

type MySchema = [schema| { users: List { name: Text } } |]

mkGetter "User" "getUsers" ''MySchema ".users[]"

test :: TestTree
test =
  runMkGetterQ
    `deepseq` testGroup
      "`mkGetter` helper"
      [ testCase "Type synonym is generated" $
          showSchemaResult @User @?= [r|Object (SchemaObject { "name": Text })|]
      , testCase "Getter function is generated" $
          let users :: [User]
              users = getUsers testData

              getName :: User -> Text
              getName = [get| .name |]
           in map getName users @?= ["Alice", "Bob", "Claire"]
      , testCase "mkGetter expression should be a lambda expression" $
          let msg = runTestQErr qState $ mkGetter "User" "getUsers" ''MySchema "foo.users[]"
           in msg @?= "Getter expression should start with '.': foo.users[]"
      ]
  where
    qState =
      QState
        { mode = MockQ
        , knownNames = []
        , reifyInfo = $(loadNames [''MySchema])
        }

    -- run same mkGetter expression that was spliced, for coverage
    runMkGetterQ = runTestQ qState $ mkGetter "User" "getUsers" ''MySchema ".users[]"

testData :: Object MySchema
testData =
  [json|
  {
    "users": [
      { "name": "Alice" },
      { "name": "Bob" },
      { "name": "Claire" }
    ]
  }
|]
