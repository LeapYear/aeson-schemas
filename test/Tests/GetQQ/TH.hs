{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.GetQQ.TH where

import Control.DeepSeq (deepseq)
import Data.Aeson.QQ (aesonQQ)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.TestUtils (
  MockedMode (..),
  QMode (..),
  QState (..),
  runTestQ,
  runTestQErr,
 )

import Data.Aeson.Schema (Object, get, schema)
import TestUtils (mkExpQQ, parseValue)
import TestUtils.DeepSeq ()

-- For testing namespaced object
testData :: Object [schema| { foo: Maybe Int } |]
testData = parseValue [aesonQQ| { "foo": null } |]

qState :: QState 'FullyMocked
qState =
  QState
    { mode = MockQ
    , knownNames = []
    , reifyInfo = []
    }

{- | Run the `get` quasiquoter at both runtime and compile-time, to get coverage.

 The `get` Quasiquoter doesn't reify anything, so this should work.
-}
runGet :: QuasiQuoter
runGet = mkExpQQ $ \s -> [|runTestQ qState (quoteExp get s) `deepseq` $(quoteExp get s)|]

getErr :: QuasiQuoter
getErr = mkExpQQ $ \s -> [|runTestQErr qState (quoteExp get s)|]
