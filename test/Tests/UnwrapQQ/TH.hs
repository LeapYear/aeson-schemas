{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.UnwrapQQ.TH where

import Control.DeepSeq (deepseq)
import Language.Haskell.TH (appTypeE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (Object, schema, unwrap)
import TestUtils (ShowSchemaResult(..), mkExpQQ)
import TestUtils.DeepSeq ()
import TestUtils.TestQ
    (MockedMode(..), QMode(..), QState(..), loadNames, runTestQ, runTestQErr)

type ListSchema = [schema| { ids: List Int } |]
type MaybeSchema = [schema| { class: Maybe Text } |]
type SumSchema = [schema| { verbosity: Int | Bool } |]
type ABCSchema = [schema|
  {
    a: Bool,
    b: Bool,
    c: Double,
  }
|]

type MySchema = [schema|
  {
    users: List {
      name: Text,
    },
  }
|]

type MySchemaResult = Object MySchema

-- Compile above types before reifying
$(return [])

qState :: QState 'FullyMocked
qState = QState
  { mode = MockQ
  , knownNames =
      [ ("ListSchema", ''ListSchema)
      , ("MaybeSchema", ''MaybeSchema)
      , ("SumSchema", ''SumSchema)
      , ("ABCSchema", ''ABCSchema)
      , ("NotASchema", ''Maybe)
      , ("MySchemaResult", ''MySchemaResult)
      ]
  , reifyInfo = $(
      loadNames
        [ ''ListSchema
        , ''MaybeSchema
        , ''SumSchema
        , ''ABCSchema
        , ''Maybe
        , ''MySchema
        , ''MySchemaResult
        ]
    )
  }

-- | A quasiquoter for generating the string representation of an unwrapped schema.
--
-- Also runs the `unwrap` quasiquoter at runtime, to get coverage information.
unwrapRep :: QuasiQuoter
unwrapRep = mkExpQQ $ \s ->
  let showSchemaResultQ = appTypeE [| showSchemaResult |] (quoteType unwrap s)
  in [| runTestQ qState (quoteType unwrap s) `deepseq` $showSchemaResultQ |]

unwrapErr :: QuasiQuoter
unwrapErr = mkExpQQ $ \s -> [| runTestQErr qState (quoteType unwrap s) |]
