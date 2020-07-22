{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ.TH where

import Control.DeepSeq (deepseq)
import Data.Aeson (FromJSON)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.Internal (ToSchemaObject, showSchemaType)
import TestUtils (mkExpQQ)
import TestUtils.DeepSeq ()
import TestUtils.TestQ
    (MockedMode(..), QMode(..), QState(..), loadNames, runTestQ, runTestQErr)

type UserSchema = [schema| { name: Text } |]
type ExtraSchema = [schema| { extra: Text } |]
type ExtraSchema2 = [schema| { extra: Maybe Text } |]

newtype Status = Status Int
  deriving (Show,FromJSON)

-- Compile above types before reifying
$(return [])

qState :: QState 'FullyMocked
qState = QState
  { mode = MockQ
  , knownNames =
      [ ("Status", ''Status)
      , ("UserSchema", ''UserSchema)
      , ("ExtraSchema", ''ExtraSchema)
      , ("ExtraSchema2", ''ExtraSchema2)
      , ("Tests.SchemaQQ.TH.UserSchema", ''UserSchema)
      , ("Tests.SchemaQQ.TH.ExtraSchema", ''ExtraSchema)
      , ("Int", ''Int)
      ]
  , reifyInfo = $(loadNames [''ExtraSchema, ''ExtraSchema2, ''Int])
  }

-- | A quasiquoter for generating the string representation of a schema.
--
-- Also runs the `schema` quasiquoter at runtime, to get coverage information.
schemaRep :: QuasiQuoter
schemaRep = mkExpQQ $ \s ->
  let schemaType = quoteType schema s
  in [| runTestQ qState (quoteType schema s) `deepseq` showSchemaType @(ToSchemaObject $schemaType) |]

schemaErr :: QuasiQuoter
schemaErr = mkExpQQ $ \s -> [| runTestQErr qState (quoteType schema s) |]
