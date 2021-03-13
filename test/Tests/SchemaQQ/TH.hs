{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.SchemaQQ.TH where

import Control.DeepSeq (deepseq)
import Data.Aeson (FromJSON, ToJSON)
import Foreign.C (CBool (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.TestUtils (
  MockedMode (..),
  QMode (..),
  QState (..),
  loadNames,
  runTestQ,
  runTestQErr,
 )

import Data.Aeson.Schema (schema, showSchema)
import TestUtils (mkExpQQ)
import TestUtils.DeepSeq ()

type UserSchema = [schema| { name: Text } |]
type ExtraSchema = [schema| { extra: Text } |]
type ExtraSchema2 = [schema| { extra: Maybe Text } |]

newtype Status = Status Int
  deriving (Show, FromJSON, ToJSON)

-- | The type referenced here should not be imported in SchemaQQ.hs nor included in 'knownNames'.
type SchemaWithHiddenImport = [schema| { a: CBool } |]

deriving instance ToJSON CBool
deriving instance FromJSON CBool

-- Compile above types before reifying
$(return [])

type WithUser = [schema| { user: #UserSchema } |]

-- Compile above types before reifying
$(return [])

qState :: QState 'FullyMocked
qState =
  QState
    { mode = MockQ
    , knownNames =
        [ ("Status", ''Status)
        , ("UserSchema", ''UserSchema)
        , ("ExtraSchema", ''ExtraSchema)
        , ("ExtraSchema2", ''ExtraSchema2)
        , ("Tests.SchemaQQ.TH.UserSchema", ''UserSchema)
        , ("Tests.SchemaQQ.TH.ExtraSchema", ''ExtraSchema)
        , ("SchemaWithHiddenImport", ''SchemaWithHiddenImport)
        , ("WithUser", ''WithUser)
        , ("Int", ''Int)
        ]
    , reifyInfo =
        $( loadNames
            [ ''UserSchema
            , ''ExtraSchema
            , ''ExtraSchema2
            , ''SchemaWithHiddenImport
            , ''WithUser
            , ''Int
            ]
         )
    }

{- | A quasiquoter for generating the string representation of a schema.

 Also runs the `schema` quasiquoter at runtime, to get coverage information.
-}
schemaRep :: QuasiQuoter
schemaRep = mkExpQQ $ \s ->
  let schemaType = quoteType schema s
   in [|runTestQ qState (quoteType schema s) `deepseq` showSchema @ $schemaType|]

schemaErr :: QuasiQuoter
schemaErr = mkExpQQ $ \s -> [|runTestQErr qState (quoteType schema s)|]
