{-# LANGUAGE TemplateHaskell #-}

module Tests.Object.FromJSON.TH where

import Data.Proxy (Proxy (..))
import Language.Haskell.TH.Quote (QuasiQuoter (quoteType))

import Data.Aeson.Schema (Object, schema)
import TestUtils (mkExpQQ)

schemaProxy :: QuasiQuoter
schemaProxy = mkExpQQ $ \s ->
  let schemaType = [t|Object $(quoteType schema s)|]
   in [|Proxy :: Proxy $schemaType|]
