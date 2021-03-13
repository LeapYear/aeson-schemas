{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Data.Schemas (
  module Benchmarks.Data.Schemas,
  module Benchmarks.Data.Schemas.TH,
) where

import Control.Monad (forM)
import Data.Char (chr, toUpper)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.TestUtils ()

import Benchmarks.Data.Schemas.TH

-- Generates:
--
--   type Schema1   = { a1: Int }
--   type Schema5   = { a1: Int, a2: Int, ... a5: Int }
--   type Schema10  = { a1: Int, a2: Int, ... a10: Int }
--   type Schema100 = { a1: Int, a2: Int, ... a100: Int }
$( do
    -- The sizes of schemas to generate
    let schemaSizes = [1, 5, 10, 100]
        allSchemas = flip map schemaSizes $ \n ->
          let name = "Schema" ++ show n
           in (n, name, mkName name)

    concat
      <$> sequence
        [ forM allSchemas $ \(n, _, name) -> genSchema name $ keysTo n
        , [d|
            sizedSchemas :: [(Int, String)]
            sizedSchemas = $(lift $ flip map allSchemas $ \(n, name, _) -> (n, name))

            sizedSchemasNames :: [(String, Name)]
            sizedSchemasNames = $(lift $ flip map allSchemas $ \(_, name, thName) -> (name, thName))
            |]
        ]
 )

-- Generates:
--
--   type SchemaNest1   = { a1: Int }
--   type SchemaNest5   = { a1: { a2: { ... a5: Int } }
--   type SchemaNest10  = { a1: { a2: { ... a10: Int } }
--   type SchemaNest100 = { a1: { a2: { ... a100: Int } }
$( do
    -- The depths of schemas to generate
    let schemaSizes = [1, 5, 10, 100]
        allSchemas = flip map schemaSizes $ \n ->
          let name = "SchemaNest" ++ show n
           in (n, name, mkName name)

    concat
      <$> sequence
        [ forM allSchemas $ \(n, _, name) ->
            genSchema' name $
              foldr (\i inner -> genSchemaDef [Field (mkField i) inner]) "Int" [1 .. n]
        , [d|
            nestedSchemas :: [(Int, String)]
            nestedSchemas = $(lift $ flip map allSchemas $ \(n, name, _) -> (n, name))

            nestedSchemasNames :: [(String, Name)]
            nestedSchemasNames = $(lift $ flip map allSchemas $ \(_, name, thName) -> (name, thName))
            |]
        ]
 )

-- Generates:
--
--   type SchemaA1 = { a1: Int }
--   type SchemaB1 = { b1: Int }
--   ...
--   type SchemaZ1 = { z1: Int }
--   type SchemaA2 = { a2: Int }
--   type SchemaB2 = { a2: Int }
--   ...
$( do
    let numSchemas = 100
        allSchemas = flip map [1 .. numSchemas] $ \n ->
          let (q, r) = n `divMod` 26
              c = chr $ 97 + r -- a .. z
              field = c : show q
              name = "Schema" ++ map toUpper field
           in (field, name, mkName name)

    concat
      <$> sequence
        [ forM allSchemas $ \(field, _, name) -> genSchema name [Field field "Int"]
        , [d|
            singleSchemas :: [String]
            singleSchemas = $(lift $ flip map allSchemas $ \(_, name, _) -> name)

            singleSchemasNames :: [(String, Name)]
            singleSchemasNames = $(lift $ flip map allSchemas $ \(_, name, thName) -> (name, thName))
            |]
        ]
 )
