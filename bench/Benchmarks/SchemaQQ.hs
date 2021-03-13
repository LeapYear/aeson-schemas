{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.SchemaQQ where

import Criterion.Main
import Language.Haskell.TH.Quote (QuasiQuoter (quoteType))
import Language.Haskell.TH.TestUtils (
  QMode (..),
  QState (..),
  loadNames,
  runTestQ,
 )

import qualified Data.Aeson.Schema

import Benchmarks.Data.Schemas
import Utils.DeepSeq ()

benchmarks :: Benchmark
benchmarks =
  bgroup
    "schema quasiquoter"
    [ byKeys
    , byNestedKeys
    , byNumOfIncluded
    , byKeysInIncluded
    , byNumOfExtended
    , byKeysInExtended
    ]
  where
    byKeys = bgroup "# of keys" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ keysTo n
         in bench (show n) $ nf runSchema schemaDef

    byNestedKeys = bgroup "# of nested keys" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = iterateN n (\prev -> genSchemaDef [Field "a" prev]) "Int"
         in bench (show n) $ nf runSchema schemaDef

    byNumOfIncluded = bgroup "Include given # of schemas" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ map (\name -> Include name name) $ take n singleSchemas
         in bench (show n) $ nf runSchema schemaDef

    byKeysInIncluded = bgroup "Include schema with given # of keys" $
      flip map sizedSchemas $ \(n, schema) ->
        let schemaDef = genSchemaDef [Include "a" schema]
         in bench (show n) $ nf runSchema schemaDef

    byNumOfExtended = bgroup "Extend given # of schemas" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ map Ref $ take n singleSchemas
         in bench (show n) $ nf runSchema schemaDef

    byKeysInExtended = bgroup "Extend schema with given # of keys" $
      flip map sizedSchemas $ \(n, schema) ->
        let schemaDef = genSchemaDef [Field "a" "Int", Ref schema]
         in bench (show n) $ nf runSchema schemaDef

    runSchema =
      let qstate =
            QState
              { mode = MockQ
              , knownNames = sizedSchemasNames ++ singleSchemasNames
              , reifyInfo = $(loadNames $ map snd $ sizedSchemasNames ++ singleSchemasNames)
              }
       in runTestQ qstate . quoteType Data.Aeson.Schema.schema

{- Utilities -}

{- | Apply the given functions the given number of times.

 The first parameter must be >= 0.
-}
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x = iterate f x !! n
