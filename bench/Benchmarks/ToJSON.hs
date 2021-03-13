{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Benchmarks.ToJSON where

import Criterion.Main
import qualified Data.Aeson as Aeson

import Benchmarks.Data.Objects
import Benchmarks.Data.Schemas

benchmarks :: Benchmark
benchmarks =
  bgroup
    "ToJSON instance"
    [ byKeys
    , byNestedKeys
    ]
  where
    byKeys =
      bgroup
        "# of keys"
        [ bench "1" $ nf Aeson.toJSON (schemaObject @Schema1)
        , bench "5" $ nf Aeson.toJSON (schemaObject @Schema5)
        , bench "10" $ nf Aeson.toJSON (schemaObject @Schema10)
        , bench "100" $ nf Aeson.toJSON (schemaObject @Schema100)
        ]

    byNestedKeys =
      bgroup
        "# of nested keys"
        [ bench "1" $ nf Aeson.toJSON (schemaObject @SchemaNest1)
        , bench "5" $ nf Aeson.toJSON (schemaObject @SchemaNest5)
        , bench "10" $ nf Aeson.toJSON (schemaObject @SchemaNest10)
        , bench "100" $ nf Aeson.toJSON (schemaObject @SchemaNest100)
        ]
