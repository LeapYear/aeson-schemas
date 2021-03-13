{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Benchmarks.Show where

import Criterion.Main

import Benchmarks.Data.Objects
import Benchmarks.Data.Schemas

benchmarks :: Benchmark
benchmarks =
  bgroup
    "Show instance"
    [ byKeys
    , byNestedKeys
    ]
  where
    byKeys =
      bgroup
        "# of keys"
        [ bench "1" $ nf show (schemaObject @Schema1)
        , bench "5" $ nf show (schemaObject @Schema5)
        , bench "10" $ nf show (schemaObject @Schema10)
        , bench "100" $ nf show (schemaObject @Schema100)
        ]

    byNestedKeys =
      bgroup
        "# of nested keys"
        [ bench "1" $ nf show (schemaObject @SchemaNest1)
        , bench "5" $ nf show (schemaObject @SchemaNest5)
        , bench "10" $ nf show (schemaObject @SchemaNest10)
        , bench "100" $ nf show (schemaObject @SchemaNest100)
        ]
