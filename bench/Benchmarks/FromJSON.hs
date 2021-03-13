{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Benchmarks.FromJSON where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import qualified Data.Aeson as Aeson

import Data.Aeson.Schema (IsSchema, Object)

import Benchmarks.Data.Objects
import Benchmarks.Data.Schemas

benchmarks :: Benchmark
benchmarks =
  bgroup
    "FromJSON instance"
    [ byKeys
    , byNestedKeys
    ]
  where
    byKeys =
      bgroup
        "# of keys"
        [ bench "1" $ nf (Aeson.fromJSON @(Object Schema1)) (schemaValue @Schema1)
        , bench "5" $ nf (Aeson.fromJSON @(Object Schema5)) (schemaValue @Schema5)
        , bench "10" $ nf (Aeson.fromJSON @(Object Schema10)) (schemaValue @Schema10)
        , bench "100" $ nf (Aeson.fromJSON @(Object Schema100)) (schemaValue @Schema100)
        ]

    byNestedKeys =
      bgroup
        "# of nested keys"
        [ bench "1" $ nf (Aeson.fromJSON @(Object SchemaNest1)) (schemaValue @SchemaNest1)
        , bench "5" $ nf (Aeson.fromJSON @(Object SchemaNest5)) (schemaValue @SchemaNest5)
        , bench "10" $ nf (Aeson.fromJSON @(Object SchemaNest10)) (schemaValue @SchemaNest10)
        , bench "100" $ nf (Aeson.fromJSON @(Object SchemaNest100)) (schemaValue @SchemaNest100)
        ]

{- Orphans -}

instance IsSchema schema => NFData (Object schema) where
  rnf = rnf . show
