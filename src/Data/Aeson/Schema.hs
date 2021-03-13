{- |
Module      :  Data.Aeson.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines a new way of parsing JSON data by defining type-level schemas and
extracting information using quasiquoters that will check if a given query path is
valid at compile-time.
-}
module Data.Aeson.Schema (
  -- * Object
  Object,
  toMap,

  -- * Schemas
  Schema,
  IsSchema,
  showSchema,

  -- * Quasiquoters for extracting or manipulating JSON data or schemas
  schema,
  get,
  unwrap,
  mkGetter,
) where

import Data.Aeson.Schema.Internal
import Data.Aeson.Schema.TH
import Data.Aeson.Schema.Type
