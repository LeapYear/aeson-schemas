{- |
Module      :  Data.Aeson.Schema.TH
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Template Haskell definitions for doing various @aeson-schemas@ operations.

'Data.Aeson.Schema.SchemaType' defines the shape of the JSON object stored in
'Data.Aeson.Schema.Object', and we can use 'Data.Aeson.Schema.Internal.getKey' to lookup a key that
is checked at compile-time to exist in the object.

To make it easier to extract deeply nested keys, this module defines QuasiQuoters that generate the
corresponding 'Data.Aeson.Schema.Internal.getKey' expressions.

In addition to the QuasiQuotes extension, the following extensions will need to be enabled to
use these QuasiQuoters:

* DataKinds
* FlexibleContexts
* TypeFamilies
-}
module Data.Aeson.Schema.TH (
  schema,
  get,
  unwrap,

  -- * Utilities
  mkGetter,

  -- * Helpers for Enum types
  mkEnum,
  genFromJSONEnum,
  genToJSONEnum,
) where

import Data.Aeson.Schema.TH.Enum
import Data.Aeson.Schema.TH.Get
import Data.Aeson.Schema.TH.Getter
import Data.Aeson.Schema.TH.Schema
import Data.Aeson.Schema.TH.Unwrap
