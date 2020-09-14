{-# LANGUAGE LambdaCase #-}

module Schemas.TH
  ( SchemaDef(..)
  , genSchema
  , genSchema'
  , genSchemaDef
  , keysTo
  , mkField
  ) where

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Aeson.Schema (schema)

data SchemaDef
  = Field String String   -- ^ { a: Int }
  | Include String String -- ^ { a: #OtherSchema }
  | Ref String            -- ^ { #OtherSchema }

genSchema :: Name -> [SchemaDef] -> DecQ
genSchema name = genSchema' name . genSchemaDef

genSchema' :: Name -> String -> DecQ
genSchema' name = tySynD name [] . quoteType schema

genSchemaDef :: [SchemaDef] -> String
genSchemaDef schemaDef = "{" ++ intercalate "," (map fromSchemaDef schemaDef) ++ "}"
  where
    fromSchemaDef = \case
      Field key ty -> key ++ ": " ++ ty
      Include key name -> key ++ ": #" ++ name
      Ref name -> "#" ++ name

keysTo :: Int -> [SchemaDef]
keysTo n = map (\i -> Field (mkField i) "Int") [1..n]

mkField :: Int -> String
mkField i = "a" ++ show i
