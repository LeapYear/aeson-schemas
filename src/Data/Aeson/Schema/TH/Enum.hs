{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Data.Aeson.Schema.TH.Enum
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Template Haskell functions for Enum types.
-}
module Data.Aeson.Schema.TH.Enum (
  genFromJSONEnum,
  genToJSONEnum,
  mkEnum,
) where

import Control.Monad (forM, unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

{- | Make an enum type with the given constructors, that can be parsed from JSON.

 The 'FromJSON' instance will match to a string value matching the constructor name,
 case-insensitive.

 @
 mkEnum \"State" [\"OPEN", \"CLOSED"]

 -- generates equivalent of:
 --   data State = OPEN | CLOSED deriving (...)
 --   genFromJSONEnum ''State
 --   genToJSONEnum ''State
 @
-}
mkEnum :: String -> [String] -> Q [Dec]
mkEnum name vals =
  concat
    <$> sequence
      [ (: []) <$> dataDec
      , mkFromJSON name' vals'
      , mkToJSON name' vals'
      ]
  where
    name' = mkName name
    vals' = map mkName vals
    dataDec = dataD (pure []) name' [] Nothing (map toCon vals') [derivClause Nothing deriveClasses]
    deriveClasses =
      [ [t|Eq|]
      , [t|Ord|]
      , [t|Show|]
      , [t|Enum|]
      ]
    toCon val = normalC val []

{- | Generate an instance of 'FromJSON' for the given data type.

 Prefer using 'mkEnum'; this function is useful for data types in which you want greater control
 over the actual data type.

 The 'FromJSON' instance will match to a string value matching the constructor name,
 case-insensitive.

 @
 data State = Open | CLOSED deriving (Show,Enum)
 genFromJSONEnum ''State

 -- outputs:
 --   Just Open
 --   Just Open
 --   Just CLOSED
 --   Just CLOSED
 main = mapM_ print
   [ decodeState \"open"
   , decodeState \"OPEN"
   , decodeState \"closed"
   , decodeState \"CLOSED"
   ]
   where
     decodeState :: String -> Maybe State
     decodeState = decode . show
 @
-}
genFromJSONEnum :: Name -> Q [Dec]
genFromJSONEnum name = getEnumConstructors name >>= mkFromJSON name

{- | Generate an instance of 'ToJSON' for the given data type.

 Prefer using 'mkEnum'; this function is useful for data types in which you want greater control
 over the actual data type.

 The 'ToJSON' instance will encode the enum as a string matching the constructor name.

 @
 data State = Open | CLOSED deriving (Show,Enum)
 genToJSONEnum ''State

 -- outputs:
 --   \"Open"
 --   \"CLOSED"
 main = mapM_ print
   [ encode Open
   , encode CLOSED
   ]
 @
-}
genToJSONEnum :: Name -> Q [Dec]
genToJSONEnum name = getEnumConstructors name >>= mkToJSON name

{- Helpers -}

getEnumConstructors :: Name -> Q [Name]
getEnumConstructors name = do
  -- check if 'name' is an Enum
  ClassI _ instances <- reify ''Enum
  let instanceNames = flip mapMaybe instances $ \case
        InstanceD _ _ (AppT _ (ConT n)) _ -> Just n
        _ -> Nothing
  unless (name `elem` instanceNames) $ fail $ "Not an Enum type: " ++ show name

  -- extract constructor names
  reify name >>= \case
    TyConI (DataD _ _ _ _ cons _) -> forM cons $ \case
      NormalC con [] -> return con
      con -> fail $ "Invalid constructor: " ++ show con
    info -> fail $ "Invalid data type: " ++ show info

mkFromJSON :: Name -> [Name] -> Q [Dec]
mkFromJSON name cons = do
  let toPattern = litP . stringL . map toLower . nameBase
      toMatch con = match (toPattern con) (normalB [|pure $(conE con)|]) []

  t <- newName "t"
  let parseEnum =
        caseE [|Text.unpack $ Text.toLower $(varE t)|] $
          map toMatch cons ++ [match wildP (normalB $ appE badParse $ varE t) []]

  [d|
    instance FromJSON $(conT name) where
      parseJSON (String $(varP t)) = $parseEnum
      parseJSON v = $badParse v
    |]
  where
    badParse =
      let prefix = litE $ stringL $ "Bad " ++ nameBase name ++ ": "
       in [|fail . ($prefix ++) . show|]

mkToJSON :: Name -> [Name] -> Q [Dec]
mkToJSON name cons =
  [d|
    instance ToJSON $(conT name) where
      toJSON = $(lamCaseE $ map encodeConstructor cons)
    |]
  where
    encodeConstructor con = match (conP con []) (normalB [|String $ Text.pack $(lift $ nameBase con)|]) []
