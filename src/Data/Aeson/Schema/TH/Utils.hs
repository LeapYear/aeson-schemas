{-|
Module      :  Data.Aeson.Schema.TH.Utils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Utils where

import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first, second)
import Data.List (intercalate)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)

import Data.Aeson.Schema.Internal
    (Object, Schema(..), SchemaResult, SchemaType(..))
import qualified Data.Aeson.Schema.Internal as Internal
import Data.Aeson.Schema.Key (SchemaKey(..), fromSchemaKey)
import qualified Data.Aeson.Schema.Show as SchemaShow

-- | Show the given schema as a type.
showSchemaType :: HasCallStack => Type -> String
showSchemaType = SchemaShow.showSchemaType . parseSchemaType

parseSchemaType :: HasCallStack => Type -> SchemaShow.SchemaType
parseSchemaType = \case
  PromotedT name
    | name == 'SchemaBool -> SchemaShow.SchemaBool
    | name == 'SchemaInt -> SchemaShow.SchemaInt
    | name == 'SchemaDouble -> SchemaShow.SchemaDouble
    | name == 'SchemaText -> SchemaShow.SchemaText
  AppT (PromotedT name) (ConT inner)
    | name == 'SchemaCustom -> SchemaShow.SchemaCustom $ nameBase inner
  AppT (PromotedT name) inner
    | name == 'SchemaMaybe -> SchemaShow.SchemaMaybe $ parseSchemaType inner
    | name == 'SchemaTry -> SchemaShow.SchemaTry $ parseSchemaType inner
    | name == 'SchemaList -> SchemaShow.SchemaList $ parseSchemaType inner
    | name == 'SchemaObject -> SchemaShow.SchemaObject $ fromPairs inner
    | name == 'SchemaUnion -> SchemaShow.SchemaUnion $ map parseSchemaType $ typeToList inner
  ty -> error $ "Unknown type: " ++ show ty
  where
    fromPairs pairs = map (second parseSchemaType) $ typeToSchemaPairs pairs

typeToList :: HasCallStack => Type -> [Type]
typeToList = \case
  PromotedNilT -> []
  AppT (AppT PromotedConsT x) xs -> x : typeToList xs
  SigT ty _ -> typeToList ty
  ty -> error $ "Not a type-level list: " ++ show ty

typeToPair :: HasCallStack => Type -> (Type, Type)
typeToPair = \case
  AppT (AppT (PromotedTupleT 2) a) b -> (a, b)
  SigT ty _ -> typeToPair ty
  ty -> error $ "Not a type-level pair: " ++ show ty

typeToSchemaPairs :: HasCallStack => Type -> [(SchemaKey, Type)]
typeToSchemaPairs = map (bimap parseSchemaKey stripSigs . typeToPair) . typeToList

typeQListToTypeQ :: [TypeQ] -> TypeQ
typeQListToTypeQ = foldr consT promotedNilT
  where
    -- nb. https://stackoverflow.com/a/34457936
    consT x xs = appT (appT promotedConsT x) xs

schemaPairsToTypeQ :: [(SchemaKey, TypeQ)] -> TypeQ
schemaPairsToTypeQ = typeQListToTypeQ . map pairT
  where
    pairT (k, v) =
      let schemaKey = case k of
            NormalKey key -> [t| 'Internal.NormalKey $(litT $ strTyLit key) |]
            PhantomKey key -> [t| 'Internal.PhantomKey $(litT $ strTyLit key) |]
      in [t| '($schemaKey, $v) |]

parseSchemaKey :: HasCallStack => Type -> SchemaKey
parseSchemaKey = \case
  AppT (PromotedT ty) (LitT (StrTyLit key))
    | ty == 'Internal.NormalKey -> NormalKey key
    | ty == 'Internal.PhantomKey -> PhantomKey key
  SigT ty _ -> parseSchemaKey ty
  ty -> error $ "Could not parse a schema key: " ++ show ty

-- | Strip all kind signatures from the given type.
stripSigs :: Type -> Type
stripSigs = \case
  ForallT tyVars ctx ty -> ForallT tyVars ctx (stripSigs ty)
  AppT ty1 ty2 -> AppT (stripSigs ty1) (stripSigs ty2)
  SigT ty _ -> stripSigs ty
  InfixT ty1 name ty2 -> InfixT (stripSigs ty1) name (stripSigs ty2)
  UInfixT ty1 name ty2 -> UInfixT (stripSigs ty1) name (stripSigs ty2)
  ParensT ty -> ParensT (stripSigs ty)
  ty -> ty

reifySchema :: Name -> TypeQ
reifySchema = reify >=> \case
  TyConI (TySynD _ _ ty) -> pure $ stripSigs ty
  info -> fail $ "Unknown reified schema: " ++ show info

-- | Unwrap the given type using the given getter operations.
--
-- Accepts Bool for whether to maintain functor structure (True) or strip away functor applications
-- (False).
unwrapType :: Bool -> GetterOps -> Type -> TypeQ
unwrapType _ [] = fromSchemaType
  where
    fromSchemaType schema = case schema of
      AppT (PromotedT ty) inner
        -- TODO: factor out
        | ty == 'Schema -> fromSchemaType (AppT (PromotedT 'SchemaObject) inner)
        | ty == 'SchemaCustom -> [t| SchemaResult $(pure schema) |]
        | ty == 'SchemaMaybe -> [t| Maybe $(fromSchemaType inner) |]
        | ty == 'SchemaTry -> [t| Maybe $(fromSchemaType inner) |]
        | ty == 'SchemaList -> [t| [$(fromSchemaType inner)] |]
        | ty == 'SchemaObject -> [t| Object ('Schema $(pure inner)) |]
        | ty == 'SchemaUnion -> [t| SchemaResult $(pure schema) |]
      PromotedT ty
        | ty == 'SchemaBool -> [t| Bool |]
        | ty == 'SchemaInt -> [t| Int |]
        | ty == 'SchemaDouble -> [t| Double |]
        | ty == 'SchemaText -> [t| Text |]
      AppT t1 t2 -> appT (fromSchemaType t1) (fromSchemaType t2)
      TupleT _ -> pure schema
      _ -> fail $ "Could not convert schema: " ++ showSchemaType schema
unwrapType keepFunctor (op:ops) = \case
  -- TODO: factor out
  AppT (PromotedT ty) inner | ty == 'Schema -> unwrapType' (op:ops) (AppT (PromotedT 'SchemaObject) inner)
  schema@(AppT (PromotedT ty) inner) ->
    case op of
      GetterKey key | ty == 'SchemaObject ->
        case lookup key (getObjectSchema inner) of
          Just schema' -> unwrapType' ops schema'
          Nothing -> fail $ "Key '" ++ key ++ "' does not exist in schema: " ++ showSchemaType schema
      GetterKey key -> fail $ "Cannot get key '" ++ key ++ "' in schema: " ++ showSchemaType schema
      GetterList elems | ty == 'SchemaObject -> do
        (elem':rest) <- mapM (`unwrapType'` schema) elems
        if all (== elem') rest
          then unwrapType' ops elem'
          else fail $ "List contains different types with schema: " ++ showSchemaType schema
      GetterList _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
      GetterTuple elems | ty == 'SchemaObject ->
        foldl appT (tupleT $ length elems) $ map (`unwrapType'` schema) elems
      GetterTuple _ -> fail $ "Cannot get keys in schema: " ++ showSchemaType schema
      GetterBang | ty == 'SchemaMaybe -> unwrapType' ops inner
      GetterBang | ty == 'SchemaTry -> unwrapType' ops inner
      GetterBang -> fail $ "Cannot use `!` operator on schema: " ++ showSchemaType schema
      GetterMapMaybe | ty == 'SchemaMaybe -> withFunctor [t| Maybe |] $ unwrapType' ops inner
      GetterMapMaybe | ty == 'SchemaTry -> withFunctor [t| Maybe |] $ unwrapType' ops inner
      GetterMapMaybe -> fail $ "Cannot use `?` operator on schema: " ++ showSchemaType schema
      GetterMapList | ty == 'SchemaList -> withFunctor (pure ListT) $ unwrapType' ops inner
      GetterMapList -> fail $ "Cannot use `[]` operator on schema: " ++ showSchemaType schema
      GetterBranch branch | ty == 'SchemaUnion ->
        let subTypes = typeToList inner
        in if branch >= length subTypes
          then fail $ "Branch out of bounds for schema: " ++ showSchemaType schema
          else unwrapType' ops $ subTypes !! branch
      GetterBranch _ -> fail $ "Cannot use `@` operator on schema: " ++ showSchemaType schema
  -- allow starting from (Object schema)
  AppT (ConT ty) inner | ty == ''Object -> unwrapType' (op:ops) inner
  schema -> fail $ unlines ["Cannot get type:", show schema, show op]
  where
    unwrapType' = unwrapType keepFunctor
    getObjectSchema = map (first getSchemaKey . typeToPair) . typeToList
    getSchemaKey = fromSchemaKey . parseSchemaKey
    withFunctor f = if keepFunctor then appT f else id

{- GetterOps -}

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterList [GetterOps]
  | GetterTuple [GetterOps]
  | GetterBang
  | GetterMapList
  | GetterMapMaybe
  | GetterBranch Int
  deriving (Show,Lift)

showGetterOps :: GetterOps -> String
showGetterOps = concatMap showGetterOp
  where
    showGetterOp = \case
      GetterKey key -> '.':key
      GetterList elems -> ".[" ++ intercalate "," (map showGetterOps elems) ++ "]"
      GetterTuple elems -> ".(" ++ intercalate "," (map showGetterOps elems) ++ ")"
      GetterBang -> "!"
      GetterMapList -> "[]"
      GetterMapMaybe -> "?"
      GetterBranch x -> '@' : show x
