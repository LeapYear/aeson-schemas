{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      :  Data.Aeson.Schema.TH.Utils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable
-}
module Data.Aeson.Schema.TH.Utils (
  reifySchema,
  lookupSchema,
  loadSchema,
  resolveSchemaType,
  schemaVToTypeQ,
  schemaTypeVToTypeQ,
) where

import Control.Applicative (empty)
import Control.Monad (forM)
import Data.Bifunctor (bimap)
import Language.Haskell.TH

import Data.Aeson.Schema.Internal (Object)
import Data.Aeson.Schema.Key (SchemaKey' (..), SchemaKeyV)
import Data.Aeson.Schema.Type (
  Schema' (..),
  SchemaObjectMapV,
  SchemaType' (..),
  SchemaTypeV,
  SchemaV,
  fromSchemaV,
  toSchemaObjectV,
 )
import Data.Aeson.Schema.Utils.Invariant (unreachable)
import Data.Aeson.Schema.Utils.NameLike (NameLike (..), resolveName)

{- Loading schema from TH -}

reifySchema :: String -> Q SchemaV
reifySchema name = lookupSchema (NameRef name) >>= loadSchema

data ReifiedSchema = ReifiedSchema
  { reifiedSchemaName :: Name
  , reifiedSchemaType :: TypeWithoutKinds
  }

{- | Look up a schema with the given name. Errors if the name doesn't exist or if the name does
 not refer to a schema.
-}
lookupSchema :: NameLike -> Q ReifiedSchema
lookupSchema nameLike = do
  name <- lookupSchemaName nameLike
  ReifiedSchema name <$> reifySchemaType name
  where
    lookupSchemaName = \case
      NameRef name -> lookupTypeName name >>= maybe (fail $ "Unknown schema: " ++ name) return
      NameTH name -> return name

    reifySchemaType :: Name -> Q TypeWithoutKinds
    reifySchemaType schemaName =
      reify schemaName >>= \case
        TyConI (TySynD _ _ (stripKinds -> ty))
          -- `type MySchema = 'Schema '[ ... ]`
          | isPromotedSchema ty ->
            return ty
          -- `type MySchema = Object ('Schema '[ ... ])`
          | Just inner <- unwrapObject ty
            , isPromotedSchema inner ->
            return inner
          -- `type MySchema = Object OtherSchema`
          | Just (ConT schemaName') <- unwrapObject ty ->
            reifySchemaType schemaName'
        _ -> fail $ "'" ++ show schemaName ++ "' is not a Schema"

    -- If the given type is of the format `Object a`, return `a`.
    unwrapObject :: TypeWithoutKinds -> Maybe TypeWithoutKinds
    unwrapObject = \case
      AppT (ConT name) inner | name == ''Object -> Just inner
      _ -> Nothing

    -- Return True if the given type is of the format: 'Schema '[ ... ]
    isPromotedSchema :: TypeWithoutKinds -> Bool
    isPromotedSchema = \case
      AppT (PromotedT name) _ | name == 'Schema -> True
      _ -> False

loadSchema :: ReifiedSchema -> Q SchemaV
loadSchema ReifiedSchema{reifiedSchemaType} =
  maybe (fail $ "Could not parse schema: " ++ show reifiedSchemaType) return $ parseSchema reifiedSchemaType
  where
    -- should be the inverse of schemaVToTypeQ
    parseSchema :: TypeWithoutKinds -> Maybe SchemaV
    parseSchema ty = do
      schemaObjectType <- case ty of
        AppT (PromotedT name) schemaType | name == 'Schema -> return schemaType
        _ -> empty

      Schema <$> parseSchemaObjectMap schemaObjectType

    -- should be the inverse of schemaObjectMapVToTypeQ
    parseSchemaObjectMap :: TypeWithoutKinds -> Maybe SchemaObjectMapV
    parseSchemaObjectMap schemaObjectType = do
      schemaObjectListOfPairs <- mapM typeToPair =<< typeToList schemaObjectType
      forM schemaObjectListOfPairs $ \(schemaKeyType, schemaTypeType) -> do
        schemaKey <- parseSchemaKey schemaKeyType
        schemaType <- parseSchemaType schemaTypeType
        return (schemaKey, schemaType)

    -- should be the inverse of schemaKeyVToTypeQ
    parseSchemaKey :: TypeWithoutKinds -> Maybe SchemaKeyV
    parseSchemaKey = \case
      AppT (PromotedT ty) (LitT (StrTyLit key))
        | ty == 'NormalKey -> return $ NormalKey key
        | ty == 'PhantomKey -> return $ PhantomKey key
      _ -> empty

    -- should be the inverse of schemaTypeVToTypeQ
    parseSchemaType :: TypeWithoutKinds -> Maybe SchemaTypeV
    parseSchemaType = \case
      AppT (PromotedT name) (ConT inner)
        | name == 'SchemaScalar -> return $ SchemaScalar $ NameTH inner
      AppT (PromotedT name) inner
        | name == 'SchemaMaybe -> SchemaMaybe <$> parseSchemaType inner
        | name == 'SchemaTry -> SchemaTry <$> parseSchemaType inner
        | name == 'SchemaList -> SchemaList <$> parseSchemaType inner
        | name == 'SchemaUnion -> do
          schemas <- typeToList inner
          SchemaUnion <$> mapM parseSchemaType schemas
        | name == 'SchemaObject -> SchemaObject <$> parseSchemaObjectMap inner
      AppT (PromotedT name) (AppT (PromotedT right) (ConT inner))
        | name == 'SchemaInclude
          , right == 'Right ->
          return $ SchemaInclude $ Left $ NameTH inner
      _ -> empty

-- | Resolve SchemaInclude, if present. (Not recursive)
resolveSchemaType :: SchemaTypeV -> Q SchemaTypeV
resolveSchemaType = \case
  SchemaInclude (Left name) -> fmap toSchemaObjectV . loadSchema =<< lookupSchema name
  SchemaInclude (Right _) -> unreachable "Found 'SchemaInclude Right' when resolving schema type"
  schemaType -> pure schemaType

{- Splicing schema into TH -}

schemaVToTypeQ :: SchemaV -> TypeQ
schemaVToTypeQ = appT [t| 'Schema|] . schemaObjectMapVToTypeQ . fromSchemaV

schemaObjectMapVToTypeQ :: SchemaObjectMapV -> TypeQ
schemaObjectMapVToTypeQ = promotedListT . map schemaObjectPairVToTypeQ
  where
    schemaObjectPairVToTypeQ :: (SchemaKeyV, SchemaTypeV) -> TypeQ
    schemaObjectPairVToTypeQ = promotedPairT . bimap schemaKeyVToTypeQ schemaTypeVToTypeQ

    schemaKeyVToTypeQ :: SchemaKeyV -> TypeQ
    schemaKeyVToTypeQ = \case
      NormalKey key -> [t| 'NormalKey $(litT $ strTyLit key)|]
      PhantomKey key -> [t| 'PhantomKey $(litT $ strTyLit key)|]

schemaTypeVToTypeQ :: SchemaTypeV -> TypeQ
schemaTypeVToTypeQ = \case
  SchemaScalar name -> [t| 'SchemaScalar $(resolveName name >>= conT)|]
  SchemaMaybe inner -> [t| 'SchemaMaybe $(schemaTypeVToTypeQ inner)|]
  SchemaTry inner -> [t| 'SchemaTry $(schemaTypeVToTypeQ inner)|]
  SchemaList inner -> [t| 'SchemaList $(schemaTypeVToTypeQ inner)|]
  SchemaUnion schemas -> [t| 'SchemaUnion $(promotedListT $ map schemaTypeVToTypeQ schemas)|]
  SchemaObject pairs -> [t| 'SchemaObject $(schemaObjectMapVToTypeQ pairs)|]
  SchemaInclude (Left name) -> [t| 'SchemaInclude ( 'Right $(conT . reifiedSchemaName =<< lookupSchema name))|]
  SchemaInclude (Right _) -> unreachable "Found 'SchemaInclude Right' when converting to TypeQ"

{- TH utilities -}

{- | Same as 'Type' except without any kind signatures or applications at any depth.

 Provides no actual guarantees. The caller is responsible for making sure the value
 has been run through 'stripKinds' at one point.
-}
type TypeWithoutKinds = Type

-- | Recursively strip all kind signatures and applications.
stripKinds :: Type -> TypeWithoutKinds
stripKinds ty =
  case ty of
    -- cases that strip + recurse
    SigT ty1 _ -> stripKinds ty1
#if MIN_VERSION_template_haskell(2,15,0)
    AppKindT ty1 _ -> stripKinds ty1
#endif

    -- cases that recurse
    ForallT tyVars ctx ty1 -> ForallT tyVars ctx (stripKinds ty1)
#if MIN_VERSION_template_haskell(2,16,0)
    ForallVisT tyVars ty1 -> ForallVisT tyVars (stripKinds ty1)
#endif
    AppT ty1 ty2 -> AppT (stripKinds ty1) (stripKinds ty2)
    InfixT ty1 name ty2 -> InfixT (stripKinds ty1) name (stripKinds ty2)
    UInfixT ty1 name ty2 -> UInfixT (stripKinds ty1) name (stripKinds ty2)
    ParensT ty1 -> ParensT (stripKinds ty1)
#if MIN_VERSION_template_haskell(2,15,0)
    ImplicitParamT str ty1 -> ImplicitParamT str (stripKinds ty1)
#endif

    -- base cases
    VarT _ -> ty
    ConT _ -> ty
    PromotedT _ -> ty
    TupleT _ -> ty
    UnboxedTupleT _ -> ty
    UnboxedSumT _ -> ty
    ArrowT -> ty
    EqualityT -> ty
    ListT -> ty
    PromotedTupleT _ -> ty
    PromotedNilT -> ty
    PromotedConsT -> ty
    StarT -> ty
    ConstraintT -> ty
    LitT _ -> ty
    WildCardT -> ty

typeToList :: TypeWithoutKinds -> Maybe [TypeWithoutKinds]
typeToList = \case
  PromotedNilT -> Just []
  AppT (AppT PromotedConsT x) xs -> (x :) <$> typeToList xs
  _ -> Nothing

typeToPair :: TypeWithoutKinds -> Maybe (TypeWithoutKinds, TypeWithoutKinds)
typeToPair = \case
  AppT (AppT (PromotedTupleT 2) a) b -> Just (a, b)
  _ -> Nothing

promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr consT promotedNilT
  where
    -- nb. https://stackoverflow.com/a/34457936
    consT x xs = appT (appT promotedConsT x) xs

promotedPairT :: (TypeQ, TypeQ) -> TypeQ
promotedPairT (a, b) = [t|'($a, $b)|]
