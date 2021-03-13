{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Data.Aeson.Schema.TH.Unwrap
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'unwrap' quasiquoter.
-}
module Data.Aeson.Schema.TH.Unwrap where

import Control.Monad ((<=<), (>=>))
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Data.Aeson.Schema.Internal (Object, SchemaResult)
import Data.Aeson.Schema.Key (fromSchemaKeyV)
import Data.Aeson.Schema.TH.Parse (
  GetterOperation (..),
  GetterOps,
  UnwrapSchema (..),
  parseUnwrapSchema,
 )
import Data.Aeson.Schema.TH.Utils (
  reifySchema,
  resolveSchemaType,
  schemaTypeVToTypeQ,
  schemaVToTypeQ,
 )
import Data.Aeson.Schema.Type (
  Schema' (..),
  SchemaType' (..),
  SchemaTypeV,
  SchemaV,
  showSchemaTypeV,
  toSchemaObjectV,
 )

{- | Defines a QuasiQuoter to extract a schema within the given schema.

 The base schema needs to be defined in a separate module.

 For example:

 > -- | MyFoo ~ Object [schema| { b: Maybe Bool } |]
 > type MyFoo = [unwrap| MySchema.foo.nodes[] |]

 If the schema is imported qualified, you can use parentheses to distinguish it from the
 expression:

 > type MyFoo = [unwrap| (MyModule.Schema).foo.nodes[] |]

 You can then use the type alias as usual:

 > parseBar :: MyFoo -> String
 > parseBar = maybe "null" show . [get| .b |]
 >
 > foo = map parseBar [get| result.foo.nodes[] |]

 The syntax is mostly the same as 'Data.Aeson.Schema.TH.get', except the operations run on the
 type itself, instead of the values. Differences from 'Data.Aeson.Schema.TH.get':

 * @x!@ is only valid if @x@ is a @Maybe a@ type. Returns @a@, the type wrapped in the 'Maybe'.

 * @x?@ is the same as @x!@.

 * @x[]@ is only valid if @x@ is a @[a]@ type. Returns @a@, the type contained in the list.

 * @x\@#@ is only valid if @x@ is a @SumType@. Returns the type at that branch in the sum type.
-}
unwrap :: QuasiQuoter
unwrap =
  QuasiQuoter
    { quoteExp = error "Cannot use `unwrap` for Exp"
    , quoteDec = error "Cannot use `unwrap` for Dec"
    , quoteType = parseUnwrapSchema >=> generateUnwrapSchema
    , quotePat = error "Cannot use `unwrap` for Pat"
    }

generateUnwrapSchema :: UnwrapSchema -> TypeQ
generateUnwrapSchema UnwrapSchema{..} = reifySchema startSchema >>= unwrapSchema getterOps

-- | Unwrap the given schema by applying the given operations, stripping out functors.
unwrapSchema :: GetterOps -> SchemaV -> TypeQ
unwrapSchema = unwrapSchemaUsing StripFunctors

-- | Unwrap the given schema by applying the given operations, using the given 'FunctorHandler'.
unwrapSchemaUsing :: FunctorHandler -> GetterOps -> SchemaV -> TypeQ
unwrapSchemaUsing functorHandler getterOps = toResultTypeQ <=< flip go (NonEmpty.toList getterOps) . toSchemaObjectV
  where
    toResultTypeQ :: UnwrapSchemaResult -> TypeQ
    toResultTypeQ = \case
      -- special case SchemaObject to make it further inspectable
      SchemaResult (SchemaObject pairs) -> [t|Object $(schemaVToTypeQ (Schema pairs))|]
      SchemaResult schemaType -> [t|SchemaResult $(schemaTypeVToTypeQ schemaType)|]
      SchemaResultList schemaResult -> appT listT (toResultTypeQ schemaResult)
      SchemaResultTuple schemaResults -> foldl appT (tupleT $ length schemaResults) $ map toResultTypeQ schemaResults
      SchemaResultWrapped functorTy schemaResult ->
        let handleFunctor ty =
              case functorHandler of
                ApplyFunctors -> AppT functorTy ty
                StripFunctors -> ty
         in handleFunctor <$> toResultTypeQ schemaResult

    go :: SchemaTypeV -> [GetterOperation] -> Q UnwrapSchemaResult
    go schemaType [] = pure $ SchemaResult schemaType
    go schemaType' (op : ops) = do
      schemaType <- resolveSchemaType schemaType'

      let invalid message = fail $ message ++ ": " ++ showSchemaTypeV schemaType
          wrapMaybe = SchemaResultWrapped (ConT ''Maybe)
          wrapList = SchemaResultWrapped ListT

      case op of
        GetterKey key ->
          case schemaType of
            SchemaObject pairs ->
              case lookup key $ map (first fromSchemaKeyV) pairs of
                Just inner -> go inner ops
                Nothing -> invalid $ "Key '" ++ key ++ "' does not exist in schema"
            _ -> invalid $ "Cannot get key '" ++ key ++ "' in schema"
        GetterBang ->
          case schemaType of
            SchemaMaybe inner -> go inner ops
            SchemaTry inner -> go inner ops
            _ -> invalid "Cannot use `!` operator on schema"
        GetterMapMaybe ->
          case schemaType of
            SchemaMaybe inner -> wrapMaybe <$> go inner ops
            SchemaTry inner -> wrapMaybe <$> go inner ops
            _ -> invalid "Cannot use `?` operator on schema"
        GetterMapList ->
          case schemaType of
            SchemaList inner -> wrapList <$> go inner ops
            _ -> invalid "Cannot use `[]` operator on schema"
        GetterBranch branch ->
          case schemaType of
            SchemaUnion schemas ->
              if branch < length schemas
                then go (schemas !! branch) ops
                else invalid "Branch out of bounds for schema"
            _ -> invalid "Cannot use `@` operator on schema"
        -- suffixes; ops should be empty

        GetterList elemOps ->
          case schemaType of
            SchemaObject _ -> do
              elemSchemas <- traverse (go schemaType . NonEmpty.toList) elemOps
              let elemSchema = NonEmpty.head elemSchemas
              if all (== elemSchema) elemSchemas
                then pure $ SchemaResultList elemSchema
                else invalid "List contains different types in schema"
            _ -> invalid "Cannot get keys in schema"
        GetterTuple elemOps ->
          case schemaType of
            SchemaObject _ -> SchemaResultTuple <$> mapM (go schemaType . NonEmpty.toList) (NonEmpty.toList elemOps)
            _ -> invalid "Cannot get keys in schema"

data UnwrapSchemaResult
  = SchemaResult SchemaTypeV
  | SchemaResultList UnwrapSchemaResult
  | SchemaResultTuple [UnwrapSchemaResult]
  | -- | Type should be of kind `* -> *`
    SchemaResultWrapped Type UnwrapSchemaResult
  deriving (Eq)

-- | A data type that indicates how to handle functors when unwrapping a schema.
data FunctorHandler
  = -- | handleFunctor Maybe Int ==> Maybe Int
    ApplyFunctors
  | -- | handleFunctor Maybe Int ==> Int
    StripFunctors
