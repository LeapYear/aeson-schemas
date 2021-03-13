{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Data.Aeson.Schema.TH.Getter
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Template Haskell functions for getter functions.
-}
module Data.Aeson.Schema.TH.Getter where

import Control.Monad (unless)
import Data.Aeson.Schema.Internal (Object)
import Data.Maybe (isNothing)
import Language.Haskell.TH

import Data.Aeson.Schema.TH.Get (generateGetterExp)
import Data.Aeson.Schema.TH.Parse (GetterExp (..), parseGetterExp)
import Data.Aeson.Schema.TH.Unwrap (
  FunctorHandler (..),
  unwrapSchema,
  unwrapSchemaUsing,
 )
import Data.Aeson.Schema.TH.Utils (loadSchema, lookupSchema, schemaVToTypeQ)
import Data.Aeson.Schema.Utils.NameLike (NameLike (..))

{- | A helper that generates a 'Data.Aeson.Schema.TH.get' expression and a type alias for the result
 of the expression.

 > mkGetter "Node" "getNodes" ''MySchema ".nodes[]"
 >
 > {\- is equivalent to -\}
 >
 > -- | Node ~ { b: Maybe Bool }
 > type Node = [unwrap| MySchema.nodes[] |]
 >
 > getNodes :: Object MySchema -> [Node]
 > getNodes = [get| .nodes[] |]

 'mkGetter' takes four arguments:

   [@unwrapName@] The name of the type synonym to store the unwrapped schema as

   [@funcName@] The name of the getter function

   [@startSchema@] The schema to extract/unwrap from

   [@ops@] The operation to pass to the 'Data.Aeson.Schema.TH.get' and
           'Data.Aeson.Schema.TH.unwrap' quasiquoters

 There is one subtlety that occurs from the use of the same @ops@ string for both the
 'Data.Aeson.Schema.TH.unwrap' and 'Data.Aeson.Schema.TH.get' quasiquoters:
 'Data.Aeson.Schema.TH.unwrap' strips out intermediate functors, while 'Data.Aeson.Schema.TH.get'
 applies within the functor. So in the above example, @".nodes[]"@ strips out the list when
 saving the schema to @Node@, while in the below example, @".nodes"@ doesn't strip out the list
 when saving the schema to @Nodes@.

 > mkGetter "Nodes" "getNodes" ''MySchema ".nodes"
 >
 > {\- is equivalent to -\}
 >
 > -- | Nodes ~ List { b: Maybe Bool }
 > type Nodes = [unwrap| MySchema.nodes |]
 >
 > getNodes :: Object MySchema -> Nodes
 > getNodes = [get| .nodes |]

 As another example,

 > mkGetter "MyName" "getMyName" ''MySchema ".f?[].name"
 >
 > {\- is equivalent to -\}
 >
 > -- | MyName ~ Text
 > type MyName = [unwrap| MySchema.f?[].name |]
 >
 > getMyBool :: Object MySchema -> Maybe [MyName]
 > getMyBool = [get| .f?[].name |]
-}
mkGetter :: String -> String -> Name -> String -> DecsQ
mkGetter unwrapName funcName startSchemaName ops = do
  getterExp@GetterExp{..} <- parseGetterExp ops
  unless (isNothing start) $
    fail $ "Getter expression should start with '.': " ++ ops

  startSchema <- lookupSchema (NameTH startSchemaName) >>= loadSchema

  let unwrapResult = unwrapSchema getterOps startSchema
      funcResult = unwrapSchemaUsing ApplyFunctors getterOps startSchema
      getterFunc = generateGetterExp getterExp
      unwrapName' = mkName unwrapName
      funcName' = mkName funcName

  sequence
    [ tySynD unwrapName' [] unwrapResult
    , sigD funcName' [t|Object $(schemaVToTypeQ startSchema) -> $funcResult|]
    , funD funcName' [clause [] (normalB getterFunc) []]
    ]
