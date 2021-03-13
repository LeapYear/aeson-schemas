{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Data.Aeson.Schema.TH.Get
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'get' quasiquoter.
-}
module Data.Aeson.Schema.TH.Get where

import Control.Monad ((>=>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Data.Aeson.Schema.Internal (getKey)
import Data.Aeson.Schema.TH.Parse (
  GetterExp (..),
  GetterOperation (..),
  GetterOps,
  parseGetterExp,
 )
import Data.Aeson.Schema.Utils.Sum (fromSumType)

{- | Defines a QuasiQuoter for extracting JSON data.

 Example:

 > let Just result = decode ... :: Maybe (Object MySchema)
 >
 > [get| result.foo.a |]          :: Int
 > [get| result.foo.nodes |]      :: [Object (..)]
 > [get| result.foo.nodes[] |]    :: [Object (..)]
 > [get| result.foo.nodes[].b |]  :: [Maybe Bool]
 > [get| result.foo.nodes[].b! |] :: [Bool] -- runtime error if any values are Nothing
 > [get| result.foo.c |]          :: Text
 > [get| result.foo.(a,c) |]      :: (Int, Text)
 > [get| result.foo.[c,d] |]      :: [Text]
 >
 > let nodes = [get| result.foo.nodes |]
 > flip map nodes $ \node -> fromMaybe ([get| node.num |] == 0) [get| node.b |]
 > map [get| .num |] nodes

 Syntax:

 * @x.y@ is only valid if @x@ is an 'Data.Aeson.Schema.Object'. Returns the value of the key @y@.

 * @.y@ returns a function that takes in an 'Data.Aeson.Schema.Object' and returns the value of
   the key @y@.

 * @x.[y,z.a]@ is only valid if @x@ is an 'Data.Aeson.Schema.Object', and if @y@ and @z.a@ have
   the same type. Returns the value of the operations @y@ and @z.a@ as a list.
   MUST be the last operation.

 * @x.(y,z.a)@ is only valid if @x@ is an 'Data.Aeson.Schema.Object'. Returns the value of the
   operations @y@ and @z.a@ as a tuple.
   MUST be the last operation.

 * @x!@ is only valid if @x@ is a 'Maybe'. Unwraps the value of @x@ from a 'Just' value and
   errors (at runtime!) if @x@ is 'Nothing'.

 * @x[]@ is only valid if @x@ is a list. Applies the remaining rules as an 'fmap' over the
   values in the list, e.g.

     * @x[]@ without anything after is equivalent to @x@
     * @x[].y@ gets the key @y@ in all the Objects in @x@
     * @x[]!@ unwraps all 'Just' values in @x@ (and errors if any 'Nothing' values exist in @x@)

 * @x?@ follows the same rules as @x[]@ except it's only valid if @x@ is a 'Maybe'.

 * @x\@#@ is only valid if @x@ is a 'SumType'. If the sum type contains a value at the given
   branch (e.g. @x\@0@ for @Here v@), return 'Just' that value, otherwise 'Nothing'. (added in
   v1.1.0)

   e.g. with the schema @{ a: Int | Bool }@, calling @[get| .a\@0 |]@ will return @Maybe Int@ if
   the sum type contains an 'Int'.
-}
get :: QuasiQuoter
get =
  QuasiQuoter
    { quoteExp = parseGetterExp >=> generateGetterExp
    , quoteDec = error "Cannot use `get` for Dec"
    , quoteType = error "Cannot use `get` for Type"
    , quotePat = error "Cannot use `get` for Pat"
    }

generateGetterExp :: GetterExp -> ExpQ
generateGetterExp GetterExp{..} = applyStart $ resolveGetterOpExps $ mkGetterOpExps [] getterOps
  where
    applyStart expr = maybe expr (appE expr . varE . mkName) start

    startDisplay = case start of
      Nothing -> ""
      Just s -> if '.' `elem` s then "(" ++ s ++ ")" else s

    mkGetterOpExps :: [GetterOperation] -> GetterOps -> GetterOpExps
    mkGetterOpExps historyPrefix = mapWithHistory (mkGetterOpExp . (historyPrefix ++))

    mkGetterOpExp :: [GetterOperation] -> GetterOperation -> GetterOpExp
    mkGetterOpExp history = \case
      GetterKey key ->
        let keyType = litT $ strTyLit key
         in ApplyOp [|getKey (Proxy :: Proxy $keyType)|]
      GetterBang ->
        let expr = startDisplay ++ showGetterOps history
         in ApplyOp [|fromJust expr|]
      GetterMapMaybe ->
        ApplyOpInfix [|(<$?>)|]
      GetterMapList ->
        ApplyOpInfix [|(<$:>)|]
      GetterBranch branch ->
        let branchType = litT $ numTyLit $ fromIntegral branch
         in ApplyOp [|fromSumType (Proxy :: Proxy $branchType)|]
      GetterList elemOps ->
        ApplyOpsIntoList $ mkGetterOpExps history <$> elemOps
      GetterTuple elemOps ->
        ApplyOpsIntoTuple $ mkGetterOpExps history <$> elemOps

{- Runtime helpers -}

-- | fromJust with helpful error message
fromJust :: HasCallStack => String -> Maybe a -> a
fromJust expr = Maybe.fromMaybe (error errMsg)
  where
    errMsg = "Called 'fromJust' on null expression" ++ if null expr then "" else ": " ++ expr

-- | fmap specialized to Maybe
(<$?>) :: (a -> b) -> Maybe a -> Maybe b
(<$?>) = (<$>)

-- | fmap specialized to [a]
(<$:>) :: (a -> b) -> [a] -> [b]
(<$:>) = (<$>)

{- Code generation helpers -}

data GetterOpExp
  = -- | next . f
    ApplyOp ExpQ
  | -- | (next `f`)
    ApplyOpInfix ExpQ
  | -- | \v -> [f1 v, f2 v, ...]
    ApplyOpsIntoList (NonEmpty GetterOpExps)
  | -- | \v -> (f1 v, f2 v, ...)
    ApplyOpsIntoTuple (NonEmpty GetterOpExps)

type GetterOpExps = NonEmpty GetterOpExp

resolveGetterOpExps :: GetterOpExps -> ExpQ
resolveGetterOpExps (op NonEmpty.:| ops) =
  case op of
    ApplyOp f -> [|$next . $f|]
    ApplyOpInfix f -> infixE (Just next) f Nothing
    -- suffixes; ops should be empty
    ApplyOpsIntoList elemOps -> resolveEach listE elemOps
    ApplyOpsIntoTuple elemOps -> resolveEach tupE elemOps
  where
    next = maybe [|id|] resolveGetterOpExps $ NonEmpty.nonEmpty ops

    resolveEach fromElems elemOps = do
      val <- newName "v"
      let applyVal expr = appE expr (varE val)
      lamE [varP val] $ fromElems $ map (applyVal . resolveGetterOpExps) $ NonEmpty.toList elemOps

showGetterOps :: Foldable t => t GetterOperation -> String
showGetterOps = concatMap showGetterOp
  where
    showGetterOp = \case
      GetterKey key -> '.' : key
      GetterBang -> "!"
      GetterMapList -> "[]"
      GetterMapMaybe -> "?"
      GetterBranch x -> '@' : show x
      GetterList elemOps -> ".[" ++ showGetterOpsList elemOps ++ "]"
      GetterTuple elemOps -> ".(" ++ showGetterOpsList elemOps ++ ")"

    showGetterOpsList = intercalate "," . NonEmpty.toList . fmap showGetterOps

{- Utilities -}

{- | Run the given function for each element in the list, providing all elements seen so far.

 e.g. for a list [1,2,3], this will return the result of

   [f [] 1, f [1] 2, f [1,2] 3]
-}
mapWithHistory :: ([a] -> a -> b) -> NonEmpty a -> NonEmpty b
mapWithHistory f xs = NonEmpty.zipWith f (NonEmpty.inits xs) xs
