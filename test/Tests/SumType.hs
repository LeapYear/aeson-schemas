{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SumType where

import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Schema.Utils.Sum (SumType (..), fromSumType)

type SpecialJSON = SumType '[Bool, Int, [String]]

toSpecialJSON :: ToJSON a => a -> SpecialJSON
toSpecialJSON = either (error . ("Invalid SpecialJSON: " ++) . show) id . toSpecialJSON'

toSpecialJSON' :: ToJSON a => a -> Either String SpecialJSON
toSpecialJSON' = eitherDecode . encode

{- Tests -}

test :: TestTree
test =
  testGroup
    "SumType"
    [ testCase "Sanity checks" $
        -- this should compile
        let values =
              [ Here True
              , Here False
              , There (Here 1)
              , There (Here 10)
              , There (There (Here []))
              , There (There (Here ["a"]))
              ] ::
                [SpecialJSON]
         in values @?= values
    , testDecode
    , testFromSumType
    ]

testDecode :: TestTree
testDecode =
  testGroup
    "Decode SumType"
    [ testProperty "branch 1" $ \(b :: Bool) ->
        toSpecialJSON' b === Right (Here b)
    , testProperty "branch 2" $ \(x :: Int) ->
        toSpecialJSON' x === Right (There (Here x))
    , testProperty "branch 3" $ \(l :: [String]) ->
        toSpecialJSON' l === Right (There (There (Here l)))
    , goldenVsString "invalid SumType" "test/goldens/sumtype_decode_invalid.golden" $
        case toSpecialJSON' [True] of
          Right v -> error $ "Unexpectedly decoded value: " ++ show v
          Left e -> pure $ fromString e
    ]

testFromSumType :: TestTree
testFromSumType =
  testGroup
    "fromSumType"
    [ testProperty "branch 0 valid" $ \b ->
        fromSumType (Proxy @0) (toSpecialJSON b) === Just b
    , testProperty "branch 0 invalid" $
        forAll (specialJSONExcept 0) $ \(branch, value) ->
          labelBranch branch $ fromSumType (Proxy @0) value === Nothing
    , testProperty "branch 1 valid" $ \x ->
        fromSumType (Proxy @1) (toSpecialJSON x) === Just x
    , testProperty "branch 1 invalid" $
        forAll (specialJSONExcept 1) $ \(branch, value) ->
          labelBranch branch $ fromSumType (Proxy @1) value === Nothing
    , testProperty "branch 2 valid" $ \l ->
        fromSumType (Proxy @2) (toSpecialJSON l) === Just l
    , testProperty "branch 2 invalid" $
        forAll (specialJSONExcept 2) $ \(branch, value) ->
          labelBranch branch $ fromSumType (Proxy @2) value === Nothing
    ]
  where
    specialJSONExcept :: Int -> Gen (Int, SpecialJSON)
    specialJSONExcept validBranch =
      let fmapFst (a, gen) = (a,) <$> gen
       in oneof $
            map fmapFst $
              filter
                ((/= validBranch) . fst)
                [ (0, toSpecialJSON <$> arbitrary @Bool)
                , (1, toSpecialJSON <$> arbitrary @Int)
                , (2, toSpecialJSON <$> arbitrary @[String])
                ]

    labelBranch branch = label $ "branch " ++ show branch
