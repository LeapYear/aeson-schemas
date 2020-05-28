{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AllTypes where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema
import Data.Aeson.Schema.TH (mkEnum)
import qualified Data.Text as Text

import Util (getMockedResult)

{- Greeting enum -}

mkEnum "Greeting" ["HELLO", "GOODBYE"]

{- Coordinate scalar -}

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show)

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> return $ Coordinate (x, y)
      _ -> fail $ "Bad Coordinate: " ++ Text.unpack s

{- AllTypes result -}

type Schema = [schema|
  {
    bool: Bool,
    int: Int,
    int2: Int,
    double: Double,
    text: Text,
    scalar: Coordinate,
    enum: Greeting,
    maybeObject: Maybe {
      text: Text,
    },
    maybeObjectNull: Maybe {
      text: Text,
    },
    tryObject: Try {
      a: Int,
    },
    tryObjectNull: Try {
      a: Int,
    },
    maybeList: Maybe List {
      text: Text,
    },
    maybeListNull: Maybe List {
      text: Text,
    },
    // this is a comment
    list: List {
      type: Text,
      maybeBool: Maybe Bool,
      maybeInt: Maybe Int,
      maybeNull: Maybe Bool,
    },
    nonexistent: Maybe Text,
    // future_key: Int,
    union: List (
        { a: Int } | List Bool | Text
    ),
    [phantom]: {
      keyForPhantom: Int,
    },
  }
|]

result :: Object Schema
result = $(getMockedResult "test/all_types.json")

{- AllTypes getters -}

mkGetter "ListItem" "getList" ''AllTypes.Schema ".list[]"
