{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.Utils.NameLike (
  NameLike (..),
  fromName,
  resolveName,
) where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (Name, Q, lookupTypeName, nameBase)

data NameLike = NameRef String | NameTH Name

instance Eq NameLike where
  ty1 == ty2 = fromName ty1 == fromName ty2

instance Show NameLike where
  show = show . fromName

fromName :: NameLike -> String
fromName = \case
  NameRef s -> s
  NameTH name -> nameBase name

resolveName :: NameLike -> Q Name
resolveName = \case
  -- some hardcoded cases
  NameRef "Bool" -> pure ''Bool
  NameRef "Int" -> pure ''Int
  NameRef "Double" -> pure ''Double
  NameRef "Text" -> pure ''Text
  -- general cases
  NameRef name -> lookupTypeName name >>= maybe (fail $ "Unknown type: " ++ name) pure
  NameTH name -> pure name
