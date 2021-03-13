module Data.Aeson.Schema.Utils.Invariant (
  unreachable,
) where

{- | An error function to indicate that a branch is unreachable. Provides a useful error message
 if it ends up happening, pointing users to write a bug report.
-}
unreachable :: String -> a
unreachable msg =
  error $
    unlines
      [ "`aeson-schemas` internal error: " ++ msg
      , "Please file a bug report at https://github.com/LeapYear/aeson-schemas/issues/"
      ]
