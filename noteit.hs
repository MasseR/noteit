{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Char
import System.Console.CmdArgs
import Test.QuickCheck

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

slug :: Text -> Text
slug x = T.filter (isPrint) $ T.toLower $ T.map (\y -> if isSpace y then '_' else y) x

prop_slug_no_spaces x = property $ T.length (T.dropWhile (not . isSpace) $ slug x) == 0
prop_slug_all_lower x = property $ T.length (T.filter (isUpper) $ slug x) == 0
prop_slug_all_printable x = property $ T.length (T.filter (not . isPrint) (slug x)) == 0

main = undefined
