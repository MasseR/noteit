{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Char (isSpace)

slug :: Text -> Text
slug x = T.toLower $ T.map (\y -> if isSpace y then '_' else y) x
