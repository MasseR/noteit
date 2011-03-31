{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Char
import System.Console.CmdArgs
import Test.QuickCheck
import System.Environment.XDG.BaseDir
import System.IO (openTempFile)
import System.Directory (copyFile)
import Data.Time.Clock
import System.Locale
import Data.Time.Format

data Title = Title Text Slug | Date Text deriving (Show, Read)
newtype Slug = Slug Text deriving (Read, Show, Eq, Ord)

fromSlug ::  Slug -> Text
fromSlug (Slug x) = x

slug :: Text -> Slug
slug x = Slug . T.filter (isPrint) $ T.toLower $ T.map (\y -> if isSpace y then '_' else y) x

data NoteItArgs = NoteItArgs {
    add :: Bool
  , edit :: Int
  , list :: Bool
  } deriving (Show, Data, Typeable)

noteitargs ::  Mode (CmdArgs NoteItArgs)
noteitargs = cmdArgsMode $ NoteItArgs {
    add = def
  , edit = def
  , list = def
  }

time ::  IO Text
time = fmap titletime getCurrentTime
titletime ::  UTCTime -> Text
titletime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

metaFile ::  IO FilePath
metaFile = getUserDataFile "noteit" ".meta"
noteFile :: Slug -> IO FilePath
noteFile = getUserDataFile "noteit" . T.unpack .  fromSlug -- Should also add .markdown

maybeTitle ::  Text -> IO Title
maybeTitle x
  | x == "" = Date `fmap` time
  | otherwise = return $ title x
title x = Title x $ slug x

addNote ::  IO ()
addNote = do
  TI.putStr "Title: "
  title <- TI.getLine >>= maybeTitle
  print title

main ::  IO ()
main = do
  a <- cmdArgsRun noteitargs
  case a of
       (NoteItArgs True _ _) -> addNote

--tests

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

prop_slug_no_spaces x = property $ T.length (T.dropWhile (not . isSpace) $ fromSlug $ slug x) == 0
prop_slug_all_lower x = property $ T.length (T.filter (isUpper) $ fromSlug $ slug x) == 0
prop_slug_all_printable x = property $ T.length (T.filter (not . isPrint) (fromSlug $ slug x)) == 0
