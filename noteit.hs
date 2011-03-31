{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Main where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Char (isPrint, isSpace, isUpper, isPrint)
import System.Console.CmdArgs (Data, Typeable, Mode, CmdArgs, cmdArgsMode, def, cmdArgsRun)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import System.Environment.XDG.BaseDir (getUserDataFile, getUserDataDir)
import System.IO (openTempFile)
import System.Directory (copyFile, createDirectoryIfMissing)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

newtype Note a = Note (ErrorT String (StateT DB IO) a) deriving (Monad, MonadError String, MonadState DB, MonadIO, Functor, Applicative, Alternative)

data Title = Title Text Slug | Date Text deriving (Show, Read)
newtype Slug = Slug Text deriving (Read, Show, Eq, Ord)
type DB = Map Slug FilePath

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

time ::  Note Text
time = fmap titletime $ liftIO getCurrentTime

titletime ::  UTCTime -> Text
titletime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

metaFile :: MonadIO m => m FilePath
metaFile = liftIO $ getUserDataFile "noteit" ".meta"

noteFile :: Slug -> IO FilePath
noteFile = liftIO . fmap (++ ".markdown") . getUserDataFile "noteit" . T.unpack .  fromSlug

maybeTitle ::  Text -> Note Title
maybeTitle x
  | x == "" = Date `fmap` time
  | otherwise = return $ title x
title x = Title x $ slug x

addNote ::  Note ()
addNote = do
  liftIO $ TI.putStr "Title: "
  title <- (liftIO $ TI.getLine) >>= maybeTitle
  return ()

readMeta :: (MonadError String m, MonadIO m) => m DB
readMeta = do
  m <- metaFile
  e <- liftIO $ doesFileExist m
  if not e
     then return M.empty
     else liftIO (readFile m) >>= readM

writeMeta :: DB -> IO ()
writeMeta db = do
  f <- metaFile
  TI.writeFile f $ T.pack $ show db

readM :: ((MonadError String) m, Read a) => String -> m a
readM x = case reads x of
               [] -> throwError $ "Could not parse: " ++ x
               [(a,_)] -> return a

mkPlaceHolder :: Title -> Maybe Text
mkPlaceHolder (Title t _) = Just $ T.unlines $ [t, T.replicate (T.length t) "=", ""]
mkPlaceHolder _ = Nothing

noteDir = getUserDataDir "noteit"

runNote :: Note () -> IO ()
runNote (Note x) = do
  dir <- noteDir
  createDirectoryIfMissing True dir
  edb <- runErrorT $ readMeta
  case edb of
       Left e -> TI.putStrLn $ "Could not read metafile: " `T.append` (T.pack e)
       Right db -> do
        (r, db') <- runStateT (runErrorT x) db
        case r of
             Left e' -> TI.putStrLn $ "Error: " `T.append` (T.pack e')
             Right _ -> writeMeta db'
        return ()

main ::  IO ()
main = do
  a <- cmdArgsRun noteitargs
  case a of
       (NoteItArgs True _ _) -> undefined

--tests

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

prop_slug_no_spaces x = property $ T.length (T.dropWhile (not . isSpace) $ fromSlug $ slug x) == 0
prop_slug_all_lower x = property $ T.length (T.filter (isUpper) $ fromSlug $ slug x) == 0
prop_slug_all_printable x = property $ T.length (T.filter (not . isPrint) (fromSlug $ slug x)) == 0
