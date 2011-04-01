{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Main where

import System.Exit (ExitCode(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import Data.Char (isPrint, isSpace, isUpper, isPrint)
import System.Console.CmdArgs (Data, Typeable, Mode, CmdArgs, cmdArgsMode, def, cmdArgsRun, (&=), program)
import Test.QuickCheck (Arbitrary, arbitrary, property)
import System.Environment.XDG.BaseDir (getUserDataFile, getUserDataDir)
import System.IO (hClose, openTempFile)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import Control.Exception (bracket)
import System.Environment (getEnv)
import System.Process (system)

newtype Note a = Note (ErrorT String (StateT DB IO) a)
  deriving (Monad, MonadError String, MonadState DB, MonadIO, Functor, Applicative, Alternative)
newtype Selection = Selection Int deriving (Read, Show, Eq, Ord, Num, Real, Enum, Integral)
fromSelection ::  Selection -> Int
fromSelection (Selection i) = i - 1
selection ::  Int -> Selection
selection = Selection

data Title = Title Text Slug | Date Text deriving (Show, Read)
newtype Slug = Slug Text deriving (Read, Show, Eq, Ord)
type DB = Set Slug

fromSlug ::  Slug -> Text
fromSlug (Slug x) = x

slug :: Text -> Slug
slug x = Slug . T.filter (isPrint) $ T.toLower $ T.map (\y -> if isSpace y then '_' else y) x

data NoteItArgs = NoteItArgs {
    add :: Bool
  , edit :: Maybe Int
  , list :: Bool
  , remove :: Maybe Int
  } deriving (Show, Data, Typeable)

noteitargs ::  Mode (CmdArgs NoteItArgs)
noteitargs = cmdArgsMode $ NoteItArgs {
    add = def
  , edit = def
  , list = def
  , remove = def
  } &= program "NoteIt"

time ::  Note Text
time = fmap titletime $ liftIO getCurrentTime

editor :: Note String
editor = liftIO $ getEnv "EDITOR"

runEditor :: Slug -> Note ()
runEditor s = do
  e <- editor
  f <- noteFile s
  liftIO $ system $ e ++ " " ++ f
  return ()

titletime ::  UTCTime -> Text
titletime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

metaFile :: MonadIO m => m FilePath
metaFile = liftIO $ getUserDataFile "noteit" ".meta"

noteFile :: Slug -> Note FilePath
noteFile = liftIO . fmap (++ ".markdown") . getUserDataFile "noteit" . T.unpack .  fromSlug

maybeTitle ::  Text -> Note Title
maybeTitle x
  | x == "" = Date `fmap` time
  | otherwise = return $ title x
title x = Title x $ slug x

slugFromTitle :: Title -> Slug
slugFromTitle (Title _ s) = s
slugFromTitle (Date d) = slug d

addNote ::  Note ()
addNote = do
  liftIO $ TI.putStrLn "Title: "
  title <- liftIO TI.getLine >>= maybeTitle
  let ph = mkPlaceHolder title
  modify (insNote title)
  when (isJust ph) $
    writePlaceholder (slugFromTitle title) (fromJust ph)
  runEditor $ slugFromTitle title

insNote :: Title -> DB -> DB
insNote (Title _ s) db = S.insert s db
insNote (Date d) db = S.insert (slug d) db
rmNote :: Selection -> DB -> DB
rmNote i db = S.delete (selectionToSlug i db) db -- horribly inefficient :P

readMeta :: (MonadError String m, MonadIO m) => m DB
readMeta = do
  m <- metaFile
  e <- liftIO $ doesFileExist m
  if not e
     then return S.empty
     else liftIO (readFile m) >>= readM

writePlaceholder :: Slug -> Text -> Note ()
writePlaceholder s t = do
  f <- noteFile s
  liftIO $ bracket
    (openTempFile "/tmp" "note")
    (\(p,h) -> hClose h >> copyFile p f)
    (\(_,h) -> TI.hPutStr h t)

writeMeta :: DB -> IO ()
writeMeta db = do
  f <- metaFile
  bracket
    (openTempFile "/tmp" "meta")
    (\(p,h) -> hClose h >> copyFile p f)
    (\(_,h) -> TI.hPutStr h (T.pack $ show db))

readM :: ((MonadError String) m, Read a) => String -> m a
readM x = case reads x of
               [] -> throwError $ "Could not parse: " ++ x
               [(a,_)] -> return a

mkPlaceHolder :: Title -> Maybe Text
mkPlaceHolder (Title t _) = Just $ T.unlines [t, T.replicate (T.length t) "=", ""]
mkPlaceHolder _ = Nothing

noteDir = getUserDataDir "noteit"

runNote :: Note () -> IO ()
runNote (Note x) = do
  dir <- noteDir
  createDirectoryIfMissing True dir
  edb <- runErrorT readMeta
  case edb of
       Left e -> TI.putStrLn $ "Could not read metafile: " `T.append` T.pack e
       Right db -> do
        (r, db') <- runStateT (runErrorT x) db
        case r of
             Left e' -> TI.putStrLn $ "Error: " `T.append` T.pack e'
             Right _ -> writeMeta db'
        return ()

listings :: Note Text
listings = fmap (fmtlistings . S.toList) (get)
  where fmtlistings x = T.unlines $
          zipWith (\i y -> (T.pack $ show i) `T.append` ". " `T.append` fromSlug y) [1..] x

listNotes ::  Note ()
listNotes = listings >>= liftIO . TI.putStrLn

selectionToSlug :: Selection -> DB -> Slug
selectionToSlug i = (!! (fromSelection i)) . S.toList

editNote ::  Selection -> Note ()
editNote i = do
  s <- fmap (selectionToSlug i) get
  runEditor s
removeNote :: Selection -> Note ()
removeNote i = modify (rmNote i)

main ::  IO ()
main = do
  a <- cmdArgsRun noteitargs
  case a of
       (NoteItArgs True _ _ _) -> runNote addNote
       (NoteItArgs _ _ True _) -> runNote listNotes
       (NoteItArgs _ _ _ (Just i)) -> runNote $ removeNote $ selection i
       (NoteItArgs _ (Just i) _ _) -> runNote $ editNote $ selection i

--tests

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

prop_slug_no_spaces x = property $ T.length (T.dropWhile (not . isSpace) $ fromSlug $ slug x) == 0
prop_slug_all_lower x = property $ T.length (T.filter isUpper $ fromSlug $ slug x) == 0
prop_slug_all_printable x = property $ T.length (T.filter (not . isPrint) (fromSlug $ slug x)) == 0
