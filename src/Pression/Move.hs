{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Pression.Move where

import Pression.Types
import Pression.Parser
import Pression.Config
import Pression.Library

import Control.Lens ((^?!), (^?), to)
import System.FilePath ((</>), (<.>))
import System.DiskSpace (getAvailSpace)
import System.Directory (createDirectoryIfMissing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String.Conv (toS)
import Control.Monad
import Data.Foldable
import Fmt
import System.IO.Unsafe (unsafePerformIO)
import Data.LruCache.IO (newLruHandle, cached, LruHandle)
import Data.List (sortOn)
import Path (parseAbsDir, parseAbsFile)
import Path.IO

moveGame :: InstalledGame -> FilePath -> IO ()
moveGame g@(InstalledGame from game) dest = do
  -- TODO check that steam is not running
  let manifestName = "appmanifest_" ++ show game <.> "acf"
  manifest <- parseSteamFile' (from </> manifestName)
  let installDir = manifest ^?! key "AppState" . key "installdir" . _String . to toS
      sizeOnDisk = manifest ^?! key "AppState" . key "SizeOnDisk" . _String . to (read . toS)

  availSpace <- getAvailSpace dest
  when (availSpace < sizeOnDisk)
    (fail $ "No enough space left on "#|dest|#" ("#|availSpace|#" available, needs "#|sizeOnDisk|#")")

  name <- gameName g
  size <- getSizeOnDisk g
  let sizeInGig = (fromInteger size) / 1024 / 1024 / 1024 :: Double
  fmtLn ("Moving "#||name||#" to "#|dest|#" ("#|| sizeInGig ||#" GB).")

  createDirectoryIfMissing False (dest </> "common")

  -- No recurive copy in base, so using the one from path0-io
  fromDir <- parseAbsDir (from </> "common" </> installDir)
  toDir <- parseAbsDir (dest </> "common" </> installDir)
  fromManifest <- parseAbsFile (from </> manifestName)
  toManifest <- parseAbsFile (dest </> manifestName)
  copyDirRecur fromDir toDir
  copyFile fromManifest toManifest
  removeDirRecur fromDir
  removeFile fromManifest

user :: String
user = "36588102"

-- TODO Make that cache nicer
{-# NOINLINE steamCache #-}
steamCache :: LruHandle String Value
steamCache = unsafePerformIO (newLruHandle 100)

parseSteamFile' :: String -> IO Value
parseSteamFile' path = cached steamCache path (parseSteamFile path)

getLastPlayed :: GameId -> IO (Maybe Integer)
getLastPlayed (GameId gid) = do
  localconfig <- parseSteamFile' (steamDir </> "userdata" </> user </> "config/localconfig.vdf")
  return (read . toS <$> localconfig ^? key "UserLocalConfigStore" . key "Software" . key "Valve" . key "Steam" . key "apps" . key (toS $ show gid) . key "LastPlayed" . _String)

playedRecently :: GameId -> IO Bool
playedRecently g = do
  mlastPlayed <- getLastPlayed g
  case mlastPlayed of
    Just lastPlayed -> do
      now <- round <$> getPOSIXTime
      let aMonthAgo = now - (60 * 60 * 24 * 30 * 1)
      return (lastPlayed >= aMonthAgo)
    Nothing -> return False

getSizeOnDisk :: InstalledGame -> IO Integer
getSizeOnDisk g = do
  manifest <- parseSteamFile' (libraryDir g </> manifestPath g)
  return (manifest ^?! key "AppState" . key "SizeOnDisk" . _String . to (read . toS))

gameName g = do
  manifest <- parseSteamFile' (libraryDir g </> manifestPath g)
  return (manifest ^?! key "AppState" . key "name" . _String)

shuffleAround :: FilePath -> FilePath -> Integer -> IO ()
shuffleAround hot cold hotDesiredFreeSpace = do
  reheat <- filterM (playedRecently . gameId) =<< gamesInDir cold
  sizeOfReheat <- sum <$> traverse getSizeOnDisk reheat
  availSpaceOnHot <- getAvailSpace hot
  let sizeToFree = max 0 (sizeOfReheat + hotDesiredFreeSpace - availSpaceOnHot)

  let addSizeAndLastPlayed g = do size <- getSizeOnDisk g
                                  lastPlayed <- getLastPlayed (gameId g)
                                  return ((g, size), lastPlayed)
  gamesAndSizeAndLastPlayed <- traverse addSizeAndLastPlayed =<< (gamesInDir hot)
  -- Don`t sort them by size, sort them by last played !
  let accum (!games, !size) (g, s) = (g:games, size+s)
      candidatesForFreezing =  map fst . sortOn snd $ gamesAndSizeAndLastPlayed
      freeze = reverse . fst . head . filter ( (>= sizeToFree). snd) . scanl accum ([], 0) $ candidatesForFreezing

  let prettyGame g = (,) <$> gameName g <*> (getLastPlayed . gameId) g
  putStrLn "Reheat:"
  print =<< traverse prettyGame reheat
  putStrLn "Freeze:"
  print =<< traverse prettyGame freeze
  print . sum =<< traverse getSizeOnDisk freeze

  traverse_ (\g -> moveGame g cold) freeze
  traverse_ (\g -> moveGame g hot) reheat
