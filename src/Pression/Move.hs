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
import System.Directory (renameDirectory, renameFile, createDirectoryIfMissing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String.Conv (toS)
import Control.Monad
import Data.Foldable
import Fmt
import System.IO.Unsafe (unsafePerformIO)
import Data.LruCache.IO (newLruHandle, cached, LruHandle)
import Data.List (sortOn)

-- TODO from might not be necessary : search for the game in the path?
moveGame :: InstalledGame -> FilePath -> IO ()
moveGame (InstalledGame from game) dest = do
  -- TODO check that steam is not running
  let manifestName = "appmanifest_" ++ show game <.> "acf"
  manifest <- parseSteamFile' (from </> manifestName)
  let installDir = manifest ^?! key "AppState" . key "installdir" . _String . to toS
      sizeOnDisk = manifest ^?! key "AppState" . key "SizeOnDisk" . _String . to (read . toS)

  availSpace <- getAvailSpace dest
  when (availSpace < sizeOnDisk)
    (fail $ "No enough space left on "#|dest|#" ("#|availSpace|#" available, needs "#|sizeOnDisk|#")")

  createDirectoryIfMissing False (dest </> "common")
  renameDirectory (from </> "common" </> installDir) (dest </> "common" </> installDir)
  renameFile (from </> manifestName) (dest </> manifestName)

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
  localconfig <- parseSteamFile (steamDir </> "userdata" </> user </> "config/localconfig.vdf")
  return (read . toS <$> localconfig ^? key "UserLocalConfigStore" . key "Software" . key "Valve" . key "Steam" . key "Apps" . key (toS $ show gid) . key "LastPlayed" . _String)

playedRecently :: GameId -> IO Bool
playedRecently g = do
  mlastPlayed <- getLastPlayed g
  case mlastPlayed of
    Just lastPlayed -> do
      now <- round <$> getPOSIXTime
      let twoWeeksAgo = now - (60 * 60 * 24 * 7 * 2)
      return (lastPlayed >= twoWeeksAgo)
    Nothing -> return False

getSizeOnDisk :: InstalledGame -> IO Integer
getSizeOnDisk g = do
  manifest <- parseSteamFile' (libraryDir g </> manifestPath g)
  return (manifest ^?! key "AppState" . key "SizeOnDisk" . _String . to (read . toS))

shuffleAround :: FilePath -> FilePath -> Integer -> IO ()
shuffleAround hot cold hotDesiredFreeSpace = do
  reheat <- filterM (playedRecently . gameId) =<< gamesInDir cold
  sizeOfReheat <- sum <$> traverse getSizeOnDisk reheat
  let sizeToFree = sizeOfReheat + hotDesiredFreeSpace

  let addSize g = do size <- getSizeOnDisk g
                     return (g, size)
  candidatesForFreezing <- traverse addSize =<< (gamesInDir hot)
  let accum (!games, !size) (g, s) = (g:games, size+s)
      freeze = fst . head . filter ( (>= sizeToFree). snd) . scanl accum ([], 0) . sortOn (negate . snd) $ candidatesForFreezing

  putStrLn "Reheat:"
  print reheat
  putStrLn "Freeze:"
  print freeze

  traverse_ (\g -> moveGame g cold) freeze
  traverse_ (\g -> moveGame g hot) reheat

