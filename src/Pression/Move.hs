{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pression.Move where

import Control.Lens ((^?), (^?!), failing, to, folding)
import Data.List (maximum)
import Data.LruCache.IO (LruHandle, cached, newLruHandle)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Fmt
import Path (parseAbsDir, parseAbsFile)
import Path.IO
import Pression.Config
import Pression.Library
import Pression.Parser
import Pression.Types
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.DiskSpace (getAvailSpace)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Relude.Unsafe as Unsafe
import Relude.Extra.Tuple

moveGame :: InstalledGame -> FilePath -> IO ()
moveGame g@(InstalledGame from game) dest = do
  -- TODO check that steam is not running
  let manifestName = "appmanifest_" ++ show game <.> "acf"
  manifest <- parseSteamFile' (from </> manifestName)
  let installDir = manifest ^?! key "AppState" . key "installdir" . _String . to toS
      sizeOnDisk = manifest ^?! key "AppState" . key "SizeOnDisk" . _String . folding (readMaybe . toS)
  availSpace <- getAvailSpace dest
  when
    (availSpace < sizeOnDisk)
    (fail $ "No enough space left on " +| dest |+ " (" +| availSpace |+ " available, needs " +| sizeOnDisk |+ ")")
  name <- gameName g
  size <- getSizeOnDisk g
  let sizeInGig = fromInteger size / 1024 / 1024 / 1024 :: Double
  fmtLn ("Moving " +|| name ||+ " to " +| dest |+ " (" +|| sizeInGig ||+ " GB).")
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

-- TODO Make that cache nicer
{-# NOINLINE steamCache #-}
steamCache :: LruHandle String Value
steamCache = unsafePerformIO (newLruHandle 100)

parseSteamFile' :: String -> IO Value
parseSteamFile' path = cached steamCache path (parseSteamFile path)

getLastPlayed :: GameId -> IO (Maybe Integer)
getLastPlayed (GameId gid) = do
  users <- listDirectory (steamDir </> "userdata")
  playTimes <- mapM lastPlayedFor users
  return (maximum playTimes)
  where
    lastPlayedFor user = do
      localconfig <-
        parseSteamFile'
          (steamDir </> "userdata" </> user </> "config/localconfig.vdf")
      return
        ( localconfig ^? key "UserLocalConfigStore"
            . key "Software"
            . failing (key "valve") (key "Valve")
            . key "Steam"
            . failing (key "apps") (key "Apps")
            . key (show gid) -- This seems to be different on windows and mac
            . key "LastPlayed"
            . _String
            . to toS
            . folding readMaybe
        )

playedRecently :: GameId -> IO Bool
playedRecently g = do
  mlastPlayed <- getLastPlayed g
  case mlastPlayed of
    Just lastPlayed -> do
      now <- round <$> getPOSIXTime
      let aMonthAgo = now - (60 * 60 * 24 * 30)
      return (lastPlayed >= aMonthAgo)
    Nothing -> return False

getSizeOnDisk :: InstalledGame -> IO Integer
getSizeOnDisk g = do
  manifest <- parseSteamFile' (libraryDir g </> manifestPath g)
  return (manifest ^?! key "AppState" . key "SizeOnDisk" . _String . folding (readMaybe . toS))

gameName :: InstalledGame -> IO Text
gameName g = do
  manifest <- parseSteamFile' (libraryDir g </> manifestPath g)
  return (manifest ^?! key "AppState" . key "name" . _String)

sortOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortOnM f = fmap (map snd . sortWith fst) . traverse (\x -> do y <- f x; return (y, x))

data ShuffleResult = ShuffleResult {hot :: FilePath, cold :: FilePath, reheat :: [InstalledGame], freeze :: [InstalledGame]}

gamesToShuffleAround :: FilePath -> FilePath -> Integer -> IO ShuffleResult
gamesToShuffleAround hot cold hotDesiredFreeSpace = do
  reheat <- filterM (playedRecently . gameId) =<< gamesInDir cold
  sizeOfReheat <- sum <$> traverse getSizeOnDisk reheat
  availSpaceOnHot <- getAvailSpace hot
  let sizeToFree = max 0 (sizeOfReheat + hotDesiredFreeSpace - availSpaceOnHot)
  -- Don`t sort them by size, sort them by last played ! (most recently played at the end)
  candidatesForFreezing <- sortOnM (getLastPlayed . gameId) =<< gamesInDir hot
  candidatesForFreezingWithSize <- traverse (traverseToSnd getSizeOnDisk) candidatesForFreezing
  let accum (!games, !size) (g, s) = (g : games, size + s)
      freeze = reverse . fst . Unsafe.head . filter ((>= sizeToFree) . snd) . scanl accum ([], 0) $ candidatesForFreezingWithSize
  return ShuffleResult {..}

printShuffleResult :: ShuffleResult -> IO ()
printShuffleResult ShuffleResult{..} = do
  let prettyGame g = (,) <$> gameName g <*> (getLastPlayed . gameId) g
  putStrLn "Reheat:"
  print =<< traverse prettyGame reheat
  putStrLn "Freeze:"
  print =<< traverse prettyGame freeze
  print . sum =<< traverse getSizeOnDisk freeze


shuffleAround :: ShuffleResult -> IO ()
shuffleAround ShuffleResult{..} = do
  traverse_ (`moveGame` cold) freeze
  traverse_ (`moveGame` hot) reheat
