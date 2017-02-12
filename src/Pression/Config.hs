{-# LANGUAGE OverloadedStrings #-}

module Pression.Config where

import           Pression.Parser

import           Control.Lens
import           Data.Maybe (fromJust)
import           Data.Tagged
import qualified Data.Text as T
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import           System.IO.Unsafe (unsafePerformIO)
import           System.Info.Extra (isWindows)
import           Text.Trifecta

data ConfigVdf

getSteamDir :: IO String
getSteamDir =
    if isWindows
        then return "C:\\Program Files (x86)\\Steam"
        else do
            home <- getHomeDirectory
            return (home </> ".steam/steam")

steamDir :: String
steamDir = unsafePerformIO getSteamDir

getConfig :: IO (Tagged ConfigVdf Value)
getConfig =
  Tagged . fromJust <$>
  parseFromFile
    steamConfigParser
    (steamDir </> "config/config.vdf")

configInstallFolders :: Tagged ConfigVdf Value -> [String]
configInstallFolders config =
  untag config ^.. key "InstallConfigStore" . key "Software" . key "Valve" .
  key "Steam" .
  _Object .
  itraversed .
  indices ("BaseInstallFolder" `T.isPrefixOf`) .
  _String .
  to T.unpack
