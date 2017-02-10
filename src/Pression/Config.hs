{-# LANGUAGE OverloadedStrings #-}

module Pression.Config where

import Control.Lens
import Data.Maybe
import Data.Tagged
import qualified Data.Text as T
import Pression.Parser
import Text.Trifecta
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe
import System.Info.Extra

data ConfigVdf

getSteamDir :: IO String
getSteamDir = if isWindows
                then return "C:\\Program Files (x86)\\Steam"
                else do home <- getHomeDirectory
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
