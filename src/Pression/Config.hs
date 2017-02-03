{-# LANGUAGE OverloadedStrings #-}

module Pression.Config where

import Control.Lens
import Data.Maybe
import Data.Tagged
import qualified Data.Text as T
import Pression.Parser
import Text.Trifecta

data ConfigVdf

getConfig :: IO (Tagged ConfigVdf Value)
getConfig =
  Tagged . fromJust <$>
  parseFromFile
    steamConfigParser
    "C:\\Program Files (x86)\\Steam\\config\\config.vdf"

configInstallFolders :: Tagged ConfigVdf Value -> [String]
configInstallFolders config =
  untag config ^.. key "InstallConfigStore" . key "Software" . key "Valve" .
  key "Steam" .
  _Object .
  itraversed .
  indices ("BaseInstallFolder" `T.isPrefixOf`) .
  _String .
  to T.unpack
