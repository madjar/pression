module Pression.Library
  ( gamesIds, GameId(..)
  ) where

import Data.Array ((!))
import Data.Maybe
import Pression.Config
import System.Directory
import Text.Regex.TDFA

-- data Game = Game
--   { steamId :: Integer
--   } deriving (Show)

newtype GameId = GameId Integer deriving Show

gamesIds :: IO [GameId]
gamesIds = do
  config <- getConfig
  let base = "C:\\Program Files (x86)\\Steam\\steamapps"
      folders = base : configInstallFolders config
  concat <$> traverse gamesIdsInPath folders

gamesIdsInPath :: FilePath -> IO [GameId]
gamesIdsInPath path = do
  content <- listDirectory path
  return $ mapMaybe getManifestId content

getManifestId :: String -> Maybe GameId
getManifestId file = do
  result <- file =~~ "appmanifest_([0-9]+).acf"
  return $ GameId $ read (mrSubs result ! 1)
