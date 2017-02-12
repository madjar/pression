{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Pression.Library
  ( gamesIds, GameId(..)
  ) where

import Data.Array ((!))
import Data.Maybe
import Pression.Config
import System.Directory
import System.FilePath
import Text.Regex.PCRE.Rex (rex)

-- data Game = Game
--   { steamId :: Integer
--   } deriving (Show)

newtype GameId = GameId Integer deriving Show

gamesIds :: IO [GameId]
gamesIds = do
  config <- getConfig
  let base = steamDir </> "steamapps"
      folders = base : configInstallFolders config
  concat <$> traverse gamesIdsInPath folders

gamesIdsInPath :: FilePath -> IO [GameId]
gamesIdsInPath path = do
  content <- listDirectory path
  return $ mapMaybe matchManifest content

matchManifest :: String -> Maybe GameId
matchManifest = [rex|^appmanifest_(?{ GameId . read }[0-9]+).acf$|]
