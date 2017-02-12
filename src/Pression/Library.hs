{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Pression.Library
  ( gamesIds, GameId(..), runGame, randomGame
  ) where

import Pression.Config

import Control.Monad.Random (uniform)
import Data.Array ((!))
import Data.Maybe (mapMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Info.Extra (isWindows)
import System.Process (callCommand)
import Text.Regex.PCRE.Rex (rex)

newtype GameId = GameId Integer deriving Show

gamesIds :: IO [GameId]
gamesIds = do
    config <- getConfig
    let base = steamDir </> "steamapps"
        folders = base : configInstallFolders config
    concat <$> traverse gamesIdsInPath folders
  where
    gamesIdsInPath path = mapMaybe matchManifest <$> listDirectory path
    matchManifest = [rex|^appmanifest_(?{ GameId . read }[0-9]+).acf$|]

randomGame :: IO GameId
randomGame = uniform =<< gamesIds

runGame :: GameId -> IO ()
runGame (GameId gid) =
    callCommand $ runcmd ++ " steam://nav/games/details/" ++ show gid
  where
    runcmd =
        if isWindows
            then "open"
            else "steam"
