{-# LANGUAGE TupleSections #-}
module Pression.Library
  ( steamappsDirs, gamesInDir, allGames, GameId(..), runGame, randomGame
  ) where

import Pression.Types
import Pression.Config

import Control.Monad.Random (uniform)
import Data.Maybe (mapMaybe)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Info.Extra (isWindows)
import System.Process (callCommand)
import Text.Regex.TDFA ((=~~), mrSubs)
import Data.Array ((!))


steamappsDirs :: IO [String]
steamappsDirs = do
  config <- getConfig
  return $ map (</> "steamapps") (steamDir : configInstallFolders config)


gamesInDir :: FilePath -> IO [InstalledGame]
gamesInDir dir = map (InstalledGame dir) . mapMaybe matchManifest <$> listDirectory dir
    where matchManifest :: String -> Maybe Integer
          matchManifest file = do result <- file =~~ "appmanifest_([0-9]+).acf"
                                  return $ read (mrSubs result ! 1)

allGames :: IO [InstalledGame]
allGames = do
    folders <- steamappsDirs
    concat <$> traverse gamesInDir folders

randomGame :: IO GameId
randomGame = gameId <$> (uniform =<< allGames)

runGame :: GameId -> IO ()
runGame (GameId gid) =
    callCommand $ runcmd ++ " steam://nav/games/details/" ++ show gid --TODO move to typed-process
  where
    runcmd =
        if isWindows
            then "start"
            else "steam"
