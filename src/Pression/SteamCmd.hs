{-# LANGUAGE OverloadedStrings #-}
module Pression.SteamCmd where

import Pression.Types

import Data.Monoid
import System.Process.Typed
import System.Directory
import System.FilePath ((</>))


downloadGame :: FilePath -> GameId -> IO ()
downloadGame root (GameId game) = do
  let destination = root </> show game
      instructions =
        [ "+@ShutdownOnFailedCommand 1"
        , "+@NoPromptForPassword 1"
        , "+@sSteamCmdForcePlatformType windows"
        , "+login georgesmadjar"
        , "+force_install_dir " <> destination
        , "+app_update " <> show game <> " validate"
        , "+quit"
        ]
  createDirectoryIfMissing True destination
  runProcess_ (proc "steamcmd" instructions)
