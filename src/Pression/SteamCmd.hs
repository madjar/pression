{-# LANGUAGE OverloadedStrings #-}

module Pression.SteamCmd where

import Pression.Types
import System.Directory
import System.FilePath ((</>))
import System.Process.Typed

downloadGame :: FilePath -> GameId -> IO ()
downloadGame root (GameId game) = do
  let destination = root </> show game
      instructions =
        [ "+login georgesmadjar",
          "+force_install_dir " <> destination,
          "+app_update " <> show game <> " validate"
        ]
  createDirectoryIfMissing True destination
  runProcess_ (steamcmdProc instructions)

appInfo :: GameId -> IO ()
appInfo (GameId i) =
  runProcess_ (steamcmdProc ["+app_info_print " <> show i])

steamcmd :: [String] -> IO ()
steamcmd args =
  runProcess_ (steamcmdProc args)

steamcmdProc :: [String] -> ProcessConfig () () ()
steamcmdProc args =
  proc
    "steamcmd"
    ( [ "+@ShutdownOnFailedCommand 1",
        "+@NoPromptForPassword 1",
        "+@sSteamCmdForcePlatformType windows"
      ]
        <> args
        <> ["+quit"]
    )
