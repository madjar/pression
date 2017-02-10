module Pression.RunRandom where

import Pression.Library
import Control.Monad.Random
import System.Process
import System.Info.Extra

runRandom :: IO ()
runRandom = do
  ids <- gamesIds
  GameId chosenId <- uniform ids
  callCommand $ runcmd ++ " steam://nav/games/details/" ++ show chosenId
  where runcmd = if isWindows
                   then "open"
                   else "steam"
