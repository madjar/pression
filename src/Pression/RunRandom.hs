module Pression.RunRandom where

import Pression.Library
import Control.Monad.Random
import System.Process

runRandom :: IO ()
runRandom = do
  ids <- gamesIds
  GameId chosenId <- uniform ids
  callCommand $ "start steam://nav/games/details/" ++ show chosenId
