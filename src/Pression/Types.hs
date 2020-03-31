{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pression.Types where

import Data.Binary
import System.FilePath ((<.>), (</>))

newtype GameId = GameId Integer deriving (Show, Binary)

data InstalledGame
  = InstalledGame
      { libraryDir :: FilePath,
        gameId_ :: Integer
        --TODO manifest cache here?
      }
  deriving (Show)

-- TODO decide if needed ?
-- TODO this probably belongs in Library
gameId :: InstalledGame -> GameId
gameId = GameId . gameId_

manifestPath :: InstalledGame -> FilePath
manifestPath (InstalledGame dir i) = dir </> "appmanifest_" ++ show i <.> "acf"
