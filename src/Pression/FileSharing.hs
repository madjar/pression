{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Pression.FileSharing where

import Pression.Types
import Pression.Parser

import Conduit
import Data.Conduit.Serialization.Binary
import Data.Binary
import System.FilePath
import System.Directory
import System.IO             (IOMode (ReadMode), hFileSize, openBinaryFile, hClose)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Control.Lens
import Data.Monoid
import Data.List

sendFile ::
  MonadResource m => FilePath -> Producer m ByteString
sendFile fp = do
  bracketP (openBinaryFile fp ReadMode) hClose $ \h -> do
    size <- liftIO $ hFileSize h
    sourcePut (put (fromInteger size :: Int))
    sourceHandle h

sendFileAs :: MonadResource m => FilePath -> FilePath -> Producer m ByteString
sendFileAs fp name = do
  sourcePut (put name)
  sendFile fp

sendGame root (GameId game) = do
  let gameDir = root </> show game
      manifestRelPath = "steamapps" </> "appmanifest_" <> show game <> ".acf"
      manifestFile = gameDir </> manifestRelPath
  -- Read the manifest to find out the install dir
  manifest <- liftIO $ parseSteamFile manifestFile
  let installdir =
        manifest ^. key "AppState" . key "installdir" . _String . to T.unpack
  -- Send the manifest
  sendFileAs manifestFile manifestRelPath
  -- Send the rest (but not the steamapps dir)
  sourceDirectoryDeep False root .|
    filterC (\fp -> not ("steamapps" `isPrefixOf` fp)) .|
    awaitForever
      (\fp ->
         sendFileAs
           fp
           ("steamapps" </> "common" </> installdir </> makeRelative gameDir fp))


recvFile ::
  MonadResource m => FilePath -> Consumer ByteString m ()
recvFile root = do
  fpRel <- sinkGet get
  let fp = root </> fpRel
  liftIO $ createDirectoryIfMissing True $ takeDirectory fp

  fileLen <- sinkGet get
  takeCE fileLen .| sinkFile fp

recvTree root = peekForeverE (recvFile root)

--steamFileServer
