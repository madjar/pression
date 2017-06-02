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
import Data.Conduit.Network
import Network.Multicast
import Network.Socket (SockAddr)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.ByteString.Lazy (toStrict, fromStrict)

import Control.Concurrent (threadDelay)

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

sendGame ::
  MonadResource m => FilePath -> GameId -> Producer m ByteString
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

recvTree ::
  MonadResource m => FilePath -> Consumer ByteString m ()
recvTree root = peekForeverE (recvFile root)

steamFileServer :: FilePath -> Int -> IO a
steamFileServer root port =
  runTCPServer settings $ \appData ->
    runConduitRes $ appSource appData .| serverConduit .| appSink appData
  where
    settings = serverSettings port "*"
    serverConduit = do
      game <- sinkGet get
      sendGame root game

steamServerAnnounce :: FilePath -> Int -> IO b
steamServerAnnounce root port = do
  (sock, addr) <- multicastSender "224.0.0.99" 9999
  let loop = do
        games <- listDirectory root
        let msg = (port, games) :: (Int, [String])
        _ <- sendTo sock (toStrict $ encode msg) addr
        threadDelay 1000000
        loop
  loop


discoverSteamServer :: IO (SockAddr, Int, [String])
discoverSteamServer = do
  sock <- multicastReceiver "224.0.0.99" 9999
  (msg, addr) <- recvFrom sock 1024
  print msg
  let (port, games) = decode (fromStrict msg) :: (Int, [String])
  return (addr, port, games)

steamFileClient :: FilePath -> ByteString -> Int -> GameId -> IO ()
steamFileClient root server port game =
  runTCPClient settings $ \appData -> do
    runConduit $ sourcePut (put game) .| appSink appData
    runConduitRes $ appSource appData .| recvTree root
  where
    settings = clientSettings port server

--TODO don't forget withSocketsDo
