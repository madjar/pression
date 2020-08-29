{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fmt
import Pression.Library
import Pression.Move

freeSize :: Integer
freeSize = 100 * 1024 * 1024 * 1024

main :: IO ()
main =
  steamappsDirs >>= \case
    [hotPath, coldPath] -> do
      fmtLn ("Hot: " +| hotPath |+ "")
      fmtLn ("Cold: " +| coldPath |+ "")
      shuffleResult <- gamesToShuffleAround hotPath coldPath freeSize
      printShuffleResult shuffleResult
      shuffleAround shuffleResult
    _ -> fmtLn "Error, could not find two steam libraries"
