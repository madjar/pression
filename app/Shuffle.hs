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
    [hot, cold] -> do
      fmtLn ("Hot: " +| hot |+ "")
      fmtLn ("Cold: " +| cold |+ "")
      shuffleAround hot cold freeSize
    _ -> fmtLn "Error, could not find two steam libraries"
