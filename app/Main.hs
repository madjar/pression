{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Pression.Library

main :: IO ()
main = randomGame >>= runGame
