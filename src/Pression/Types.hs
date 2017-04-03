{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pression.Types where

import Data.Binary

newtype GameId = GameId Integer deriving (Show, Binary)
