{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pression.Parser where

import Control.Applicative
import Control.Lens
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (Text)
import Text.Trifecta

data Value
  = String Text
  | Object (InsOrdHashMap Text Value)
  deriving (Show)

makePrisms ''Value


steamConfigParser :: Parser Value
steamConfigParser = Object . InsOrd.fromList <$> some entry
  where
    entry = (,) <$> key <*> value
    key = stringLiteral <?> "key"
    value = (String <$> stringLiteral) <|> braces steamConfigParser


key :: Text -> Traversal' Value Value
key i = _Object . ix i
