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
import Data.Monoid ((<>))
import Data.String.Conv (toS)

data Value
  = String Text
  | Object (InsOrdHashMap Text Value)
  deriving (Show)

makePrisms ''Value


parseSteamFile :: String -> IO Value
parseSteamFile fp = do
  result <- parseFromFile steamConfigParser fp
  case result of
    Nothing -> fail $ "Could not parse " <> fp
    Just v -> return v

steamConfigParser :: Parser Value
steamConfigParser = Object . InsOrd.fromList <$> many entry
  where
    entry = (,) <$> objectKey <*> objectValue
    objectKey = stringLiteral <?> "key"
    objectValue = String . toS <$> dummyStringLiteral <|> braces steamConfigParser

dummyStringLiteral :: TokenParsing m => m String
dummyStringLiteral = token $ between (char '"') (char '"') (many strChar)
  where strChar = '"' <$ text "\\\"" <|> notChar '"'

key :: Text -> Traversal' Value Value
key i = _Object . ix i
