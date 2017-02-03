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

steamConfigParser :: Parser Value
steamConfigParser = Object . InsOrd.fromList <$> some entry
  where
    entry = (,) <$> key <*> value
    key = stringLiteral <?> "key"
    value = (String <$> stringLiteral) <|> braces steamConfigParser

_String :: Prism' Value Text
_String =
  prism String $ \v ->
    case v of
      String s -> Right s
      _ -> Left v

_Object :: Prism' Value (InsOrdHashMap Text Value)
_Object =
  prism
    Object
    (\v ->
       case v of
         Object o -> Right o
         _ -> Left v)

key :: Text -> Traversal' Value Value
key i = _Object . ix i
