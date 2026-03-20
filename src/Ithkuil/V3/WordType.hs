{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Word Type Detection
-- Determines whether a word is a formative, bias adjunct, or other type.
module Ithkuil.V3.WordType
  ( WordType(..)
  , detectWordType
  , parseWord
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Ithkuil.V3.Grammar
import Ithkuil.V3.Parse (CaTables, cbTableReverse, parseFormativeWithCa)
import qualified Ithkuil.V3.FullParse as FP

-- | V3 word types
data WordType
  = WFormative Formative   -- ^ A parsed formative
  | WBias Bias             -- ^ A bias adjunct (Cb consonant cluster)
  | WUnknown Text          -- ^ Could not determine word type
  deriving (Show, Eq)

-- | Detect the type of a V3 word
detectWordType :: CaTables -> Text -> WordType
detectWordType ca word
  | T.null word = WUnknown word
  -- Check if it's a bias adjunct (starts with glottal stop, all consonants)
  | isBiasAdjunct word = case Map.lookup (T.toLower word) cbTableReverse of
      Just b  -> WBias b
      Nothing -> WUnknown word
  -- Otherwise try to parse as formative
  | otherwise = case FP.parseFormative ca word of
      FP.Success f -> WFormative f
      FP.Failure _ -> WUnknown word

-- | Convenience: parse a word, returning either a formative or error
parseWord :: CaTables -> Text -> Either Text Formative
parseWord ca word = case FP.parseFormative ca word of
  FP.Success f -> Right f
  FP.Failure e -> Left e

-- | Check if a word looks like a bias adjunct
-- Bias adjuncts start with glottal stop (') and are pure consonant clusters
isBiasAdjunct :: Text -> Bool
isBiasAdjunct t = case T.uncons t of
  Just ('\x2019', rest) -> T.all isConsonantChar rest  -- Unicode right quote
  Just ('\'', rest)     -> T.all isConsonantChar rest   -- ASCII apostrophe
  _                     -> False

isConsonantChar :: Char -> Bool
isConsonantChar c = c `notElem` ("aâäeêëiîoôöuûüáéíóúàèìòùæøɨ " :: [Char])
  && c /= '\x2019' && c /= '\''
