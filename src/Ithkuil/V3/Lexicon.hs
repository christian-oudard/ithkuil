{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Lexicon
-- Loads root consonant definitions from data/v3_lexicon.dat.
-- Format: ROOT|definition (one per line, roots in uppercase)
module Ithkuil.V3.Lexicon
  ( V3Lexicon
  , loadV3Lexicon
  , lookupV3Root
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | V3 lexicon: maps root consonant clusters to definitions
type V3Lexicon = Map Text Text

-- | Load V3 lexicon from a pipe-delimited file.
-- Each line: ROOT|definition
loadV3Lexicon :: FilePath -> IO V3Lexicon
loadV3Lexicon path = do
  contents <- TIO.readFile path
  let entries = [ (T.toLower root, def)
                | line <- T.lines contents
                , not (T.null line)
                , let (root, rest) = T.breakOn "|" line
                , not (T.null rest)
                , let def = T.drop 1 rest
                ]
  return $ Map.fromList entries

-- | Look up a root in the V3 lexicon (case-insensitive)
lookupV3Root :: Text -> V3Lexicon -> Maybe Text
lookupV3Root root = Map.lookup (T.toLower root)
