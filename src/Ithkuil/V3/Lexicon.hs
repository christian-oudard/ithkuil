{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Lexicon
-- Loads root consonant definitions from data/v3_lexicon.dat.
-- Format: ROOT|definition (one per line, roots in uppercase)
module Ithkuil.V3.Lexicon
  ( Lexicon
  , loadLexicon
  , lookupRoot
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | V3 lexicon: maps root consonant clusters to definitions
type Lexicon = Map Text Text

-- | Load V3 lexicon from a pipe-delimited file.
-- Each line: ROOT|definition
loadLexicon :: FilePath -> IO Lexicon
loadLexicon path = do
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
lookupRoot :: Text -> Lexicon -> Maybe Text
lookupRoot root = Map.lookup (T.toLower root)
