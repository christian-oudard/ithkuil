{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Lexicon Data
-- Auto-generated from collaborative spreadsheet
module Ithkuil.Lexicon where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Root entry
data RootEntry = RootEntry
  { rootCr :: Text
  , rootStem0 :: Text  -- Basic/generic meaning
  , rootStem1 :: Text
  , rootStem2 :: Text
  , rootStem3 :: Text
  } deriving (Show, Eq)

-- | Affix entry
data AffixEntry = AffixEntry
  { affixCs :: Text
  , affixAbbrev :: Text
  , affixDesc :: Text
  , affixType :: Text  -- "1", "2", "3", "D1", "D2", "A1", "A2", "0"
  , affixDegrees :: [Text]  -- 9 degrees
  } deriving (Show, Eq)

-- | All roots indexed by consonant form
roots :: Map Text RootEntry
roots = Map.fromList
  [ ("b", RootEntry "b" "belief/doc" "believing" "faith/belief in doctrine" "dogma/unquestionable belief")
  -- ... (full data in JSON)
  ]

-- | All affixes indexed by consonant form
affixes :: Map Text AffixEntry
affixes = Map.fromList
  [ ("b", AffixEntry "b" "DEV" "Degree of Development" "D1" ["reversal/undoing", "reversal/undoing in large chunks", "reversal little by little", "moribund/stagnant", "well-maintained/well-kept", "one by one", "little by little", "by leaps and bounds", "complete & irreversible achievement"])
  -- ... (full data in JSON)
  ]

-- | Statistics
rootCount :: Int
rootCount = 4720

affixCount :: Int
affixCount = 527
