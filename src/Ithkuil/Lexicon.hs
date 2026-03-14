{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Ithkuil V4 Lexicon Data
-- Loads from JSON files in data/
module Ithkuil.Lexicon
  ( RootEntry(..)
  , AffixEntry(..)
  , loadRoots
  , loadAffixes
  , lookupRoot
  , lookupAffix
  , rootsFromJSON
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

-- | Root entry from lexicon
data RootEntry = RootEntry
  { rootCr :: Text      -- Consonant form (e.g., "m", "rr", "lţk")
  , rootStem0 :: Text   -- Stem 0: generic/basic meaning
  , rootStem1 :: Text   -- Stem 1: specific meaning
  , rootStem2 :: Text   -- Stem 2: specific meaning
  , rootStem3 :: Text   -- Stem 3: specific meaning
  } deriving (Show, Eq, Generic)

instance FromJSON RootEntry where
  parseJSON = withObject "RootEntry" $ \v -> RootEntry
    <$> v .: "cr"
    <*> v .: "stem0"
    <*> v .: "stem1"
    <*> v .: "stem2"
    <*> v .: "stem3"

-- | Affix entry
data AffixEntry = AffixEntry
  { affixCs :: Text
  , affixAbbrev :: Text
  , affixDesc :: Text
  , affixType :: Text
  , affixDegrees :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON AffixEntry where
  parseJSON = withObject "AffixEntry" $ \v -> AffixEntry
    <$> v .: "cs"
    <*> v .: "abbrev"
    <*> v .: "description"
    <*> v .: "type"
    <*> v .: "degrees"

-- | Parse roots from JSON bytestring
rootsFromJSON :: BL.ByteString -> Either String (Map Text RootEntry)
rootsFromJSON bs = case eitherDecode bs of
  Left err -> Left err
  Right entries -> Right $ Map.fromList [(rootCr e, e) | e <- entries]

-- | Parse affixes from JSON bytestring
affixesFromJSON :: BL.ByteString -> Either String (Map Text AffixEntry)
affixesFromJSON bs = case eitherDecode bs of
  Left err -> Left err
  Right entries -> Right $ Map.fromList [(affixCs e, e) | e <- entries]

-- | Load roots from file path
loadRoots :: FilePath -> IO (Either String (Map Text RootEntry))
loadRoots path = rootsFromJSON <$> BL.readFile path

-- | Load affixes from file path
loadAffixes :: FilePath -> IO (Either String (Map Text AffixEntry))
loadAffixes path = affixesFromJSON <$> BL.readFile path

-- | Lookup a root by consonant form
lookupRoot :: Text -> Map Text RootEntry -> Maybe RootEntry
lookupRoot = Map.lookup

-- | Lookup an affix by consonant form
lookupAffix :: Text -> Map Text AffixEntry -> Maybe AffixEntry
lookupAffix = Map.lookup

