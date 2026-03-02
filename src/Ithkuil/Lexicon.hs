{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Ithkuil V4 Lexicon Data
-- Loads from JSON files in data/
module Ithkuil.Lexicon
  ( RootEntry(..)
  , AffixEntry(..)
  , V1RootEntry(..)
  , loadRoots
  , loadAffixes
  , loadV1Roots
  , lookupRoot
  , lookupAffix
  , lookupV1Root
  , rootsFromJSON
  , v1RootsFromJSON
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

-- | Display a root entry
showRoot :: RootEntry -> Text
showRoot r = T.unlines
  [ "Root: -" <> rootCr r <> "-"
  , "  Stem 0: " <> rootStem0 r
  , "  Stem 1: " <> rootStem1 r
  , "  Stem 2: " <> rootStem2 r
  , "  Stem 3: " <> rootStem3 r
  ]

--------------------------------------------------------------------------------
-- V1 Biliteral Roots
--------------------------------------------------------------------------------

-- | V1 root entry (biliteral roots)
data V1RootEntry = V1RootEntry
  { v1C1 :: Text      -- First consonant
  , v1C2 :: Text      -- Second consonant
  , v1Stem0 :: Text   -- Stem 0
  , v1Stem1 :: Text   -- Stem 1
  , v1Stem2 :: Text   -- Stem 2
  , v1Stem3 :: Text   -- Stem 3
  } deriving (Show, Eq, Generic)

instance FromJSON V1RootEntry where
  parseJSON = withObject "V1RootEntry" $ \v -> V1RootEntry
    <$> v .: "c1"
    <*> v .: "c2"
    <*> v .: "stem0"
    <*> v .: "stem1"
    <*> v .: "stem2"
    <*> v .: "stem3"

-- | Parse V1 roots from JSON bytestring
-- Key is "c1-c2" for biliteral lookup
v1RootsFromJSON :: BL.ByteString -> Either String (Map Text V1RootEntry)
v1RootsFromJSON bs = case eitherDecode bs of
  Left err -> Left err
  Right entries -> Right $ Map.fromList [(v1C1 e <> "-" <> v1C2 e, e) | e <- entries]

-- | Load V1 roots from file path
loadV1Roots :: FilePath -> IO (Either String (Map Text V1RootEntry))
loadV1Roots path = v1RootsFromJSON <$> BL.readFile path

-- | Lookup a V1 biliteral root by "c1-c2" key
lookupV1Root :: Text -> Text -> Map Text V1RootEntry -> Maybe V1RootEntry
lookupV1Root c1 c2 = Map.lookup (c1 <> "-" <> c2)
