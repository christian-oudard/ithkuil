{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Phonotactic Validation
-- Rules for valid consonant clusters and vowel sequences
module Ithkuil.Validation where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Validation Results
--------------------------------------------------------------------------------

data ValidationResult
  = Valid
  | Invalid [ValidationError]
  deriving (Show, Eq)

data ValidationError
  = InvalidConsonantCluster Text Text  -- location, cluster
  | InvalidVowelSequence Text Text     -- location, sequence
  | MissingRequiredSlot Text           -- slot name
  | InvalidSlotValue Text Text Text    -- slot, value, reason
  | PhonemeNotAllowed Text Text        -- phoneme, context
  deriving (Show, Eq)

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

--------------------------------------------------------------------------------
-- Consonant Cluster Rules
--------------------------------------------------------------------------------

-- | Maximum consonant cluster length by position
maxClusterLength :: Position -> Int
maxClusterLength Initial = 4   -- Word-initial
maxClusterLength Medial = 5    -- Word-medial (between vowels)
maxClusterLength Final = 4     -- Word-final

data Position = Initial | Medial | Final
  deriving (Show, Eq, Ord)

-- | Invalid initial clusters (cannot start a word)
invalidInitialClusters :: Set Text
invalidInitialClusters = Set.fromList
  [ "dl", "tl"  -- dental + lateral
  , "dn", "tn"  -- dental + nasal (same place)
  , "bm", "pm"  -- labial + labial nasal
  , "gň", "kň"  -- velar + velar nasal
  , "rl", "lr"  -- liquid + liquid
  , "mn", "nm"  -- nasal + nasal
  ]

-- | Invalid final clusters
invalidFinalClusters :: Set Text
invalidFinalClusters = Set.fromList
  [ "kt", "pt", "bt", "gt"  -- stop + stop
  , "fs", "vs"              -- labio-dental + alveolar fric
  ]

-- | Check if a consonant cluster is valid
validateCluster :: Position -> Text -> Maybe ValidationError
validateCluster pos cluster
  | T.length cluster > maxClusterLength pos =
      Just $ InvalidConsonantCluster (showPos pos) cluster
  | pos == Initial && cluster `Set.member` invalidInitialClusters =
      Just $ InvalidConsonantCluster "initial" cluster
  | pos == Final && cluster `Set.member` invalidFinalClusters =
      Just $ InvalidConsonantCluster "final" cluster
  | otherwise = checkClusterPhonetics pos cluster
  where
    showPos Initial = "initial"
    showPos Medial  = "medial"
    showPos Final   = "final"

-- | Check phonetic validity of cluster
checkClusterPhonetics :: Position -> Text -> Maybe ValidationError
checkClusterPhonetics _ cluster
  -- No triphthong stops
  | hasTripleStop cluster =
      Just $ InvalidConsonantCluster "phonetics" cluster
  -- No sibilant + sibilant without stop
  | hasDoubleSibilant cluster =
      Just $ InvalidConsonantCluster "sibilants" cluster
  | otherwise = Nothing

hasTripleStop :: Text -> Bool
hasTripleStop t = any (tripleStops `T.isInfixOf`) [t]
  where
    tripleStops = "ppp" -- Simplified check

hasDoubleSibilant :: Text -> Bool
hasDoubleSibilant t =
  let sibilants = "sšzžcč" :: String
      chars = T.unpack t
      sibilantCount = length $ filter (`elem` sibilants) chars
  in sibilantCount >= 2 && not (any (`elem` ("tdkg" :: String)) chars)

--------------------------------------------------------------------------------
-- Vowel Sequence Rules
--------------------------------------------------------------------------------

-- | Valid vowel sequences (diphthongs and triphthongs)
validDiphthongs :: Set Text
validDiphthongs = Set.fromList
  [ "ai", "au", "ei", "eu", "ëi", "ou", "oi", "iu", "ui"
  , "ia", "iä", "ie", "ië", "ëu", "uö", "uo", "ue", "ua"
  , "ao", "ae", "ea", "eo", "eë", "öe", "oe", "öa", "oa"
  ]

-- | Invalid vowel sequences
invalidVowelSequences :: Set Text
invalidVowelSequences = Set.fromList
  [ "ii", "uu", "aa"  -- Same vowel repeated
  , "äi", "öi", "üi"  -- Umlauted + i
  , "iü", "uü"        -- High + ü
  ]

-- | Check if a vowel sequence is valid
validateVowelSeq :: Text -> Maybe ValidationError
validateVowelSeq seq
  | T.length seq == 1 = Nothing  -- Single vowels always valid
  | seq `Set.member` validDiphthongs = Nothing
  | seq `Set.member` invalidVowelSequences =
      Just $ InvalidVowelSequence "prohibited" seq
  | T.length seq > 3 =
      Just $ InvalidVowelSequence "too long" seq
  | otherwise = Nothing  -- Allow other sequences by default

--------------------------------------------------------------------------------
-- Formative Validation
--------------------------------------------------------------------------------

-- | Validate a complete formative
validateFormative :: Text -> ValidationResult
validateFormative word =
  let parts = splitIntoParts word
      errors = concatMap validatePart parts
  in if null errors then Valid else Invalid errors

-- | Split word into consonant/vowel parts
splitIntoParts :: Text -> [(Position, PartType, Text)]
splitIntoParts t =
  let groups = T.groupBy sameType t
  in zipWith3 annotatePart [0..] (determinePositions groups) (classifyParts groups)
  where
    sameType a b = isVowel a == isVowel b
    isVowel c = c `elem` ("aäeëiöoüu" :: String)
    annotatePart _ pos (ptype, txt) = (pos, ptype, txt)
    determinePositions gs =
      let n = length gs
      in [if i == 0 then Initial else if i == n-1 then Final else Medial | i <- [0..n-1]]
    classifyParts gs = [(if isVowelPart g then VowelPart else ConsonantPart, g) | g <- gs]
    isVowelPart g = not (T.null g) && T.head g `elem` ("aäeëiöoüu" :: String)

data PartType = ConsonantPart | VowelPart
  deriving (Show, Eq)

-- | Validate a single part
validatePart :: (Position, PartType, Text) -> [ValidationError]
validatePart (pos, ConsonantPart, txt) =
  maybe [] (:[]) $ validateCluster pos txt
validatePart (_, VowelPart, txt) =
  maybe [] (:[]) $ validateVowelSeq txt

--------------------------------------------------------------------------------
-- External Juncture Validation
--------------------------------------------------------------------------------

-- | Check external juncture (word boundaries)
-- Certain sequences are invalid across word boundaries
validateExternalJuncture :: Text -> Text -> Maybe ValidationError
validateExternalJuncture word1 word2 =
  let ending = T.takeEnd 1 word1
      beginning = T.take 1 word2
      juncture = ending <> beginning
  in if juncture `Set.member` invalidJunctures
     then Just $ InvalidConsonantCluster "juncture" juncture
     else Nothing

invalidJunctures :: Set Text
invalidJunctures = Set.fromList
  [ "ťť", "ḑḑ"  -- Same dental fricative
  , "çç"         -- Double palatal
  ]

--------------------------------------------------------------------------------
-- Stress Validation
--------------------------------------------------------------------------------

-- | Validate stress placement
data StressPattern
  = Penultimate    -- Default (unmarked)
  | Ultimate       -- Final syllable stressed
  | Antepenultimate-- Third from end stressed
  deriving (Show, Eq, Ord)

-- | Check if stress is valid for word type
validateStress :: StressPattern -> Int -> Maybe ValidationError
validateStress _ syllables
  | syllables < 1 = Just $ InvalidSlotValue "stress" "none" "no syllables"
validateStress Antepenultimate syllables
  | syllables < 3 = Just $ InvalidSlotValue "stress" "antepenultimate" "too few syllables"
validateStress _ _ = Nothing
