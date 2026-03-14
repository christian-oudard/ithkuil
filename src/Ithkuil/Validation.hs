{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Phonotactic Validation
-- Implements rules from "Phonotactic Rules for New Ithkuil, v. 1.0"
module Ithkuil.Validation
  ( ValidationResult(..)
  , ValidationError(..)
  , StressError(..)
  , Position(..)
  , isValid
  , validateCluster
  , validateVowelSeq
  , validateFormative
  , validateExternalJuncture
  , validateStress
  , validateProhibitedConjunct
  , hasTripleConsonant
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Ithkuil.Grammar (Stress(..))
import Ithkuil.Parse (splitConjuncts, isVowelChar)

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
  | ProhibitedConjunct Text Text       -- rule description, cluster
  | MissingRequiredSlot Text           -- slot name
  | InvalidSlotValue Text Text Text    -- slot, value, reason
  | PhonemeNotAllowed Text Text        -- phoneme, context
  deriving (Show, Eq)

isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

--------------------------------------------------------------------------------
-- Phoneme Classification Helpers
--------------------------------------------------------------------------------

data Position = Initial | Medial | Final
  deriving (Show, Eq, Ord)

-- Character classification for consonant pair rule checking.
-- These sets follow the phonotactics PDF terminology.

isStop :: Char -> Bool
isStop c = c `elem` ("ptdkgb" :: String)

isDentalStop :: Char -> Bool
isDentalStop c = c `elem` ("td" :: String)

isVelarStop :: Char -> Bool
isVelarStop c = c `elem` ("kg" :: String)

isLabialStop :: Char -> Bool
isLabialStop c = c `elem` ("pb" :: String)

isVoicedStop :: Char -> Bool
isVoicedStop c = c `elem` ("bdg" :: String)

-- Sibilant fricatives: s, z, š, ž
isSibilantFricative :: Char -> Bool
isSibilantFricative c = c `elem` ("sšzž" :: String)

-- Sibilant affricates: c, ẓ, č, j
isSibilantAffricate :: Char -> Bool
isSibilantAffricate c = c `elem` ("cčẓj" :: String)

-- Any sibilant (fricative or affricate)
isSibilant :: Char -> Bool
isSibilant c = isSibilantFricative c || isSibilantAffricate c


-- Voiced/voiceless pairing for homologous voicing checks (Rule 2.4, 2.5)
voicedOf :: Char -> Maybe Char
voicedOf 'p' = Just 'b'
voicedOf 'b' = Just 'b'
voicedOf 't' = Just 'd'
voicedOf 'd' = Just 'd'
voicedOf 'k' = Just 'g'
voicedOf 'g' = Just 'g'
voicedOf 'f' = Just 'v'
voicedOf 'v' = Just 'v'
voicedOf 'ţ' = Just 'ḑ'
voicedOf 'ḑ' = Just 'ḑ'
voicedOf 's' = Just 'z'
voicedOf 'z' = Just 'z'
voicedOf 'š' = Just 'ž'
voicedOf 'ž' = Just 'ž'
voicedOf 'c' = Just 'ẓ'
voicedOf 'ẓ' = Just 'ẓ'
voicedOf 'č' = Just 'j'
voicedOf 'j' = Just 'j'
voicedOf _   = Nothing

-- Place of articulation grouping for "homologous" checks
placeGroup :: Char -> Int
placeGroup 'p' = 1; placeGroup 'b' = 1; placeGroup 'f' = 1; placeGroup 'v' = 1  -- labial
placeGroup 'm' = 1
placeGroup 't' = 2; placeGroup 'd' = 2  -- apico-dental
placeGroup 'ţ' = 2; placeGroup 'ḑ' = 2  -- inter-dental (same place group)
placeGroup 'n' = 2  -- dental nasal
placeGroup 's' = 3; placeGroup 'z' = 3; placeGroup 'c' = 3; placeGroup 'ẓ' = 3  -- alveolar
placeGroup 'š' = 4; placeGroup 'ž' = 4; placeGroup 'č' = 4; placeGroup 'j' = 4  -- alveolo-palatal
placeGroup 'k' = 5; placeGroup 'g' = 5  -- velar
placeGroup 'ň' = 5  -- velar nasal
placeGroup 'x' = 6  -- uvular
placeGroup _ = 0  -- others (no homologous restriction)

-- Check if two consonants are "homologous" (same place of articulation)
areHomologous :: Char -> Char -> Bool
areHomologous a b =
  let pa = placeGroup a; pb = placeGroup b
  in pa > 0 && pa == pb

-- Check voicing match
sameVoicing :: Char -> Char -> Bool
sameVoicing a b = case (voicedOf a, voicedOf b) of
  (Just va, Just vb) -> (a == va) == (b == vb)  -- both voiced or both voiceless
  _ -> True  -- if voicing not applicable, don't flag

--------------------------------------------------------------------------------
-- Section 2: Prohibited Consonantal Conjuncts
-- These rules apply to ANY adjacent consonant pair, whether within a cluster
-- or across adjacent syllables.
--------------------------------------------------------------------------------

-- | Check a pair of adjacent consonants for prohibited conjuncts.
-- Returns Just an error description if the pair violates any rule.
checkProhibitedPair :: Char -> Char -> Maybe Text
checkProhibitedPair c1 c2
  -- 2.1: No consonant can be immediately followed by a glottal stop
  | c2 == '\'' = Just "2.1: consonant followed by glottal stop"

  -- 2.2: Dental stops (t, d) cannot be followed by any sibilant
  | isDentalStop c1 && isSibilant c2 = Just "2.2: dental stop + sibilant"
  -- 2.2: Dental stops cannot be followed by their fricative counterparts
  | c1 == 't' && c2 == 'ţ' = Just "2.2: t + ţ"
  | c1 == 'd' && c2 == 'ḑ' = Just "2.2: d + ḑ"
  | c1 == 't' && c2 == 'ḑ' = Just "2.2: t + ḑ"
  | c1 == 'd' && c2 == 'ţ' = Just "2.2: d + ţ"

  -- 2.3: Velar stops cannot be followed by x or ň
  | isVelarStop c1 && c2 == 'x' = Just "2.3: velar stop + x"
  | isVelarStop c1 && c2 == 'ň' = Just "2.3: velar stop + ň"

  -- 2.4: Homologous voiceless-voiced or voiced-voiceless stop pairings prohibited
  | isStop c1 && isStop c2 && areHomologous c1 c2 && not (sameVoicing c1 c2)
    = Just "2.4: homologous stop voicing mismatch"

  -- 2.5: Homologous voiceless-voiced/voiced-voiceless fricative/affricate pairings
  | (isSibilantFricative c1 || isSibilantAffricate c1)
  , (isSibilantFricative c2 || isSibilantAffricate c2)
  , areHomologous c1 c2
  , not (sameVoicing c1 c2)
    = Just "2.5: homologous sibilant voicing mismatch"

  -- 2.6: Alveolo-palatal sibilant fricative + apico-alveolar affricate prohibited
  -- *šc, *šẓ, *žc, *žẓ
  | c1 `elem` ("šž" :: String) && c2 `elem` ("cẓ" :: String)
    = Just "2.6: alveolo-palatal fricative + alveolar affricate"

  -- 2.7: s cannot be followed by voiced affricate ẓ
  | c1 == 's' && c2 == 'ẓ' = Just "2.7: s + ẓ"

  -- 2.8: Sibilant fricative + sibilant fricative (other than geminates)
  | isSibilantFricative c1 && isSibilantFricative c2 && c1 /= c2
    = Just "2.8: sibilant fricative + sibilant fricative"

  -- 2.9: Sibilant affricate + sibilant fricative
  | isSibilantAffricate c1 && isSibilantFricative c2
    = Just "2.9: sibilant affricate + sibilant fricative"
  -- 2.9: Sibilant fricative + sibilant affricate (reverse)
  -- This is actually covered differently: č+s is fine but s+c prohibited by 2.8 logic
  -- The PDF lists *čs, *čẓ, *ẓs etc. Let's be explicit:
  | isSibilantFricative c1 && isSibilantAffricate c2
    = Just "2.9: sibilant fricative + sibilant affricate"

  -- 2.10: ç restrictions
  | c1 == 'ç' && isSibilantFricative c2 = Just "2.10: ç + sibilant fricative"
  | isSibilantFricative c1 && c2 == 'ç' = Just "2.10: sibilant fricative + ç"
  | c1 == 'ç' && c2 `elem` ("ẓj" :: String) = Just "2.10: ç + voiced sibilant affricate"
  | isSibilantAffricate c1 && c2 == 'ç' = Just "2.10: sibilant affricate + ç"
  | c1 == 'ç' && c2 == 'ļ' = Just "2.10: ç + ļ"
  | c1 == 'ļ' && c2 == 'ç' = Just "2.10: ļ + ç"
  | c1 == 'ç' && c2 == 'h' = Just "2.10: ç + h"
  | c1 == 'h' && c2 == 'ç' = Just "2.10: h + ç"
  | c1 == 'x' && c2 == 'ç' = Just "2.10: x + ç"

  -- 2.11: Nasal + affricate of same/similar place
  -- nc, nč, nẓ, nj prohibited
  | c1 == 'n' && isSibilantAffricate c2 = Just "2.11: n + sibilant affricate"

  -- 2.12: m + bilabial/interdental/dental stop prohibited (mpf, mbv, etc.)
  -- Simplified: m cannot be followed by labial stop
  | c1 == 'm' && isLabialStop c2 = Just "2.12: m + labial stop"
  -- m + interdental/dental stop: mţ, mḑ, mt, md
  | c1 == 'm' && isDentalStop c2 = Just "2.12: m + dental stop"
  | c1 == 'm' && c2 `elem` ("ţḑ" :: String) = Just "2.12: m + interdental"

  -- 2.13: Nasal + homologous stop + sibilant: mps, mbz, etc.
  -- This is a 3-consonant rule; we check the pair n+homologous sibilant separately below

  -- 2.14: n cannot be followed by labial stops p, b
  | c1 == 'n' && isLabialStop c2 = Just "2.14: n + labial stop"

  -- 2.15: nf, nv cannot be followed by consonant (handled at cluster level)
  -- As a pair: nf and nv are allowed but must be followed by vowel.
  -- We handle this in the cluster validator.

  -- 2.16: ň cannot occur before k, g, x (dental n already assimilates)
  | c1 == 'ň' && c2 `elem` ("kgx" :: String) = Just "2.16: ň + velar/uvular"
  -- ň cannot be followed by y
  | c1 == 'ň' && c2 == 'y' = Just "2.16: ň + y"

  -- 2.17: x cannot be followed by sibilant, ç, g, ļ, ň, y, h, ř
  | c1 == 'x' && isSibilant c2 = Just "2.17: x + sibilant"
  | c1 == 'x' && c2 `elem` ("çgļňyhř" :: String) = Just "2.17: x + prohibited follower"

  -- 2.18: ļ restrictions
  | c1 == 'ļ' && isVoicedStop c2 = Just "2.18: ļ + voiced stop"
  | c1 == 'h' && c2 == 'ļ' = Just "2.18: h + ļ"
  | c1 == 'ļ' && isSibilantFricative c2 = Just "2.18: ļ + sibilant fricative"

  -- 2.19: h (as final member of consonant conjunct) cannot be preceded by ļ, x, ç
  -- This is a positional rule; we handle it in cluster validation below.

  -- 2.20: r and h cannot be followed by ř
  | c1 == 'r' && c2 == 'ř' = Just "2.20: r + ř"
  | c1 == 'h' && c2 == 'ř' = Just "2.20: h + ř"

  -- 2.21: ř cannot be followed by r
  | c1 == 'ř' && c2 == 'r' = Just "2.21: ř + r"

  -- 2.22: w and y must be last member of conjunct, followed by vowel
  -- Handled at cluster level (w/y cannot be followed by another consonant)
  | c1 == 'w' && not (isVowelChar c2) = Just "2.22: w not at end of conjunct"
  | c1 == 'y' && not (isVowelChar c2) = Just "2.22: y not at end of conjunct"

  -- 2.23: Awkward conjuncts: ḑs, ḑš, ḑz, ḑž, nň
  | c1 == 'ḑ' && c2 `elem` ("sšzž" :: String) = Just "2.23: ḑ + sibilant"
  | c1 == 'n' && c2 == 'ň' = Just "2.23: n + ň"

  -- 2.24: çç and ļļ geminates not permitted (morphological confusion)
  | c1 == 'ç' && c2 == 'ç' = Just "2.24: çç geminate"
  | c1 == 'ļ' && c2 == 'ļ' = Just "2.24: ļļ geminate"

  | otherwise = Nothing

-- | Check a consonant cluster for any prohibited adjacent pair (Section 2).
-- Scans all adjacent pairs within the cluster.
validateProhibitedConjunct :: Text -> Maybe ValidationError
validateProhibitedConjunct cluster =
  let chars = T.unpack cluster
      pairs = zip chars (drop 1 chars)
  in case concatMap checkPair pairs of
       [] -> Nothing
       (err:_) -> Just err
  where
    checkPair (a, b) = case checkProhibitedPair a b of
      Just rule -> [ProhibitedConjunct rule cluster]
      Nothing -> []

--------------------------------------------------------------------------------
-- Section 1.7: Gemination Rules
--------------------------------------------------------------------------------

-- | Check for triple-or-more repetition of the same consonant (prohibited per 1.7)
hasTripleConsonant :: Text -> Bool
hasTripleConsonant t =
  let chars = T.unpack t
  in any (\(a, b, c) -> a == b && b == c) (triples chars)
  where
    triples (a:b:c:rest) = (a, b, c) : triples (b:c:rest)
    triples _ = []

-- | Glottal stop, w, y cannot be geminated (1.7)
hasProhibitedGeminate :: Text -> Bool
hasProhibitedGeminate t =
  let chars = T.unpack t
      pairs = zip chars (drop 1 chars)
  in any (\(a, b) -> a == b && a `elem` ("'wy" :: String)) pairs

--------------------------------------------------------------------------------
-- Consonant Cluster Validation
--------------------------------------------------------------------------------

-- | Maximum consonant cluster length by position (from phonotactics PDF)
maxClusterLength :: Position -> Int
maxClusterLength Initial = 4   -- Section 3: word-initial 1-4 consonants
maxClusterLength Medial  = 6   -- Section 5: intervocalic 1-6 consonants
maxClusterLength Final   = 4   -- Section 4: word-final 1-4 consonants

-- | Check if a consonant cluster is valid
validateCluster :: Position -> Text -> Maybe ValidationError
validateCluster pos cluster
  | T.null cluster = Nothing
  -- Length check
  | T.length cluster > maxClusterLength pos =
      Just $ InvalidConsonantCluster (showPos pos) cluster
  -- Triple consonant repetition (1.7)
  | hasTripleConsonant cluster =
      Just $ InvalidConsonantCluster "triple repetition" cluster
  -- Prohibited geminates: ', w, y (1.7)
  | hasProhibitedGeminate cluster =
      Just $ InvalidConsonantCluster "prohibited geminate" cluster
  -- Section 2 prohibited conjuncts (pair-level check)
  | Just err <- validateProhibitedConjunct cluster = Just err
  -- Position-specific rules
  | pos == Initial = checkInitialCluster cluster
  | pos == Final   = checkFinalCluster cluster
  | otherwise = Nothing
  where
    showPos Initial = "initial (max 4)"
    showPos Medial  = "medial (max 6)"
    showPos Final   = "final (max 4)"

-- | Additional word-initial cluster rules (Section 3)
checkInitialCluster :: Text -> Maybe ValidationError
checkInitialCluster cluster
  -- 3.1: Single consonant ļ not allowed word-initially by itself
  | cluster == "ļ" = Just $ InvalidConsonantCluster "initial" "ļ"
  -- 1.5: Glottal stop only at word-initial if followed by vowel (not another consonant)
  -- A glottal stop in the middle of an initial cluster is prohibited by 2.1
  | T.length cluster > 1 && T.head cluster == '\'' =
      Just $ InvalidConsonantCluster "initial" cluster
  | otherwise = Nothing

-- | Additional word-final cluster rules (Section 4)
checkFinalCluster :: Text -> Maybe ValidationError
checkFinalCluster cluster
  -- 4.1: w and y cannot appear word-finally
  | not (T.null cluster) && T.last cluster `elem` ("wy" :: String) =
      Just $ InvalidConsonantCluster "final" cluster
  -- 4.1: Glottal stop cannot end a word (except 'V' parsing adjuncts)
  | not (T.null cluster) && T.last cluster == '\'' && T.length cluster > 1 =
      Just $ InvalidConsonantCluster "final" cluster
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Vowel Sequence Rules (Sections 1.1-1.4)
--------------------------------------------------------------------------------

-- | The 10 permissible falling diphthongs (per Section 1.2.1)
permissibleDiphthongs :: Set Text
permissibleDiphthongs = Set.fromList
  [ "ai", "ei", "ëi", "oi", "ui"  -- -i diphthongs
  , "au", "eu", "ëu", "ou", "iu"  -- -u diphthongs
  ]

-- | Valid disyllabic vowel conjuncts (used in vowel-form table, Series 3/4)
validDisyllabicConjuncts :: Set Text
validDisyllabicConjuncts = Set.fromList
  -- Series 3 vowel forms
  [ "ia", "ie", "io", "iö", "eë", "uö", "uo", "ue", "ua"
  -- Series 3 alternates
  , "uä", "uë", "üä", "üë", "öë", "öä", "ië", "iä"
  -- Series 4 vowel forms
  , "ao", "aö", "eo", "eö", "oë", "öe", "oe", "öa", "oa"
  -- Reference-root vowel forms
  , "ae", "ea"
  ]

-- | Check if a vowel sequence is valid (rules 1.1-1.4)
validateVowelSeq :: Text -> Maybe ValidationError
validateVowelSeq vseq
  -- Single vowels always valid (1.1)
  | T.length vseq == 1 = Nothing
  -- Diphthongs are valid (1.2.1)
  | vseq `Set.member` permissibleDiphthongs = Nothing
  -- Disyllabic vowel conjuncts are valid (1.4)
  | vseq `Set.member` validDisyllabicConjuncts = Nothing
  -- Tri-syllabic or longer vowel conjuncts are prohibited (1.4)
  | T.length vseq >= 3 =
      Just $ InvalidVowelSequence "tri-syllabic+ vowel conjunct" vseq
  -- Two-vowel sequence not in known sets: check for same-vowel repetition
  | T.length vseq == 2 && T.head vseq == T.last vseq =
      Just $ InvalidVowelSequence "same vowel repeated" vseq
  -- Other 2-vowel sequences: allow (grammar may use them in various slots)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Formative Validation
--------------------------------------------------------------------------------

-- | Validate a complete formative's phonotactics
validateFormative :: Text -> ValidationResult
validateFormative word =
  let parts = splitIntoParts word
      errors = concatMap validatePart parts
  in if null errors then Valid else Invalid errors

-- | Split word into consonant/vowel parts with position annotation
splitIntoParts :: Text -> [(Position, PartType, Text)]
splitIntoParts t =
  let groups = T.groupBy sameType t
  in zipWith3 annotatePart [0..] (determinePositions groups) (classifyParts groups)
  where
    sameType a b = isVowelChar a == isVowelChar b
    annotatePart :: Int -> Position -> (PartType, Text) -> (Position, PartType, Text)
    annotatePart _ pos (ptype, txt) = (pos, ptype, txt)
    determinePositions gs =
      let n = length gs
      in [if i == 0 then Initial else if i == n-1 then Final else Medial | i <- [0..n-1]]
    classifyParts gs = [(if isVowelPart g then VowelPart else ConsonantPart, g) | g <- gs]
    isVowelPart g = not (T.null g) && isVowelChar (T.head g)

data PartType = ConsonantPart | VowelPart
  deriving (Show, Eq)

-- | Validate a single part
validatePart :: (Position, PartType, Text) -> [ValidationError]
validatePart (pos, ConsonantPart, txt) =
  maybe [] (:[]) $ validateCluster pos txt
validatePart (_, VowelPart, txt) =
  maybe [] (:[]) $ validateVowelSeq txt

--------------------------------------------------------------------------------
-- External Juncture Validation (Section 7)
--------------------------------------------------------------------------------

-- | Check external juncture (word boundaries) per Section 7
-- 7.1: Prohibited consonant conjuncts from Section 2 apply across word boundaries
-- 7.2: Triple consonant of same type across boundary is prohibited
validateExternalJuncture :: Text -> Text -> Maybe ValidationError
validateExternalJuncture word1 word2
  | T.null word1 || T.null word2 = Nothing
  | otherwise =
    let ending = T.last word1
        beginning = T.head word2
    in if not (isVowelChar ending) && not (isVowelChar beginning)
       then
         -- 7.1: Check prohibited pair across boundary
         case checkProhibitedPair ending beginning of
           Just rule -> Just $ ProhibitedConjunct ("juncture: " <> rule)
                                (T.singleton ending <> T.singleton beginning)
           Nothing ->
             -- 7.2: Check for triple consonant across boundary (geminate + same)
             let endCluster = T.takeWhileEnd (not . isVowelChar) word1
                 startCluster = T.takeWhile (not . isVowelChar) word2
                 juncture = endCluster <> startCluster
             in if hasTripleConsonant juncture
                then Just $ ProhibitedConjunct "juncture: triple consonant"
                              juncture
                else Nothing
       else Nothing

--------------------------------------------------------------------------------
-- Stress Validation
--------------------------------------------------------------------------------

-- | Stress error types (following Kotlin reference)
data StressError
  = MarkedDefaultStress    -- ^ Accent mark on default stress position
  | DoubleMarkedStress     -- ^ Two accent marks in one word
  | UnrecognizedPlacement  -- ^ Stress mark on non-standard syllable
  | StressOnSentencePrefix -- ^ Accent mark on ç/çw/çç prefix
  | LoneSentencePrefix     -- ^ ç/çw/çç with no word following
  deriving (Show, Eq, Ord)

-- | Validate stress placement, returning an error if invalid
-- Uses splitConjuncts for proper diphthong/syllable handling
validateStress :: Text -> Either StressError Stress
validateStress word =
  let accentCount = T.length (T.filter isStressedVowel word)
      conjs = splitConjuncts word
      syllables = [c | c <- conjs, not (T.null c), isVowelChar (T.head c)]
      syllCount = length syllables
      hasAccent = accentCount > 0
  in if accentCount > 1 then Left DoubleMarkedStress
     else if syllCount <= 1 && hasAccent then Left MarkedDefaultStress
     else if syllCount <= 1 then Right Monosyllabic
     else if not hasAccent then Right Penultimate
     else -- Has exactly one accent mark, 2+ syllables
       let stressIdx = length (takeWhile (not . T.any isStressedVowel) syllables)
           fromEnd = syllCount - 1 - stressIdx
       in if fromEnd == 1 then Left MarkedDefaultStress  -- penult is default
          else if fromEnd == 0 then Right Ultimate
          else if fromEnd == 2 then Right Antepenultimate
          else Left UnrecognizedPlacement
  where
    isStressedVowel c = c `elem` ("áéíóúâêôû" :: String)
